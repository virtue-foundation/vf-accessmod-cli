"""Geographic coverage analysis: facilities take the population they can reach, in ranking order.

Same rules and output files as geoCoverageAnalysis.R (amCapacityAnalysis, amCatchmentAnalyst):
    <name>_pop_resid_<subset>.tif          population left uncovered
    <name>_catchment_<subset>.gpkg         one catchment per facility
    <name>_coverage_analysis_<subset>.csv  the capacity table
    <name>_zonal_stats_<subset>.csv        coverage by admin zone (when zones are given)

Each facility's work is done on the cells it reaches, not on the whole raster. Travel time is
computed towards the facility unless the direction is "from".

A facility's travel time depends on the land cover and the terrain only, never on what earlier
facilities took, so several facilities' travel times are searched at once in threads; the
population is then shared out one facility at a time, in ranking order, as before. The results
do not depend on the number of threads.
"""

import argparse
import collections
import csv
import math
import multiprocessing
import os
import re
import sys
import threading
import time
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
from fractions import Fraction

import geopandas as gpd
import numpy as np
import shapely
from affine import Affine
from rasterio import features
from shapely.geometry import MultiPolygon, shape

from . import facilities as fac
from . import rasters, traveltime
from .accessibility import check_options, read_surface

ID = "xid"


def _extended(q):
    """A positive fraction rounded to x87 extended precision (64-bit mantissa, ties to even), which
    is what R's long double holds on x86-64. Done in whole numbers so that every machine agrees."""
    n, d = q.numerator, q.denominator
    if n == 0:
        return q
    e = n.bit_length() - d.bit_length()
    if (n >> e if e >= 0 else n << -e) < d:
        e -= 1  # now 2^e <= q < 2^(e + 1)
    shift = 63 - e
    if shift >= 0:
        n <<= shift
    else:
        d <<= -shift
    m, rest = divmod(n, d)
    if 2 * rest > d or (2 * rest == d and m & 1):
        m += 1
    return Fraction(m) / (Fraction(2) ** shift)


_NUMBER = re.compile(r"^(-?)(\d*)\.?(\d*)(?:[eE]([-+]?\d+))?$")


def r_parse(text):
    """A decimal number as R reads it (R_strtod): the digits as a whole number, scaled by a power
    of ten in extended precision, then stored as a double. The two roundings make it differ from
    the nearest double by one last bit about once in two thousand numbers."""
    match = _NUMBER.match(text)
    if match is None:  # not a number, infinite
        return float(text)
    sign, whole, decimals, exponent = match.groups()
    digits = int(whole + decimals or "0")
    power = int(exponent or 0) - len(decimals)
    value = Fraction(digits) * Fraction(10) ** power
    return float(_extended(value)) * (-1.0 if sign else 1.0)


def r_text(x):
    """A number after GRASS has printed it (15 significant digits) and R has read it back."""
    return r_parse(f"{x:.15g}")


def r_cumsum(values):
    """R's cumsum: the running total is kept in extended precision, each step stored as a double."""
    total, out = Fraction(0), np.empty(len(values), dtype=np.float64)
    for i, v in enumerate(values):
        total = _extended(total + Fraction(float(v)))
        out[i] = float(total)
    return out


def r_digits(x):
    """How many significant digits R writes for a number (at most 15). R scales the number to a
    15-digit integer in extended precision, rounds it and drops the trailing zeros (formatReal's
    scientific()); the digits it then prints are the correctly rounded ones for that count. Near a
    tie in the 15th digit this is not the same text as a plain 15-digit rounding."""
    r = abs(float(x))
    if r == 0 or not math.isfinite(r):
        return 1
    kp = math.floor(math.log10(r)) - 14
    # the table of powers of ten in R's format.c holds double constants
    ten = Fraction(float(f"1e{abs(kp)}")) if abs(kp) <= 27 else Fraction(10) ** abs(kp)
    scaled = _extended(Fraction(r) / ten if kp > 0 else Fraction(r) * ten)
    if scaled < 10**14:
        scaled = _extended(scaled * 10)
    alpha, rest = divmod(scaled.numerator, scaled.denominator)
    if 2 * rest > scaled.denominator or (2 * rest == scaled.denominator and alpha & 1):
        alpha += 1
    digits = 15
    while digits > 1 and alpha % 10 == 0:
        alpha //= 10
        digits -= 1
    return digits


def r_number(x):
    """A number after R has written it into text and GRASS has read it back. (R 4.1's rule, which
    wrote the stored results; R 4.3 and later round plainly here, about one number in 200,000
    differs.)"""
    return float(f"{x:.{r_digits(x)}g}")


def processing_order(table, order_col):
    """Facility ids by decreasing ranking value; equal values keep the table's order."""
    values = table[order_col].to_numpy(dtype=np.float64)
    return table.iloc[np.argsort(-values, kind="stable")][ID].tolist()


def allocate(zones, band, cum, capacity, ignore_capacity, max_time):
    """The capacity case for one facility (amCatchmentAnalyst): which minute bands are emptied,
    which one is reduced and by what ratio, and where the catchment stops."""
    out = {"type": None, "inner": None, "outer": None, "ratio": None, "limit": math.nan}
    pop_max = cum[-1]
    positive = cum[cum > 0]
    pop_min = positive[0] if positive.size else None
    if ignore_capacity:
        capacity = pop_max
    residual = capacity
    within = np.flatnonzero(cum <= capacity)
    if within.size:
        pop_inner, zone_inner = cum[within[-1]], int(zones[within[-1]])
    else:
        pop_inner, zone_inner = 0.0, int(zones[0])
    beyond = np.flatnonzero(cum > capacity)
    pop_outer = cum[beyond[0]] if beyond.size else None
    band_outer = band[beyond[0]] if beyond.size else None
    zone_outer = int(zones[beyond[0]]) if beyond.size else None

    if ignore_capacity:
        out.update(type="E", limit=max_time, inner=zone_inner)
        residual = 0.0
    elif capacity == pop_inner:
        out.update(type="D", limit=zone_inner, inner=zone_inner)
        residual = 0.0
    elif capacity > pop_max:
        out.update(type="C", limit=zone_inner, inner=zone_inner)
        residual = capacity - pop_max
    elif pop_min is not None and capacity < pop_min:
        out.update(
            type="B",
            limit=zone_outer,
            outer=zone_outer,
            ratio=(capacity - pop_inner) / band_outer,
        )
        residual = 0.0
    elif pop_inner > 0 and pop_outer is not None and pop_outer > 0:
        out.update(
            type="A",
            limit=zone_outer,
            inner=zone_inner,
            outer=zone_outer,
            ratio=(capacity - pop_inner) / band_outer,
        )
        residual = 0.0
    out["capacity"] = capacity
    out["capacity_residual"] = residual
    out["capacity_realised"] = capacity - residual
    out["pop_max"] = pop_max
    return out


def correlation(x, y):
    """Pearson correlation, NaN when it is not defined (R's cor)."""
    if x.size < 2:
        return math.nan
    with np.errstate(invalid="ignore", divide="ignore"):
        dx, dy = x - x.mean(), y - y.mean()
        return float((dx * dy).sum() / math.sqrt((dx * dx).sum() * (dy * dy).sum()))


def write_r_csv(path, columns, rows, row_names):
    """A table the way R's write.csv lays it out: quoted text, bare numbers, NA for missing."""

    def cell(v):
        if v is None or (isinstance(v, float) and math.isnan(v)):
            return "NA"
        if isinstance(v, str):
            return '"' + v.replace('"', '""') + '"'
        if isinstance(v, (int, np.integer)):
            return str(int(v))
        return "0" if v == 0 else f"{float(v):.{r_digits(v)}g}"

    with open(path, "w", newline="") as f:
        f.write(",".join(['""'] + ['"' + c + '"' for c in columns]) + "\n")
        f.writelines(
            ",".join(['"' + str(name) + '"'] + [cell(v) for v in row]) + "\n"
            for name, row in zip(row_names, rows)
        )


# the fastest a mode can go for a class's speed: Tobler's function peaks at speed / exp(-0.175)
# on a slight descent, a bicycle is capped at twice its speed, a motor keeps its speed
_FASTEST = {"WALKING": 1 / math.exp(-0.175), "BICYCLING": 2.0, "MOTORIZED": 1.0}
_BYTES_PER_CELL = 17  # a search's own buffers: cost, visited, touched


def reach_radius(scenarios, max_time, cell):
    """How many cells away a traveller can be after `max_time` minutes at the fastest speed any
    class allows, with a margin. No cell farther than this can be within the time limit."""
    fastest = max(
        (speed * _FASTEST[mode] for _cls, _label, speed, mode in scenarios if speed),
        default=0.0,
    )
    return math.ceil(fastest * 1000 / 60 * max_time / cell) + 2


def thread_count(asked, window_cells):
    """Threads for the travel-time searches: the number asked for (ACCESSMOD_THREADS, else one
    per processor), fewer when their buffers would take more than 40% of the machine's memory."""
    n = asked or int(os.environ.get("ACCESSMOD_THREADS") or 0) or os.cpu_count() or 1
    try:
        memory = os.sysconf("SC_PAGE_SIZE") * os.sysconf("SC_PHYS_PAGES")
    except (AttributeError, OSError, ValueError):
        return n
    return max(1, min(n, int(memory * 0.4 / (window_cells * _BYTES_PER_CELL))))


class Reach:
    """Travel time from one facility's points, searched on the window a traveller can cover
    within the time limit. Each thread keeps its own buffers."""

    def __init__(self, speed, dtm, transform, scenarios, max_time, towards):
        self.speed, self.dtm, self.towards = speed, dtm, towards
        self.ew, self.ns = transform.a, -transform.e
        self.max_cost = int(max_time * 60)
        self.radius = reach_radius(scenarios, max_time, min(self.ew, self.ns))
        side = 2 * self.radius + 1
        self.window_cells = min(speed.shape[0], side) * min(speed.shape[1], side)
        self.local = threading.local()
        self.lock = threading.Lock()
        self.seconds = 0.0

    def __call__(self, rows, cols):
        """(first row, first column, minutes) for the box of cells reached in time, -1 where a
        cell is not reached; None when no cell is."""
        t0 = time.perf_counter()
        nrows, ncols = self.speed.shape
        r0, r1 = (
            max(int(rows.min()) - self.radius, 0),
            min(int(rows.max()) + self.radius + 1, nrows),
        )
        c0, c1 = (
            max(int(cols.min()) - self.radius, 0),
            min(int(cols.max()) + self.radius + 1, ncols),
        )
        h, w = r1 - r0, c1 - c0
        buffers = getattr(self.local, "buffers", None)
        if buffers is None or buffers[0].size < h * w:
            size = max(h * w, self.window_cells)
            buffers = (
                np.full(size, math.nan, dtype=np.float64),
                np.zeros(size, dtype=np.bool_),
                np.empty(size, dtype=np.int64),
            )
            self.local.buffers = buffers
        cost, visited, touched = buffers
        n = traveltime.cumulative_cost(
            self.speed[r0:r1, c0:c1],
            self.dtm[r0:r1, c0:c1],
            rows - r0,
            cols - c0,
            self.ew,
            self.ns,
            self.max_cost,
            self.towards,
            cost[: h * w].reshape(h, w),
            visited[: h * w].reshape(h, w),
            touched,
        )
        a0, b0, minutes = traveltime.reached_minutes(
            cost, visited, touched, n, w, self.max_cost
        )
        found = (r0 + a0, c0 + b0, minutes) if minutes.size else None
        with self.lock:
            self.seconds += time.perf_counter() - t0
        return found


def outline(mask, transform, r0, c0):
    """A catchment: the cells of `mask` as polygons (r.to.vect's four-connected areas)."""
    return MultiPolygon(
        [
            shape(geom)
            for geom, _ in features.shapes(
                mask.astype(np.uint8),
                mask=mask,
                connectivity=4,
                transform=transform * Affine.translation(c0, r0),
            )
        ]
    )


def outline_packed(packed, shape_, transform, r0, c0):
    """The same from a bit-packed mask, as well-known binary: the form that crosses to another
    process. Drawing polygons is Python work that would hold up every thread of this one."""
    mask = np.unpackbits(
        np.frombuffer(packed, dtype=np.uint8), count=shape_[0] * shape_[1]
    ).reshape(shape_)
    return outline(mask.astype(np.bool_), transform, r0, c0).wkb


def zonal_table(admin, zone_col, pop, resid, transform):
    """Population and population left by admin zone (v.to.rast on `cat`, then r.univar by zone).

    Zones are numbered in file order. Where polygons overlap, GRASS gives the cell to the lowest
    number, so the polygons are burned from the last to the first.
    """
    zones = gpd.read_file(admin)
    cats = np.arange(1, len(zones) + 1)
    grid = features.rasterize(
        (
            (geom, int(cat))
            for geom, cat in zip(zones.geometry[::-1], cats[::-1])
            if geom is not None and not geom.is_empty
        ),
        out_shape=pop.shape,
        transform=transform,
        fill=0,
        dtype="int32",
    )
    inside = grid > 0
    present = np.flatnonzero(np.bincount(grid[inside], minlength=len(zones) + 1))

    def sums(values):
        ok = inside & ~np.isnan(values)
        total = np.bincount(grid[ok], weights=values[ok], minlength=len(zones) + 1)
        count = np.bincount(grid[ok], minlength=len(zones) + 1)
        total[count == 0] = np.nan
        return total

    all_pop, left = sums(pop), sums(resid)
    # the R engine replaces quotes and line breaks by spaces before it reads GRASS's table
    labels = [
        str(v).replace('"', " ").replace("'", " ").replace("\n", " ")
        for v in zones[zone_col]
    ]
    rows = []
    for cat in present:
        label = labels[cat - 1]
        # R computes from the sums as r.univar prints them
        zone_left, zone_pop = r_text(left[cat]), r_text(all_pop[cat])
        covered = zone_pop - zone_left
        rows.append(
            (
                f"{cat}\r{label}",
                [
                    int(cat),
                    label,
                    zone_left,
                    zone_pop,
                    covered,
                    covered / zone_pop * 100,
                ],
            )
        )
    rows.sort(key=lambda r: r[0])
    return [
        "cat",
        zone_col,
        "amPopResidualSum",
        "amPopSum",
        "amPopCovered",
        "amPopCoveredPercent",
    ], [r[1] for r in rows]


def run(
    lcv,
    dem,
    pop,
    scenarios,
    facilities,
    name,
    output_dir,
    order_col,
    name_col,
    max_time,
    subset="all",
    capacity_col=None,
    admin=None,
    zonal_col=None,
    knights_move=False,
    analysis_type="anisotropic",
    towards_facilities=True,
    threads=None,
):
    check_options(analysis_type, knights_move)
    timings = {"allocation": 0.0}
    t0 = time.perf_counter()
    landcover, lc_nodata, dtm, speed, scenario_rows, transform, crs = read_surface(
        lcv, dem, scenarios
    )
    population, pop_nodata, pop_transform, _ = rasters.read_band(pop)
    if not rasters.same_grid(speed.shape, transform, population.shape, pop_transform):
        raise ValueError("the population raster is not on the land cover's grid")
    pop_is_single = population.dtype == np.float32
    pop_values = population.astype(np.float64)
    if pop_nodata is not None and not np.isnan(pop_nodata):
        pop_values[population == pop_nodata] = np.nan
    del population
    if (
        lc_nodata is not None
        and (~np.isnan(pop_values) & (landcover == lc_nodata)).any()
    ):
        raise ValueError(
            "Some of the population seems to be located on barrier cells, please adjust your pop raster"
        )

    hf = fac.validated(
        fac.load(facilities, subset, transform, speed.shape),
        landcover,
        lc_nodata,
        dtm,
        scenario_rows,
    )
    del landcover
    if ID not in hf.columns:
        raise ValueError("the facilities need an '_id' column")
    ignore_capacity = capacity_col is None
    order = processing_order(hf, order_col)

    # population left: the population where it is positive and travel is possible, 0 where it is not positive
    with np.errstate(invalid="ignore"):
        resid = np.where(
            pop_values > 0,
            np.where(np.isnan(speed), np.nan, pop_values),
            np.where(np.isnan(pop_values), np.nan, 0.0),
        )
    resid_is_double = not pop_is_single
    total_left = float(np.nansum(resid))
    timings["read"] = time.perf_counter() - t0

    os.makedirs(output_dir, exist_ok=True)
    warnings_path = os.path.join(output_dir, "na_capacity_warnings.csv")
    reach = Reach(speed, dtm, transform, scenario_rows, max_time, towards_facilities)
    n_threads = thread_count(threads, reach.window_cells)
    rows_of = dict(tuple(hf.groupby(ID, sort=False)))
    points = {
        xid: (rows["row"].to_numpy(), rows["col"].to_numpy())
        for xid, rows in rows_of.items()
    }

    t_loop = time.perf_counter()
    table_rows, catchments = [], []
    # one process for four threads draws the catchments; with few threads the threads draw them.
    # The processes are forked here, before any thread exists and whatever script called the
    # engine (a spawned process would import that script again), so on Linux only.
    n_drawers = n_threads // 4 if sys.platform == "linux" else 0
    drawers = None
    if n_drawers:
        drawers = ProcessPoolExecutor(
            n_drawers, mp_context=multiprocessing.get_context("fork")
        )
        for started in [drawers.submit(os.getpid) for _ in range(n_drawers)]:
            started.result()
    drawing = collections.deque()
    try:
        with ThreadPoolExecutor(n_threads) as pool:
            waiting = iter(order)
            ahead = collections.deque()
            for step, xid in enumerate(order, start=1):
                # travel times are searched ahead of the facility being shared out, a few at a time
                while len(ahead) < n_threads + 1:
                    nxt = next(waiting, None)
                    if nxt is None:
                        break
                    ahead.append(pool.submit(reach, *points[nxt]))
                mine = rows_of[xid]
                facility_name = (
                    mine[name_col].iloc[0]
                    if len(mine) == 1
                    else mine[name_col].tolist()[0]
                )
                capacity = 0.0
                if not ignore_capacity:
                    values = mine[capacity_col].to_numpy(dtype=np.float64)
                    if np.isnan(values).any():
                        new_file = not os.path.exists(warnings_path)
                        with open(warnings_path, "a", newline="") as f:
                            w = csv.writer(f, quoting=csv.QUOTE_NONNUMERIC)
                            if new_file:
                                w.writerow(["xid", "name"])
                            w.writerow([xid, facility_name])
                    capacity = float(sum(r_text(v) for v in values[~np.isnan(values)]))

                found = ahead.popleft().result()
                t1 = time.perf_counter()
                before = total_left
                row = {
                    ID: xid,
                    name_col: facility_name,
                    "amOrderComputed": step,
                    "amTravelTimeMax": max_time,
                }
                result = None
                if found is not None:
                    r0, c0, tt = found
                    window = resid[r0 : r0 + tt.shape[0], c0 : c0 + tt.shape[1]]
                    band_all, cells = traveltime.band_sums(
                        tt, window, int(max_time) + 1
                    )
                    zones = np.flatnonzero(cells)
                    if zones.size:
                        # r.univar prints each band's sum with 15 significant digits and R works from that text
                        band = np.array([r_text(v) for v in band_all[zones]])
                        cum = r_cumsum(band)
                        result = allocate(
                            zones, band, cum, capacity, ignore_capacity, max_time
                        )
                        removed = traveltime.take(
                            tt,
                            window,
                            -1 if result["inner"] is None else result["inner"],
                            -1 if result["outer"] is None else result["outer"],
                            0.0
                            if result["outer"] is None
                            else r_number(result["ratio"]),
                        )
                        if result["outer"] is not None:
                            resid_is_double = True
                        total_left = before - removed
                        limit = result["limit"]
                        row.update(
                            amPopTravelTimeMax=result["pop_max"],
                            amCorrPopTime=correlation(zones.astype(np.float64), band),
                            amTravelTimeCatchment=limit,
                            amCapacityRealised=result["capacity_realised"],
                            amCapacityResidual=result["capacity_residual"],
                        )
                        if not ignore_capacity:
                            row[capacity_col] = capacity
                        if result["type"] is not None:
                            in_catchment = zones <= limit
                            pop_catchment = (
                                float(cum[in_catchment].max())
                                if in_catchment.any()
                                else math.nan
                            )
                            row["amPopCatchmentTotal"] = pop_catchment
                            row["amPopCatchmentDiff"] = round(
                                pop_catchment - result["capacity_realised"], 6
                            )
                            # the outline no longer depends on the facilities that follow: drawn aside
                            mask = (tt >= 0) & (tt <= limit)
                            if drawers is None:
                                drawn = pool.submit(outline, mask, transform, r0, c0)
                            else:
                                drawn = drawers.submit(
                                    outline_packed,
                                    np.packbits(mask).tobytes(),
                                    mask.shape,
                                    transform,
                                    r0,
                                    c0,
                                )
                                # masks wait in memory until they are drawn: no more than a few at a time
                                drawing.append(drawn)
                                while len(drawing) > 4 * n_drawers:
                                    drawing.popleft().result()
                            catchments.append(
                                {
                                    "length": math.nan,
                                    ID: xid,
                                    name_col: facility_name,
                                    "type": result["type"],
                                    "geometry": drawn,
                                }
                            )
                if result is None and not ignore_capacity:
                    row[capacity_col] = capacity
                row["amPopCoveredPercent"] = (
                    (before - total_left) / before * 100 if before else math.nan
                )
                table_rows.append(row)
                timings["allocation"] += time.perf_counter() - t1
            drawn = [c["geometry"].result() for c in catchments]
    finally:
        if drawers is not None:
            drawers.shutdown(cancel_futures=True)
    if drawers is not None:
        drawn = shapely.from_wkb(drawn)
    for c, geometry in zip(catchments, drawn):
        c["geometry"] = geometry
    timings["travel time and catchments"] = (
        time.perf_counter() - t_loop - timings["allocation"]
    )
    timings["travel time, all threads"] = reach.seconds
    timings["threads"], timings["drawing processes"] = n_threads, n_drawers

    t0 = time.perf_counter()
    out = resid if resid_is_double else resid.astype(np.float32)
    rasters.write_array(
        os.path.join(output_dir, f"{name}_pop_resid_{subset}.tif"),
        out,
        math.nan,
        transform,
        crs,
    )
    if catchments:
        layer = f"{name}_catchment_{subset}"
        gpd.GeoDataFrame(catchments, geometry="geometry", crs=crs.to_wkt()).to_file(
            os.path.join(output_dir, layer + ".gpkg"), layer=layer, driver="GPKG"
        )

    order_name = f"amOrderValues_{order_col}"
    ranking = {x: v for x, v in zip(hf[ID], hf[order_col])}
    columns = (
        [ID, name_col]
        + ([] if ignore_capacity else [capacity_col])
        + [
            order_name,
            "amOrderComputed",
            "amTravelTimeMax",
            "amPopTravelTimeMax",
            "amCorrPopTime",
            "amTravelTimeCatchment",
            "amPopCatchmentTotal",
            "amCapacityRealised",
            "amCapacityResidual",
            "amPopCatchmentDiff",
            "amPopCoveredPercent",
        ]
    )
    rank_of = {x: i + 1 for i, x in enumerate(sorted(set(order)))}
    rows = [
        [{**r, order_name: ranking[r[ID]]}.get(c) for c in columns] for r in table_rows
    ]
    write_r_csv(
        os.path.join(output_dir, f"{name}_coverage_analysis_{subset}.csv"),
        columns,
        rows,
        [rank_of[r[ID]] for r in table_rows],
    )

    if admin is not None:
        zcols, zrows = zonal_table(admin, zonal_col, pop_values, resid, transform)
        write_r_csv(
            os.path.join(output_dir, f"{name}_zonal_stats_{subset}.csv"),
            zcols,
            zrows,
            range(1, len(zrows) + 1),
        )
    timings["write"] = time.perf_counter() - t0
    return {
        "facilities": len(order),
        "catchments": len(catchments),
        "population_left": total_left,
        "timings": timings,
    }


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("--lcv", required=True)
    p.add_argument("--dem", required=True)
    p.add_argument("--pop", required=True)
    p.add_argument("--scenarios", required=True)
    p.add_argument("--facilities", required=True)
    p.add_argument("--admin")
    p.add_argument("--name", required=True)
    p.add_argument("--analysis_type", required=True)
    p.add_argument("--f_order", required=True)
    p.add_argument("--f_name", required=True)
    p.add_argument("--max_time", type=float, required=True)
    p.add_argument("--knights_move", action="store_true")
    p.add_argument("--output_dir", required=True)
    p.add_argument("--f_subset", default="all")
    p.add_argument("--f_capacity")
    p.add_argument("--zonal_column")
    p.add_argument("--direction", choices=["to", "from"], default="to")
    p.add_argument("--threads", type=int, default=None)
    p.add_argument("--debug_print", action="store_true")
    a = p.parse_args(argv)
    if (a.admin is None) != (a.zonal_column is None):
        a.admin = None
    print(
        run(
            a.lcv,
            a.dem,
            a.pop,
            a.scenarios,
            a.facilities,
            a.name,
            a.output_dir,
            a.f_order,
            a.f_name,
            a.max_time,
            a.f_subset,
            a.f_capacity,
            a.admin,
            a.zonal_column,
            a.knights_move,
            a.analysis_type,
            a.direction == "to",
            a.threads,
        )
    )


if __name__ == "__main__":
    main()
