"""Cumulative travel time over a speed raster: a port of the r.walk.accessmod module.

The rules and the order of the floating-point operations follow the C source
(build-env-image/modules/r.walk.accessmod: main.c, accessmod/c/timeCostManager.h,
accessmod/c/bicycleSpeed.h), so that results match the GRASS engine.

A speed cell holds mode * 1e6 + km/h * 1e3 (1 walking, 2 bicycle, 3 motorised); NaN is
no data. Moving between two neighbours costs half the distance at each cell's speed, the
speed adjusted for slope by the cell's mode. Costs are seconds.
"""

import math

import numpy as np
from numba import njit

# neighbours in the module's order: W, E, N, S, NW, NE, SE, SW
_DROW = np.array([0, 0, -1, 1, -1, -1, 1, 1], dtype=np.int64)
_DCOL = np.array([-1, 1, 0, 0, -1, 1, 1, -1], dtype=np.int64)
_KIND = np.array(
    [0, 0, 1, 1, 2, 2, 2, 2], dtype=np.int64
)  # 0 east-west, 1 north-south, 2 diagonal


@njit(cache=True, error_model="numpy")
def _newton(aero, hw, tr, tran, p):
    """bicycleSpeed.h newton(): single precision, as in C."""
    vel = np.float32(20.0)
    tol = np.float32(0.05)
    for _ in range(1, 10):
        tv = np.float32(vel + hw)
        f = np.float32(
            np.float32(vel * np.float32(np.float32(np.float32(aero * tv) * tv) + tr))
            - np.float32(tran * p)
        )
        fp = np.float32(
            np.float64(aero) * (3.0 * np.float64(vel) + np.float64(hw)) * np.float64(tv)
            + np.float64(tr)
        )
        vnew = np.float32(vel - np.float32(f / fp))
        if abs(np.float64(np.float32(vnew - vel))) < np.float64(tol):
            return vnew
        vel = vnew
    return np.float32(0.0)


@njit(cache=True, error_model="numpy")
def _bicycle_speed(speed, slope):
    """bicycleSpeed.h bicycleSpeed(): km/h on a slope for a rider doing `speed` on the flat."""
    speed = np.float32(speed)
    slope = np.float32(slope)
    rolling = np.float32(0.012)
    frontal = np.float32(0.445)
    wind = np.float32(0.0)
    temperature = np.float32(20.0)
    elevation = np.float32(500.0)
    transmission = np.float32(0.90)
    density = np.float32(
        (1.293 - 0.00426 * np.float64(temperature))
        * math.exp(-np.float64(elevation) / 7000.0)
    )
    weight = np.float32(9.8 * (80 + 15))
    air = np.float32(0.5 * np.float64(frontal) * np.float64(density))
    v = np.float32(np.float64(speed) / 3.6)
    slope_out = np.float32(np.float64(slope) * 0.01)
    tire_flat = np.float32(weight * np.float32(np.float32(0.0) + rolling))
    tire_up = np.float32(weight * np.float32(slope_out + rolling))
    total = np.float32(v + wind)
    power = np.float32(
        np.float32(
            np.float32(v * tire_flat)
            + np.float32(np.float32(np.float32(v * total) * total) * air)
        )
        / transmission
    )
    return np.float32(
        np.float64(_newton(air, wind, tire_up, transmission, power)) * 3.6
    )


@njit(cache=True, error_model="numpy")
def _mode_speed(mode, speed, slope):
    """timeCostManager.h modSwitcher(): km/h for a mode on a slope (rise over run)."""
    if mode == 1:
        top = speed / math.exp(-0.175)
        return math.exp(-3.5 * abs(slope + 0.05)) * top
    if mode == 2:
        v = np.float64(_bicycle_speed(speed, slope))
        if v >= speed * 2:  # noqa: PLR1730 - as in C: a NaN speed stays NaN, min() would hide it
            v = speed * 2
        if v < 0:
            v = 0.0
        return v
    if mode == 3:
        return speed
    return 0.0


@njit(cache=True, error_model="numpy")
def _step_cost(here, there, slope, dist, toward):
    """timeCostManager.h costManager() without the knight's move: seconds, NaN when impassable."""
    if toward:
        slope = -slope
    mode = math.floor(here / 1e6)
    speed = (here - mode * 1e6) / 1e3
    mode_adj = math.floor(there / 1e6)
    speed_adj = (there - mode_adj * 1e6) / 1e3
    div = dist / 2000
    cost = 1 / (_mode_speed(mode, speed, slope) / div) + 1 / (
        _mode_speed(mode_adj, speed_adj, slope) / div
    )
    if cost == math.inf:
        return math.nan
    return cost * 3600


@njit(cache=True, error_model="numpy")
def cumulative_cost(
    speed,
    dtm,
    start_rows,
    start_cols,
    ew_res,
    ns_res,
    max_cost,
    toward,
    cost_out,
    visited,
    touched,
):
    """Least cumulative cost in seconds from the start cells, written into `cost_out`.

    `cost_out` (float64, NaN) and `visited` (bool, False) are caller-owned buffers; every cell
    given a cost is appended to `touched` so the caller can reset the buffers without a full pass.
    Returns the number of touched cells. `max_cost` 0 means no limit; with a limit the search stops
    once the cheapest open cell is above it (cells above the limit may keep a cost, as in the module).
    """
    nrows, ncols = speed.shape
    fac = np.empty(3, dtype=np.float64)
    fac[0] = ew_res
    fac[1] = ns_res
    fac[2] = math.sqrt(ns_res * ns_res + ew_res * ew_res)

    cap = 1 << 16
    heap_cost = np.empty(cap, dtype=np.float64)
    heap_cell = np.empty(cap, dtype=np.int64)
    size = 0
    n_touched = 0

    for i in range(start_rows.shape[0]):
        r, c = start_rows[i], start_cols[i]
        if math.isnan(cost_out[r, c]):
            touched[n_touched] = r * ncols + c
            n_touched += 1
        cost_out[r, c] = 0.0
        # push
        if size == cap:
            cap *= 2
            grown_cost = np.empty(cap, dtype=np.float64)
            grown_cell = np.empty(cap, dtype=np.int64)
            grown_cost[:size] = heap_cost[:size]
            grown_cell[:size] = heap_cell[:size]
            heap_cost, heap_cell = grown_cost, grown_cell
        k = size
        size += 1
        while k > 0:
            parent = (k - 1) >> 1
            if heap_cost[parent] <= 0.0:
                break
            heap_cost[k] = heap_cost[parent]
            heap_cell[k] = heap_cell[parent]
            k = parent
        heap_cost[k] = 0.0
        heap_cell[k] = r * ncols + c

    while size > 0:
        here_cost = heap_cost[0]
        cell = heap_cell[0]
        # pop
        size -= 1
        if size > 0:
            last_cost = heap_cost[size]
            last_cell = heap_cell[size]
            k = 0
            while True:
                child = 2 * k + 1
                if child >= size:
                    break
                if child + 1 < size and heap_cost[child + 1] < heap_cost[child]:
                    child += 1
                if heap_cost[child] >= last_cost:
                    break
                heap_cost[k] = heap_cost[child]
                heap_cell[k] = heap_cell[child]
                k = child
            heap_cost[k] = last_cost
            heap_cell[k] = last_cell

        if max_cost > 0 and max_cost < here_cost:
            break
        row = cell // ncols
        col = cell - row * ncols
        if here_cost > cost_out[row, col]:
            continue
        here_dtm = np.float64(dtm[row, col])
        if math.isnan(here_dtm):
            continue
        here_speed = np.float64(speed[row, col])
        if math.isnan(here_speed):
            continue
        if visited[row, col]:
            continue
        visited[row, col] = True

        for n in range(8):
            r = row + _DROW[n]
            c = col + _DCOL[n]
            if r < 0 or r >= nrows or c < 0 or c >= ncols:
                continue
            there_speed = np.float64(speed[r, c])
            if math.isnan(there_speed):
                continue
            dist = fac[_KIND[n]]
            slope = (np.float64(dtm[r, c]) - here_dtm) / dist
            new_cost = here_cost + _step_cost(
                here_speed, there_speed, slope, dist, toward
            )
            if math.isnan(new_cost):
                continue
            old = cost_out[r, c]
            if math.isnan(old):
                touched[n_touched] = r * ncols + c
                n_touched += 1
            elif not old > new_cost:
                continue
            cost_out[r, c] = new_cost
            # push
            if size == cap:
                cap *= 2
                grown_cost = np.empty(cap, dtype=np.float64)
                grown_cell = np.empty(cap, dtype=np.int64)
                grown_cost[:size] = heap_cost[:size]
                grown_cell[:size] = heap_cell[:size]
                heap_cost, heap_cell = grown_cost, grown_cell
            k = size
            size += 1
            while k > 0:
                parent = (k - 1) >> 1
                if heap_cost[parent] <= new_cost:
                    break
                heap_cost[k] = heap_cost[parent]
                heap_cell[k] = heap_cell[parent]
                k = parent
            heap_cost[k] = new_cost
            heap_cell[k] = r * ncols + c
    return n_touched


INT16_MAX = 32767


def to_minutes(seconds, max_minutes=0, timeout_value=None):
    """The engine's clean-up of a cost map (amCleanTravelTime): whole minutes, rounded down.

    With a limit, cells above it are no data. Without one (`max_minutes` 0), cells above 32,767
    minutes take `timeout_value`, or no data when it is None. Returns (minutes as int32, valid mask).
    """
    end = (max_minutes if max_minutes else INT16_MAX) * 60
    with np.errstate(invalid="ignore"):
        valid = (seconds >= 0) & (seconds <= end)
        minutes = np.zeros(seconds.shape, dtype=np.int32)
        q = seconds[valid] / 60.0
        minutes[valid] = np.round(q - np.mod(q, 1)).astype(np.int32)
        if not max_minutes and timeout_value is not None:
            late = ~valid & (seconds / 60 > INT16_MAX)
            minutes[late] = timeout_value
            valid = valid | late
    return minutes, valid
