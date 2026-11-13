"""Accessibility analysis: travel time from every cell to the nearest facility.

Same inputs, options and output files as accessibilityAnalysis.R (anisotropic analysis):
    <name>_speed.tif                 the speed raster
    <name>_travel_time_<subset>.tif  travel time in whole minutes
"""

import argparse
import math
import os
import time

import numpy as np

from . import facilities as fac
from . import rasters, speedmap, traveltime


def read_surface(lcv, dem, scenarios):
    """Land cover, elevation and the speed raster on one grid."""
    landcover, lc_nodata, transform, crs = rasters.read_band(lcv)
    elevation, dem_nodata, dem_transform, _ = rasters.read_band(dem)
    if not rasters.same_grid(
        landcover.shape, transform, elevation.shape, dem_transform
    ):
        raise ValueError("the elevation raster is not on the land cover's grid")
    dtm = rasters.to_float32(elevation, dem_nodata)
    table = speedmap.read_scenarios(scenarios)
    speed = speedmap.speed_codes(landcover, lc_nodata, table)
    return landcover, lc_nodata, dtm, speed, table, transform, crs


def check_options(analysis_type, knights_move):
    if analysis_type != "anisotropic":
        raise NotImplementedError(
            "the in-memory engine does the anisotropic analysis only"
        )
    if knights_move:
        raise NotImplementedError(
            "the in-memory engine does not do the knight's move yet"
        )


def run(
    lcv,
    dem,
    scenarios,
    facilities,
    name,
    output_dir,
    subset="all",
    max_time=0,
    knights_move=False,
    analysis_type="anisotropic",
):
    check_options(analysis_type, knights_move)
    timings = {}
    t0 = time.perf_counter()
    landcover, lc_nodata, dtm, speed, table, transform, crs = read_surface(
        lcv, dem, scenarios
    )
    hf = fac.load(facilities, subset, transform, speed.shape)
    fac.validated(hf, landcover, lc_nodata, dtm, table)
    del landcover
    timings["read"] = time.perf_counter() - t0

    os.makedirs(output_dir, exist_ok=True)
    rasters.write_array(
        os.path.join(output_dir, f"{name}_speed.tif"),
        speedmap.codes_to_int(speed),
        speedmap.SPEED_NODATA,
        transform,
        crs,
    )

    t0 = time.perf_counter()
    cost = np.full(speed.shape, math.nan, dtype=np.float64)
    visited = np.zeros(speed.shape, dtype=np.bool_)
    touched = np.empty(speed.size, dtype=np.int64)
    rows, cols = hf["row"].to_numpy(), hf["col"].to_numpy()
    traveltime.cumulative_cost(
        speed,
        dtm,
        rows,
        cols,
        transform.a,
        -transform.e,
        int(max_time * 60),
        True,
        cost,
        visited,
        touched,
    )
    timings["travel time"] = time.perf_counter() - t0

    t0 = time.perf_counter()
    minutes, valid = traveltime.to_minutes(
        cost, max_minutes=max_time, timeout_value=None if max_time else -1
    )
    rasters.write_integer(
        os.path.join(output_dir, f"{name}_travel_time_{subset}.tif"),
        minutes,
        valid,
        transform,
        crs,
    )
    timings["write"] = time.perf_counter() - t0
    return {"facilities": len(hf), "cells": int(valid.sum()), "timings": timings}


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("--lcv", required=True)
    p.add_argument("--dem", required=True)
    p.add_argument("--scenarios", required=True)
    p.add_argument("--facilities", required=True)
    p.add_argument("--name", required=True)
    p.add_argument("--analysis_type", default="anisotropic")
    p.add_argument("--max_time", type=float, default=0)
    p.add_argument("--f_subset", default="all")
    p.add_argument("--knights_move", action="store_true")
    p.add_argument("--output_dir", required=True)
    p.add_argument("--debug_print", action="store_true")
    a = p.parse_args(argv)
    print(
        run(
            a.lcv,
            a.dem,
            a.scenarios,
            a.facilities,
            a.name,
            a.output_dir,
            a.f_subset,
            a.max_time,
            a.knights_move,
            a.analysis_type,
        )
    )


if __name__ == "__main__":
    main()
