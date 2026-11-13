"""Accessibility analysis: travel time from every cell to the nearest facility.

Same inputs, options and output files as accessibilityAnalysis.R (anisotropic analysis):
    <name>_speed.tif                 the speed raster
    <name>_travel_time_<subset>.tif  travel time in whole minutes
"""

import argparse
import math
import os
import time

import geopandas as gpd
import numpy as np

from . import rasters, speedmap, traveltime


def facility_cells(facilities, subset, transform, shape):
    """Rows and columns of the facilities in the subset, as the module places start points."""
    if subset != "all":
        column = subset
        if "-" in column or column.startswith("_"):
            column = column.replace("-", "_", 1)
            if column.startswith("_"):
                column = "x" + column[1:]
        if column not in facilities.columns:
            raise ValueError("Choose a valid subset column")
        facilities = facilities[facilities[column] == 1]
        if facilities.empty:
            raise ValueError("No facilities selected in the subset column")
    west, north = transform.c, transform.f
    cols = np.floor((facilities.geometry.x.to_numpy() - west) / transform.a).astype(
        np.int64
    )
    rows = np.floor((north - facilities.geometry.y.to_numpy()) / -transform.e).astype(
        np.int64
    )
    inside = (rows >= 0) & (rows < shape[0]) & (cols >= 0) & (cols < shape[1])
    if not inside.all():
        raise ValueError("Specified starting location outside database window")
    return facilities, rows, cols


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
    if analysis_type != "anisotropic":
        raise NotImplementedError(
            "the in-memory engine does the anisotropic analysis only"
        )
    if knights_move:
        raise NotImplementedError(
            "the in-memory engine does not do the knight's move yet"
        )
    timings = {}
    t0 = time.perf_counter()
    landcover, lc_nodata, transform, crs = rasters.read_band(lcv)
    elevation, dem_nodata, dem_transform, _ = rasters.read_band(dem)
    if not rasters.same_grid(
        landcover.shape, transform, elevation.shape, dem_transform
    ):
        raise ValueError("the elevation raster is not on the land cover's grid")
    dtm = rasters.to_float32(elevation, dem_nodata)
    del elevation
    speed = speedmap.speed_codes(
        landcover, lc_nodata, speedmap.read_scenarios(scenarios)
    )
    del landcover
    hf, rows, cols = facility_cells(
        gpd.read_file(facilities), subset, transform, speed.shape
    )
    if np.isnan(speed[rows, cols]).any():
        raise ValueError(
            "Some facilities are on positions in the land cover that are classified as a barrier, please check"
        )
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
    result = run(
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
    print(result)


if __name__ == "__main__":
    main()
