"""The speed raster: each land cover class takes the mode and speed its scenario row gives it.

Same encoding as the R engine's amCreateSpeedMap: (mode + km/h) * 1000 with walking 1000,
bicycling 2000, motorised 3000. A class with speed 0 or with no row is no data (impassable).
"""

import csv

import numpy as np

MODES = {"WALKING": 1000, "BICYCLING": 2000, "MOTORIZED": 3000}
SPEED_NODATA = -2147483648


def read_scenarios(path):
    """Rows of the scenario table as (class, label, speed, mode); the columns are read by position."""
    rows = []
    with open(path, newline="") as f:
        reader = csv.reader(f)
        next(reader)
        for line in reader:
            if len(line) < 4 or not line[0].strip():
                continue
            rows.append((int(float(line[0])), line[1], float(line[2]), line[3].strip()))
    return rows


def speed_codes(landcover, nodata, scenarios):
    """Speed codes (float32, NaN for no data) for a land cover raster of integer classes."""
    lut = np.full(
        int(max(landcover.max(), max(r[0] for r in scenarios))) + 1,
        np.nan,
        dtype=np.float32,
    )
    for cls, _label, speed, mode in scenarios:
        if speed != 0:
            lut[cls] = (MODES[mode] + speed) * 1000
    out = lut[landcover]
    if nodata is not None:
        out[landcover == nodata] = np.nan
    return out


def codes_to_int(codes):
    """The speed raster as the engine exports it: int32 with the GRASS no-data value."""
    out = np.full(codes.shape, SPEED_NODATA, dtype=np.int32)
    ok = ~np.isnan(codes)
    out[ok] = codes[ok].astype(np.int32)
    return out
