"""Raster reading and writing for the in-memory engine.

Outputs are written the way the R engine's r.out.gdal calls write them: GeoTIFF, LZW, and for
integer maps the smallest type that holds the values with that type's default no-data value.
"""

import numpy as np
import rasterio


def read_band(path):
    """One band and its grid: (array, nodata, transform, crs)."""
    with rasterio.open(path) as src:
        return src.read(1), src.nodata, src.transform, src.crs


def to_float32(array, nodata):
    """A float32 copy with NaN where the source has no data."""
    out = array.astype(np.float32)
    if nodata is not None and not np.isnan(nodata):
        out[array == nodata] = np.nan
    return out


def same_grid(a_shape, a_transform, b_shape, b_transform):
    return a_shape == b_shape and a_transform.almost_equals(b_transform, 1e-6)


def integer_type(values):
    """The type and no-data value r.out.gdal picks for an integer map with these values."""
    if values.size == 0:
        return "uint8", 255
    low, high = int(values.min()), int(values.max())
    if low >= 0 and high < 255:
        return "uint8", 255
    if low >= 0 and high < 65535:
        return "uint16", 65535
    if low > -32768 and high <= 32767:
        return "int16", -32768
    return "int32", -2147483648


def write_integer(path, values, valid, transform, crs):
    """Write an integer map; cells outside `valid` are no data."""
    dtype, nodata = integer_type(values[valid])
    out = np.full(values.shape, nodata, dtype=dtype)
    out[valid] = values[valid]
    _write(path, out, dtype, nodata, transform, crs)


def write_array(path, array, nodata, transform, crs):
    """Write an array as it is, with the given no-data value."""
    _write(path, array, array.dtype.name, nodata, transform, crs)


def _write(path, array, dtype, nodata, transform, crs):
    profile = {
        "driver": "GTiff",
        "height": array.shape[0],
        "width": array.shape[1],
        "count": 1,
        "dtype": dtype,
        "nodata": nodata,
        "crs": crs,
        "transform": transform,
        "compress": "LZW",
    }
    with rasterio.open(path, "w", **profile) as dst:
        dst.write(array, 1)
