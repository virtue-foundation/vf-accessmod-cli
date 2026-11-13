"""The facilities table as the R engine sees it after GRASS has imported the GeoPackage.

GRASS numbers the features 1..n in file order (`cat`) and renames a leading underscore in a
column name to `x`, so the facility id column `_id` is `xid`. The R engine then keeps the
subset's rows in the order of `cat` read as text ("1", "10", "11", "2", ...), which decides the
order of facilities with an equal ranking value.
"""

import geopandas as gpd
import numpy as np


def grass_name(column):
    """A column name after GRASS's import."""
    return "x" + column[1:] if column.startswith("_") else column


def subset_column(name):
    """The subset column as amFacilitiesSubset rewrites it."""
    if "-" in name or name.startswith("_"):
        name = name.replace("-", "_", 1)
        if name.startswith("_"):
            name = "x" + name[1:]
    return name


def load(path, subset, transform, shape):
    """The subset's facilities with `cat`, `row` and `col`, in file order."""
    table = gpd.read_file(path)
    table = table.rename(columns={c: grass_name(c) for c in table.columns})
    table["cat"] = np.arange(1, len(table) + 1)
    if subset != "all":
        column = subset_column(subset)
        if column not in table.columns:
            raise ValueError("Choose a valid subset column")
        table = table[table[column] == 1]
        if table.empty:
            raise ValueError("No facilities selected in the subset column")
    table = table.copy()
    table["col"] = np.floor(
        (table.geometry.x.to_numpy() - transform.c) / transform.a
    ).astype(np.int64)
    table["row"] = np.floor(
        (transform.f - table.geometry.y.to_numpy()) / -transform.e
    ).astype(np.int64)
    inside = (
        (table["row"] >= 0)
        & (table["row"] < shape[0])
        & (table["col"] >= 0)
        & (table["col"] < shape[1])
    )
    if not inside.all():
        raise ValueError("Specified starting location outside database window")
    return table


def validated(table, landcover, lc_nodata, dtm, scenarios):
    """The rows the analysis keeps (amValidateFacilitiesTable): on a land cover class and on the
    elevation raster, sorted by `cat` as text. A facility on a class with speed 0 is an error."""
    rows, cols = table["row"].to_numpy(), table["col"].to_numpy()
    classes = landcover[rows, cols]
    on_landcover = (
        np.ones(len(table), dtype=bool) if lc_nodata is None else classes != lc_nodata
    )
    on_dem = ~np.isnan(dtm[rows, cols])
    zero = {cls for cls, _label, speed, _mode in scenarios if speed == 0}
    if any(int(c) in zero for c in classes[on_landcover & on_dem]):
        raise ValueError(
            "Some facilities are on zero-speed positions on the land cover, please check"
        )
    kept = table[on_landcover & on_dem]
    return kept.iloc[np.argsort(kept["cat"].astype(str).to_numpy(), kind="stable")]
