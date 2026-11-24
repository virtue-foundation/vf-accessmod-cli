"""The in-memory engine on small made-up grids. Skipped where its libraries are not installed."""

import math

import pytest

np = pytest.importorskip("numpy")
pytest.importorskip("numba")
rasterio = pytest.importorskip("rasterio")
gpd = pytest.importorskip("geopandas")
from_origin = pytest.importorskip("rasterio.transform").from_origin
geometry = pytest.importorskip("shapely.geometry")
accessibility = pytest.importorskip("native.accessibility")
coverage = pytest.importorskip("native.coverage")
speedmap = pytest.importorskip("native.speedmap")
traveltime = pytest.importorskip("native.traveltime")

CELL = 100.0
WALK_5 = (1000 + 5) * 1000.0
DRIVE_50 = (3000 + 50) * 1000.0


def _cost(speed, dtm, row, col, toward, max_cost=0):
    cost = np.full(speed.shape, math.nan)
    traveltime.cumulative_cost(
        speed.astype(np.float32),
        dtm.astype(np.float32),
        np.array([row]),
        np.array([col]),
        CELL,
        CELL,
        max_cost,
        toward,
        cost,
        np.zeros(speed.shape, dtype=np.bool_),
        np.empty(speed.size, dtype=np.int64),
    )
    return cost


def test_flat_walk_takes_distance_over_speed():
    cost = _cost(np.full((5, 9), WALK_5), np.zeros((5, 9)), 2, 4, False)
    # 100 m at 5 km/h is 72 s; a diagonal step is sqrt(2) longer
    assert cost[2, 4] == 0
    assert cost[2, 7] == pytest.approx(3 * 72)
    assert cost[0, 2] == pytest.approx(2 * 72 * math.sqrt(2))


def test_no_data_is_a_barrier():
    speed = np.full((3, 7), WALK_5)
    speed[:, 3] = math.nan
    cost = _cost(speed, np.zeros((3, 7)), 1, 0, False)
    assert np.isnan(cost[:, 3:]).all()
    assert not np.isnan(cost[:, :3]).any()


def test_direction_turns_the_slope_round():
    dtm = np.tile(np.arange(9) * 10.0, (5, 1))  # rises to the east, 10 m a cell
    speed = np.full((5, 9), WALK_5)
    away = _cost(speed, dtm, 2, 4, False)
    towards = _cost(speed, dtm, 2, 4, True)
    # leaving for the east is the climb a walker from the west makes to arrive
    assert away[2, 6] == pytest.approx(towards[2, 2])
    assert away[2, 2] == pytest.approx(towards[2, 6])
    assert away[2, 6] > away[2, 2]


def test_motor_travel_ignores_the_slope():
    dtm = np.tile(np.arange(9) * 10.0, (5, 1))
    speed = np.full((5, 9), DRIVE_50)
    away = _cost(speed, dtm, 2, 4, False)
    assert np.array_equal(away, _cost(speed, dtm, 2, 4, True))
    assert away[2, 6] == pytest.approx(away[2, 2])


def test_search_stops_at_the_limit():
    cost = _cost(np.full((1, 30), WALK_5), np.zeros((1, 30)), 0, 0, False, 720)
    minutes, valid = traveltime.to_minutes(cost, max_minutes=12)
    assert valid[0, :11].all() and not valid[0, 11:].any()
    assert minutes[0, 10] == 12


def test_minutes_round_down_and_time_out():
    seconds = np.array([0.0, 59.9, 60.0, 33000 * 60.0, math.nan])
    minutes, valid = traveltime.to_minutes(seconds, timeout_value=-1)
    assert minutes.tolist() == [0, 0, 1, -1, 0]
    assert valid.tolist() == [True, True, True, True, False]


def test_speed_codes():
    rows = [
        (1, "Water", 0.0, "WALKING"),
        (2, "Grass", 4.0, "WALKING"),
        (5, "Road", 60.0, "MOTORIZED"),
    ]
    codes = speedmap.speed_codes(np.array([[1, 2, 5, 3, 9]]), 9, rows)
    assert codes[0, 1] == 1004000 and codes[0, 2] == 3060000
    assert np.isnan(codes[0, [0, 3, 4]]).all()
    assert speedmap.codes_to_int(codes)[0, 0] == speedmap.SPEED_NODATA


@pytest.fixture
def region(tmp_path):
    """A flat 40 x 40 grid of grass with a road along row 20, ten people a cell, two facilities."""
    transform = from_origin(500000, 9004000, CELL, CELL)
    profile = {
        "driver": "GTiff",
        "height": 40,
        "width": 40,
        "count": 1,
        "crs": "EPSG:32736",
        "transform": transform,
    }
    landcover = np.full((40, 40), 2, dtype=np.uint8)
    landcover[20, :] = 5
    landcover[:, 39] = 255
    population = np.full((40, 40), 10.0, dtype=np.float32)
    population[:, 39] = np.nan
    paths = {
        k: str(tmp_path / f"T_{k}")
        for k in (
            "lcv.tif",
            "dem.tif",
            "pop.tif",
            "scenarios.csv",
            "hf.gpkg",
            "admin.gpkg",
        )
    }
    with rasterio.open(
        paths["lcv.tif"], "w", dtype="uint8", nodata=255, **profile
    ) as f:
        f.write(landcover, 1)
    with rasterio.open(
        paths["dem.tif"], "w", dtype="int16", nodata=-32768, **profile
    ) as f:
        f.write(np.zeros((40, 40), dtype=np.int16), 1)
    with rasterio.open(
        paths["pop.tif"], "w", dtype="float32", nodata=math.nan, **profile
    ) as f:
        f.write(population, 1)
    with open(paths["scenarios.csv"], "w") as f:
        f.write("class,label,speed,mode\n2,Grass,4,WALKING\n5,Road,60,MOTORIZED\n")
    gpd.GeoDataFrame(
        {
            "_id": ["a", "b"],
            "name": ["First", "Second"],
            "vfmRelevanceScore": [2.0, 1.0],
            "beds": [500.0, 100000.0],
            "w_x": [1, 1],
        },
        geometry=[geometry.Point(500550, 9001950), geometry.Point(503050, 9001950)],
        crs="EPSG:32736",
    ).to_file(paths["hf.gpkg"], driver="GPKG")
    gpd.GeoDataFrame(
        {"NAME_1": ["West", "East"]},
        geometry=[
            geometry.box(500000, 9000000, 502000, 9004000),
            geometry.box(502000, 9000000, 504000, 9004000),
        ],
        crs="EPSG:32736",
    ).to_file(paths["admin.gpkg"], driver="GPKG")
    return paths, tmp_path


def test_accessibility_writes_the_travel_time(region):
    paths, out = region
    result = accessibility.run(
        paths["lcv.tif"],
        paths["dem.tif"],
        paths["scenarios.csv"],
        paths["hf.gpkg"],
        "T",
        str(out),
        "w_x",
    )
    assert result["facilities"] == 2
    with rasterio.open(out / "T_travel_time_w_x.tif") as f:
        minutes = f.read(1, masked=True)
    assert minutes[20, 5] == 0 and minutes[20, 30] == 0
    # half a cell of road, then 1,950 m of grass at 4 km/h: 3 s + 1,755 s
    assert minutes[0, 5] == 29
    assert minutes.mask[:, 39].all()
    with rasterio.open(out / "T_speed.tif") as f:
        assert f.read(1)[20, 0] == 3060000


def test_coverage_shares_the_population_out(region):
    paths, out = region
    result = coverage.run(
        paths["lcv.tif"],
        paths["dem.tif"],
        paths["pop.tif"],
        paths["scenarios.csv"],
        paths["hf.gpkg"],
        "T",
        str(out),
        "vfmRelevanceScore",
        "name",
        60,
        "w_x",
        "beds",
        paths["admin.gpkg"],
        "NAME_1",
    )
    assert result["facilities"] == 2 and result["catchments"] == 2
    table = gpd.pd.read_csv(out / "T_coverage_analysis_w_x.csv")
    assert table["xid"].tolist() == ["a", "b"]  # by decreasing ranking value
    assert table["amCapacityRealised"][0] == pytest.approx(500)
    with rasterio.open(out / "T_pop_resid_w_x.tif") as f:
        left = f.read(1)
    people = 39 * 40 * 10
    assert np.nansum(left) == pytest.approx(
        people - table["amCapacityRealised"].sum(), rel=1e-6
    )
    assert np.isnan(left[:, 39]).all()
    zones = gpd.pd.read_csv(out / "T_zonal_stats_w_x.csv")
    assert zones["NAME_1"].tolist() == ["West", "East"]
    assert zones["amPopSum"].sum() == pytest.approx(people)
    assert zones["amPopResidualSum"].sum() == pytest.approx(np.nansum(left))
    catchments = gpd.read_file(out / "T_catchment_w_x.gpkg")
    assert catchments["xid"].tolist() == ["a", "b"]
    assert set(catchments.columns) == {"length", "xid", "name", "type", "geometry"}


def test_threads_and_windows_do_not_change_the_results(region, monkeypatch):
    paths, out = region
    slow = str(out / "slow.csv")
    with open(slow, "w") as f:
        f.write("class,label,speed,mode\n2,Grass,4,WALKING\n5,Road,6,MOTORIZED\n")

    def run(tag, threads):
        coverage.run(
            paths["lcv.tif"],
            paths["dem.tif"],
            paths["pop.tif"],
            slow,
            paths["hf.gpkg"],
            "T",
            str(out / tag),
            "vfmRelevanceScore",
            "name",
            10,
            "w_x",
            "beds",
            paths["admin.gpkg"],
            "NAME_1",
            threads=threads,
        )

    # ten minutes at 6 km/h is ten cells: each search runs on a window smaller than the grid
    assert coverage.reach_radius(speedmap.read_scenarios(slow), 10, CELL) == 12
    run("one", 1)
    run("four", 4)
    monkeypatch.setattr(coverage, "reach_radius", lambda *args: 10_000)
    run("whole", 2)
    for other in ("four", "whole"):
        for name in ("T_coverage_analysis_w_x.csv", "T_zonal_stats_w_x.csv"):
            assert (out / other / name).read_text() == (out / "one" / name).read_text()
        with (
            rasterio.open(out / other / "T_pop_resid_w_x.tif") as a,
            rasterio.open(out / "one" / "T_pop_resid_w_x.tif") as b,
        ):
            assert np.array_equal(a.read(1), b.read(1), equal_nan=True)
        a = gpd.read_file(out / other / "T_catchment_w_x.gpkg")
        b = gpd.read_file(out / "one" / "T_catchment_w_x.gpkg")
        assert a["xid"].tolist() == b["xid"].tolist()
        assert all(x.equals(y) for x, y in zip(a.geometry, b.geometry))


def test_what_the_engine_does_not_do_is_refused():
    with pytest.raises(NotImplementedError):
        accessibility.check_options("isotropic", False)
    with pytest.raises(NotImplementedError):
        accessibility.check_options("anisotropic", True)
