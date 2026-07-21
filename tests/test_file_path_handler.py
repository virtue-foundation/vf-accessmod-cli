import os

from app import FilePathHandler


def test_region_slicing():
    """path_output uses region_string[0:3]."""
    h = FilePathHandler("abcdef")
    assert h.path_output == os.path.join("/geodata", "abc")


def test_path_combine_prefix():
    """_path_combine joins path_output + region_string + '_' + suffix."""
    h = FilePathHandler("test_region")
    assert h.path_lakes == os.path.join(h.path_output, "test_region_lakes.gpkg")
    assert h.path_srtm == os.path.join(h.path_output, "test_region_srtm.tif")
    assert h.path_merged_landcover == os.path.join(
        h.path_output, "test_region_merged_landcover.tif"
    )


def test_gadm_path():
    h = FilePathHandler("test_region")
    assert h.get_gadm_path(2) == os.path.join(
        h.path_output, "gadm41_test_region_l2.gpkg"
    )
    assert h.get_gadm_path(1) == os.path.join(
        h.path_output, "gadm41_test_region_l1.gpkg"
    )


def test_gadm_column():
    h = FilePathHandler("test_region")
    assert h.get_gadm_column(2) == "NAME_2"
    assert h.get_gadm_column(-2) == "NAME_2"
    assert h.get_gadm_column(0) == "NAME_0"
