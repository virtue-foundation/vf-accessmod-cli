from app import allowed_file


def test_allowed_extensions():
    assert allowed_file("data.csv")
    assert allowed_file("data.tif")
    assert allowed_file("data.img")
    assert allowed_file("data.geojson")


def test_case_insensitive():
    assert allowed_file("data.CSV")
    assert allowed_file("data.TIF")
    assert allowed_file("data.IMG")
    assert allowed_file("data.GEOJSON")


def test_no_extension():
    assert not allowed_file("data")


def test_wrong_extension():
    assert not allowed_file("data.exe")
    assert not allowed_file("data.jpg")
    assert not allowed_file("data")
