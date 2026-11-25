import os
import sys

import pytest

import app as app_module
from app import DIRECTION_ENV, ENGINE_ENV, _setting, selected_engine


@pytest.fixture
def commands(monkeypatch):
    """The command lines the job handlers hand to the job runner."""
    seen = []
    monkeypatch.setattr(app_module.os, "makedirs", lambda *args, **kwargs: None)
    monkeypatch.setattr(app_module.job_runner, "tracked_subprocess", seen.append)
    monkeypatch.delenv(ENGINE_ENV, raising=False)
    monkeypatch.delenv(DIRECTION_ENV, raising=False)
    return seen


def _coverage(**kwargs):
    app_module.run_coverage_analysis(
        "RWA", 180, "w_x", gadm_level=2, capacity_column="beds", **kwargs
    )


def test_default_engine_is_the_r_scripts(commands):
    _coverage()
    app_module.run_accessibility_analysis("RWA", "w_x")
    src = app_module.job_runner.source_path
    assert commands[0][:2] == ["Rscript", os.path.join(src, "geoCoverageAnalysis.R")]
    assert commands[1][:2] == ["Rscript", os.path.join(src, "accessibilityAnalysis.R")]


def test_native_engine_takes_the_same_options(commands, monkeypatch):
    _coverage()
    app_module.run_accessibility_analysis("RWA", "w_x")
    monkeypatch.setenv(ENGINE_ENV, "native")
    _coverage()
    app_module.run_accessibility_analysis("RWA", "w_x")
    launcher = os.path.join(app_module.job_runner.source_path, "run_native.py")
    assert commands[2][:3] == [sys.executable, launcher, "coverage"]
    assert commands[3][:3] == [sys.executable, launcher, "accessibility"]
    assert commands[2][3:] == commands[0][2:]
    assert commands[3][3:] == commands[1][2:]


@pytest.mark.parametrize(
    "options", [{"knights_move": True}, {"anisotropic": False}], ids=str
)
def test_native_engine_leaves_what_it_does_not_do_to_r(commands, monkeypatch, options):
    monkeypatch.setenv(ENGINE_ENV, "native")
    _coverage(**options)
    app_module.run_accessibility_analysis("RWA", **options)
    assert [c[0] for c in commands] == ["Rscript", "Rscript"]
    assert selected_engine(**options) == "grass"


def test_coverage_direction(commands, monkeypatch):
    _coverage()
    monkeypatch.setenv(DIRECTION_ENV, "from")
    _coverage()
    app_module.run_accessibility_analysis("RWA")
    assert commands[0][commands[0].index("--direction") + 1] == "to"
    assert commands[1][commands[1].index("--direction") + 1] == "from"
    assert "--direction" not in commands[2]


def test_setting_reads_case_and_blanks(monkeypatch):
    monkeypatch.setenv(ENGINE_ENV, " Native ")
    assert selected_engine() == "native"
    monkeypatch.setenv(ENGINE_ENV, "")
    assert selected_engine() == "grass"


def test_unknown_setting_is_refused(monkeypatch):
    monkeypatch.setenv(ENGINE_ENV, "fast")
    with pytest.raises(ValueError, match=ENGINE_ENV):
        selected_engine()
    monkeypatch.setenv(DIRECTION_ENV, "toHF")
    with pytest.raises(ValueError, match=DIRECTION_ENV):
        _setting(DIRECTION_ENV, app_module.DIRECTIONS)
