import threading
from unittest import mock

import pytest

from app import (
    JobRunner,
    JobConflictError,
    _run_job_in_thread,
    single_job_only,
    job_runner,
    app,
)


class TestJobRunner:
    def test_start_job_sets_state(self):
        jr = JobRunner()
        assert not jr.running
        assert not jr.error

        jr._start_job("landcover")
        assert jr.running
        assert jr.last_endpoint == "/merge_landcover"
        assert not jr.error

    def test_start_job_clears_error(self):
        jr = JobRunner()
        jr.error = True
        jr._start_job("landcover")
        assert not jr.error

    def test_start_job_unknown_raises(self):
        jr = JobRunner()
        with pytest.raises(ValueError, match="Unexpected job type"):
            jr._start_job("nonexistent")

    def test_status_json_shape(self):
        jr = JobRunner()
        jr._start_job("accessibility")
        assert jr.status_json() == {
            "check_endpoint": "/check",
            "last_endpoint": "/accessibility_analysis",
            "running": True,
            "error": False,
        }

    def test_status_json_defaults(self):
        jr = JobRunner()
        assert jr.status_json() == {
            "check_endpoint": "/check",
            "last_endpoint": None,
            "running": False,
            "error": False,
        }

    @pytest.mark.parametrize(
        "returncode,expected_error", [(0, False), (1, True), (137, True), (139, True)]
    )
    def test_tracked_subprocess_sets_error_on_nonzero_exit(
        self, returncode, expected_error
    ):
        jr = JobRunner()
        with mock.patch(
            "app.subprocess.run", return_value=mock.Mock(returncode=returncode)
        ):
            jr.tracked_subprocess(["Rscript", "script.R"])
        assert jr.error is expected_error
        assert not jr.running

    def test_start_job_raises_when_already_running(self):
        jr = JobRunner()
        jr._start_job("landcover")
        with pytest.raises(ValueError, match="Cannot start job"):
            jr._start_job("coverage")

    def test_concurrent_start_job_exactly_one_wins(self):
        jr = JobRunner()
        results = []
        barrier = threading.Barrier(2)

        def try_start():
            barrier.wait(timeout=5)
            try:
                jr._start_job("landcover")
                results.append("ok")
            except ValueError:
                results.append("busy")

        threads = [threading.Thread(target=try_start) for _ in range(2)]
        for t in threads:
            t.start()
        for t in threads:
            t.join(timeout=5)
        assert sorted(results) == ["busy", "ok"]

    def test_launch_job_reserves_running_before_thread_starts(self):
        started = threading.Event()
        release = threading.Event()

        def slow_job():
            started.set()
            release.wait(5)

        with mock.patch("app.job_runner", JobRunner()) as jr:
            thread = _run_job_in_thread("landcover", slow_job, ())
            # Reserved synchronously: running is True before the thread body runs.
            assert jr.running
            assert started.wait(5)
            assert jr.running
            release.set()
            thread.join(timeout=5)
            assert not jr.running

    def test_launch_job_clears_running_and_sets_error_on_exception(self):
        def boom():
            raise RuntimeError("job crashed")

        with mock.patch("app.job_runner", JobRunner()) as jr:
            thread = _run_job_in_thread("landcover", boom, ())
            thread.join(timeout=5)
        assert jr.error
        assert not jr.running


class TestSingleJobOnly:
    def test_passes_when_not_running(self):
        assert not job_runner.running

        @single_job_only
        def dummy():
            return 42

        assert dummy() == 42

    def test_raises_when_running(self):
        job_runner.running = True
        try:

            @single_job_only
            def dummy():
                return 42

            with pytest.raises(JobConflictError, match="Cannot start job"):
                dummy()
        finally:
            job_runner.running = False


class TestJobConflictHttpStatus:
    def test_job_conflict_returns_409(self):
        job_runner.running = True
        try:
            resp = app.test_client().post("/merge_landcover", json={})
            assert resp.status_code == 409
            assert resp.get_json() == {
                "error": "Cannot start job until previous one is finished"
            }
        finally:
            job_runner.running = False

    def test_non_conflict_error_is_not_409(self):
        # A genuine handler error (missing region_string) must NOT be masked as 409.
        job_runner.running = False
        resp = app.test_client().post("/merge_landcover", json={})
        assert resp.status_code == 400
        assert resp.get_json() == {"error": "region_string is required"}
