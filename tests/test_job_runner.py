import pytest

from app import JobRunner, single_job_only, job_runner


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

            with pytest.raises(ValueError, match="Cannot start job"):
                dummy()
        finally:
            job_runner.running = False
