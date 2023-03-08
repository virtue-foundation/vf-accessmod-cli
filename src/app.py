from dataclasses import dataclass
import functools
import json
import os
import subprocess
from threading import Thread

from flask import Flask, flash, request, redirect, url_for, send_from_directory
from werkzeug.utils import secure_filename

app = Flask(__name__)

@dataclass
class JobRunner:
    check_endpoint = "/check"
    file_transfer_endpoint = "/file_transfer"
    landcover_endpoint = "/merge_landcover"
    accessibility_endpoint = "/accessibility_analysis"
    error = False
    last_endpoint = None
    running = False
    source_path = "/workspaces/vf-accessmod-cli/src"

    def _start_job(self, job):
        if job == "landcover":
            self.last_endpoint = self.landcover_endpoint
        elif job == "accessibility":
            self.last_endpoint = self.accessibility_endpoint
        else:
            raise ValueError("Unexpected job type")
        self.running = True
        self.error = False

    def status_json(self):
        return {"check_endpoint": self.check_endpoint, "last_endpoint": self.last_endpoint, "running": self.running, "error": self.error}

    def tracked_subprocess(self, process, job):
        self._start_job(job)
        result = subprocess.run(process)
        if result.returncode == 1:
            self.error = True
        self.running = False


job_runner = JobRunner()


def allowed_file(filename):
    return '.' in filename and \
           os.path.splitext(filename)[1].lower() in [".csv", ".tif", ".img", ".geojson"]


def single_job_only(func):
    """Raises an error if a job is already running."""
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        if job_runner.running:
            raise ValueError("Cannot start job until previous one is finished")
        return func(*args, **kwargs)
    return wrapper


class FilePathHandler:
    def __init__(self, region_string):
        self.region_string = region_string
        self.base_path = "/geodata"
        self.gadm_filename_prefix = "gadm41_"
        self.path_output = os.path.join(self.base_path, region_string[0:3])
        self.path_lakes = self._path_combine("lakes.geojson")
        self.path_land_use_key = self._path_combine("land_use_key.csv")
        self.path_landcover = self._path_combine("esri.tif")
        self.path_rivers = self._path_combine("rivers.geojson")
        self.path_roads = self._path_combine("roads.geojson")
        self.path_srtm = self._path_combine("srtm.tif")
        self.path_merged_landcover = self._path_combine("merged_landcover.img")
        self.path_facilities = self._path_combine("facilities.geojson")
        self.path_scenario_table = self._path_combine("scenario_table.csv")

    def _path_combine(self, suffix):
        return os.path.join(self.path_output, self.region_string + "_" + suffix)

    def _get_gadm_path(self, level):
        return os.path.join(self.path_output, f"{self.gadm_filename_prefix}{self.region_string}_l{level}")


def run_merge_landcover(region_string, skip_rivers=False, skip_lakes=False, skip_artifacts=False):
    paths = FilePathHandler(region_string)
    process = ["Rscript", os.path.join(job_runner.source_path, "mergeLandCover.R")]
    process.extend(["--lcv", paths.path_landcover])
    process.extend(["--roads", paths.path_roads])
    if skip_rivers:
        process.extend(["--b1", "null"])
    else:
        process.extend(["--b1", paths.path_rivers])
    if skip_lakes:
        process.extend(["--b2", "null"])
    else:
        process.extend(["--b2", paths.path_lakes])
    process.extend(["--table", paths.path_land_use_key])
    process.extend(["--name", region_string])
    process.extend(["--output_dir", paths.path_output])
    process.append("--debug_print")
    if not skip_artifacts:
        process.append("--clean-bridges")
    job_runner.tracked_subprocess(process, "landcover")


def run_accessibility_analysis(region_string, facilities_subset=None, knights_move=False, anisotropic=True):
    paths = FilePathHandler(region_string)
    process = ["Rscript", os.path.join(job_runner.source_path, "accessibilityAnalysis.R")]
    process.extend(["--lcv", paths.path_merged_landcover])
    process.extend(["--dem", paths.path_srtm])
    process.extend(["--scenarios", paths.path_scenario_table])
    process.extend(["--facilities", paths.path_facilities])
    process.extend(["--name", region_string])
    if facilities_subset:
        process.extend(["--facilities_subset", facilities_subset])
    if anisotropic:
        process.extend(["--analysis_type", "anisotropic"])
    else:
        process.extend(["--analysis_type", "isotropic"])
    if knights_move:
        process.append("--knights_move")
    process.extend(["--output_dir", paths.path_output])
    process.append("--debug_print")
    job_runner.tracked_subprocess(process, "accessibility")


@app.post(job_runner.landcover_endpoint)
@single_job_only
def landcover_request():
    request_data = request.get_json()
    region_string = request_data["region_string"]
    skip_rivers = request_data.get("skip_rivers", False)
    skip_lakes = request_data.get("skip_lakes", False)
    skip_artifacts = request_data.get("skip_artifacts", False)
    args = [region_string, skip_rivers, skip_lakes, skip_artifacts]
    Thread(target=run_merge_landcover, args=args).start()
    return job_runner.status_json(), 202


@app.post(job_runner.accessibility_endpoint)
@single_job_only
def accssibility_request():
    request_data = request.get_json()
    region_string = request_data["region_string"]
    facilities_subset = request_data.get("facilities_subset", None)
    knights_move = request_data.get("knights_move", False)
    anisotropic = request_data.get("anisotropic", True)
    args = [region_string, facilities_subset, knights_move, anisotropic]
    Thread(target=run_accessibility_analysis, args=args).start()
    return job_runner.status_json(), 202


@app.get(job_runner.check_endpoint)
def check_request():
    return job_runner.status_json(), 200


@app.route(job_runner.file_transfer_endpoint, methods=['GET', 'POST'])
def file_transfer():
    def _get_path_object(json_data):
        return FilePathHandler(json_data["region_string"])

    def _download():
        json_data = request.get_json()
        paths = _get_path_object(json_data)
        filename = json_data["filename"]
        return send_from_directory(directory=paths.path_output, path=filename)

    def _upload():
        paths = _get_path_object(request.form)
        # check if the post request has the file part
        if 'file' not in request.files:
            flash('No file part')
            return redirect(request.url)
        file = request.files['file']
        # If the user does not select a file, the browser submits an
        # empty file without a filename.
        if file.filename == '':
            flash('No selected file')
            return redirect(request.url)
        if file and allowed_file(file.filename):
            print("allowed")
            filename = secure_filename(file.filename)
            path = paths.path_output
            os.makedirs(path, exist_ok=True)
            file.save(os.path.join(path, filename))
            return {"region_string": paths.region_string, "filename": filename}, 201

    if request.method == 'POST':
        return _upload()
    return _download()
