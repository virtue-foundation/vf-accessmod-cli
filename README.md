# vf-accessmod-cli

A GUI-less rewrite of the WHO's [Accessmod 5](https://github.com/unige-geohealth/accessmod) software for use in the Virtue Foundation's Healthcare Analytics pipeline. Contains command line only scripts as well as a Flask based rest API for use in containers.

_This software is a modified version of the World Health Organization's "Accessmod ver, 5" software. This software is not the product of, is not provided by, and is not endorsed or affiliated with, the World Health Organization. The original AccessMod ver. 5 software is © the World Health Organization.

## Engines

The accessibility and coverage analyses have two engines. The container's `ACCESSMOD_ENGINE`
environment variable chooses one; requests, output files and their names are the same.

- `grass` (default): the R scripts over GRASS.
- `native`: the in-memory engine in `src/native` (Python, numba). It does the anisotropic
  analysis without the knight's move; a request for the isotropic analysis or the knight's move
  runs on the R scripts whatever the setting. Land cover merging always runs on the R scripts.

`native` writes the same travel-time raster, speed raster, population left, catchments and zonal
statistics as `grass`, cell for cell (checked against stored production results of Rwanda,
Burundi, Lesotho, Togo and Senegal, where coverage took 42 to 149 s against 413 to 2,445 s).
Known differences: two reporting columns of the capacity table (`amPopCoveredPercent`,
`amCorrPopTime`) agree to about nine digits, the `*report.txt` and log files of the R scripts
are not written, and identical results need an x86-64 machine (R's extended-precision sums).

In the coverage analysis `native` searches several facilities' travel times at once, one thread
per processor unless `ACCESSMOD_THREADS` says otherwise (fewer when memory is short); the results
do not depend on the number of threads.

`ACCESSMOD_COVERAGE_DIRECTION` sets the travel direction of the coverage analysis for both
engines: `to` the facilities (default, as in AccessMod 5) or `from` them. Up to v2.0.0 the
coverage analysis always travelled from the facilities, because the script compared `"toHF"`
with `"toHf"`; `from` reproduces those results. On the five countries above the national
coverage moves by 0.01 point at most; small zones move more where capacity is short.

## Testing

### Pre-commit hooks

Run all linters and formatters (styler, lintr, ruff):

    uv run pre-commit run --all-files

### Python tests (pytest)

From repo root — no GRASS required:

    uv run pytest

### R pure-helper tests (testthat)

Runs inside the env container where testthat is installed:

    Rscript tests/run.R

Covers the pure R helper functions in `src/functions.R` (string/parse helpers
that don't need a GRASS session)._
