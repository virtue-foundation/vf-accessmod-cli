# AGENTS.md

Guidance for agents working in `vf-accessmod-cli`. GUI-less, containerized port of WHO AccessMod 5 for the Virtue Foundation healthcare analytics pipeline.

## Stack

- **Python Flask API** (`src/app.py`) that spawns **R scripts** as `Rscript` subprocesses. No JS frontend.
- **R** scripts run inside a **GRASS GIS 7.8.7** session (`rgrass`), with a custom C module `r.walk.accessmod` and a patched `r.reclass`.
- No `package.json`, no test suite, no linter/formatter/typecheck configured. Don't invent commands that don't exist.

## Two-image Docker architecture (don't confuse them)

1. **Base env image** — `build-env-image/Dockerfile` builds `ghcr.io/virtue-foundation/vf-accessmod-cli_env:main`:
   - Compiles GRASS 7.8.7 from source (`--without-wxwidgets`, no GUI), applies `patches/`, and builds `modules/r.walk.accessmod` against it.
   - Installs R packages from `requirements_r.txt` (space-separated, one per line, installed via `build-env-image/build_r_packages.sh`) and Python deps from `requirements.txt` (`flask` only).
   - Sets all `AM5_*` / `GISBASE` / `GISDBASE` / `GRASS_*` env vars and exposes `grass` / `grass78`.
2. **App image** — root `Dockerfile`, `FROM` the env image above. Copies `src/`, runs `flask run --host=0.0.0.0` from `/workspaces/vf-accessmod-cli/src`.

CI (`.github/workflows/`): the env-image workflow only fires on changes under `build-env-image/**`, `requirements.txt`, `requirements_r.txt`. The app-image workflow fires on pushes to `main`/`staging`/`dev-*` and `v*.*.*` tags. If you change an R/Python dependency or the GRASS build, you must update the env image, not just the app image.

## Local dev

Run inside the env container (devcontainer uses the same env image); GRASS and R are not installable on a bare host without the full build. `config.R` initializes a GRASS session on load — it expects `GISBASE` / `GISDBASE` env vars to already be set (they are, in the env image).

## App runtime conventions (`src/app.py`)

- Endpoints: `POST /merge_landcover`, `POST /accessibility_analysis`, `POST /coverage_analysis`, `GET /check`, `GET|POST /file_transfer`.
- **One job at a time.** `@single_job_only` raises if a job is already running; `JobRunner` tracks `running`/`error`/`last_endpoint`. Poll `/check` for completion.
- Job handlers start a background `Thread` that calls `Rscript <script>.R ...` and return `202` immediately. Non-zero R exit sets `error=True`.
- All region data lives under `/geodata/<region_string[0:3]>/<region_string>_<file>`. `FilePathHandler(region_string)` is the single source for these paths — reuse it rather than hardcoding.
- Allowed upload extensions: `.csv`, `.tif`, `.img`, `.geojson`.

## R script conventions

- `accessibilityAnalysis.R`, `geoCoverageAnalysis.R`, `mergeLandCover.R` are the three entrypoints invoked by `app.py`. Each uses `optparse`; `app.py`'s `_add_common_arguments` / `_add_accessibility_arguments` build the CLI, so changing a flag requires updating **both** the R `make_option` list and the Python builder.
- `--name` (region string), `--output_dir`, and `--debug_print` are added to every script by `_add_common_arguments`.
- Shared helpers: `functions.R` (general), `functions-geo.R` (GRASS/geo), `functions_accessibility.R`. `config.R` holds all paths/classes and loads `dictionary/main.json` + `dictionary/classes.json` — edit dictionary JSON, not the R that reads it.

## Things that are easy to get wrong

- `requirements_r.txt` is **space-separated package names**, not pip-style. `build_r_packages.sh` reads it line by line (the `while IFS=" "` loop tolerates the single-token-per-line form). Don't reformat it.
- The app image's `ENTRYPOINT` is `flask run --host=0.0.0.0`; there is no separate WSGI server. `FLASK_APP` is intentionally not set — Flask auto-discovers `app.py` in the WORKDIR.
- `accessmod-changes.md` is the upstream AccessMod changelog (reference), not this project's changelog.
- `/geodata` and `grassdb/` are runtime data dirs (gitignored); never commit contents.
