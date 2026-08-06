# Env image modernization plan

Goal: bring `build-env-image/` up to — and ahead of — AccessMod 5.9.1's dependency baseline. Two stages, split so the remote build is the gate.

Constraint: **no local build.** GRASS/R can't run on the bare host — everything is verified by pushing a branch and watching GitHub Actions. So stage 1's definition of done is "the `Env Dependencies` + `Docker` + `Tests` workflows go green"; we deliberately do **not** touch R source in stage 1.

## Decision: rebase on `ubuntu:26.04` LTS ("Resolute")

We rebase the env image from `ubuntu:22.04` to `ubuntu:26.04` LTS. This is the "right way": 26.04 ships every target version from **native apt** — no PPAs, no external CRAN/UbuntuGIS repos, no pinned digests for geo libs. One source of truth for versions, LTS support to 2031.

Verified package versions in 26.04's apt (vs. 22.04 current, and AccessMod 5.9.1 upstream):

| Component | 22.04 (ours) | **26.04 (target)** | Upstream 5.9.1 |
|---|---|---|---|
| R | 4.1.2 | **4.5.2** | 4.5.0 |
| GRASS (apt `grass-dev`) | — | **8.4.2** | 8.3.2 (source) |
| GDAL | 3.4.1 | **3.12.2** | 3.9.1 |
| GEOS | 3.10.2 | **3.14.1** | 3.12+ |
| PROJ | 8.2.1 | **9.7.1** | 3.9-era |
| gcc | 11 | **15.2** | — |

Soname / package-name bumps in 26.04 (verify each at edit time from the 26.04 Packages index):

| 22.04 package | 26.04 package | Note |
|---|---|---|
| `libgdal30` | `libgdal38` | GDAL 3.12.2 soname |
| `libgeos3.10.2` | `libgeos3.14.1` | (or runtime `libgeos-c1`) |
| `libicu70` | `libicu78` | ICU 78.2 |
| `libpng16-16` | `libpng16-16t64` | 24.04+ t64 time64 transition |
| `libtiff5` | `libtiff6` | `libtiff5` not found in 26.04 |
| `libexecs0` | `libexecs1` | `libexecs0` not found in 26.04 |
| `libjsoncpp25` | `libjsoncpp26` | |
| `libncurses5` | `libncurses6` / `libtinfo6` | 26.04 ships ncurses6 |

The table above is verified against `packages.ubuntu.com` for 26.04 (Resolute) and 22.04 (jammy), but treat every soname as a build-time assertion — a wrong name fails immediately at `apt-get install` and names the missing package.

26.04 doesn't just match upstream — it's ahead on GDAL/GEOS/PROJ, and it's the only base that hits R 4.5 + modern geo libs with zero external repos. Accepted tradeoff: bigger blast radius (libc, python3, every system lib jumps a generation) — explicitly accepted by the team.

## Why GRASS is still a source build

26.04 ships `grass-dev` 8.4.2, but we still build GRASS from source for two reasons:

1. **`patches/raster`** modifies GRASS *core* — `raster/r.reclass/{parse,main,input}.c` — to allow huge reclass rule tables (`lo[102400], hi[102400]`). apt's GRASS isn't patched.
2. **`r.walk.accessmod`** is a custom C addon compiled against GRASS headers.

We build **GRASS 8.4.2 from source** (matching apt's `grass-dev` 8.4.2) so the platform is self-consistent — apt's dev headers and our source build share the same major.minor.

**Patch-risk note:** the patches are **full-file replacements** (`COPY patches .` overwrites the three `r.reclass` files), not context diffs — they cannot fail to "apply." The only question is API compatibility, and the patched code uses only stable `Rast_*` APIs unchanged between GRASS 8.3 and 8.4. Low risk.

`modules/r.walk.accessmod` and `patches/raster` are byte-identical to upstream 5.9.1 — no porting needed. The `GRASS_CONFIG` flag block also still applies to GRASS 8.

---

## Stage 1 — make the remote build green (Dockerfile only)

No R source changes. The image must build and all three workflows (`docker-publish-environment.yml`, `docker-publish.yml`, `test.yml`) must pass on the pushed branch.

### 1.1 Rebase on `ubuntu:26.04` + bump GRASS to 8.4.2 (source build)

Single coordinated change to the top of the Dockerfile:

- `FROM ubuntu:22.04 as main` → `FROM ubuntu:26.04 AS main` (also fix the deprecated lowercase `as`).
- `ARG GRASS_VERSION=7.8.7` → `8.4.2`.
- Replace **all 9** `/usr/local/grass78` occurrences with `/usr/local/grass84` — `grep -c 'grass78'` in the Dockerfile returns 9: the `make MODULE_TOPDIR=`, the `cp module_items.xml`, the `rm -rf demolocation/fonts/gui/share` lines, the `mkdir -p .../gui/wxpython/xml`, the `mv module_items.xml`, and a trailing comment. (The plan originally said "four" — that was an undercount.) Better still, prefer `$(grass --config path)` dynamically where the surrounding step allows it, so the path stops being hardcoded. The `ln -sf /usr/local/grass \`grass --config path\`` already follows `grass --config path` and self-heals — keep it.
- Update the runtime apt package names to 26.04's versions. Several 22.04-specific pins change: `libgdal30` → `libgdal36` (26.04's soname), `libgeos3.10.2` → `libgeos3.14.1` (or just `libgeos-c1` runtime), `libjsoncpp25` → newer `libjsoncppXX`, `libicu70` → `libicu80` (26.04 ships ICU 80). Verify exact names from the 26.04 Packages index at edit time — don't guess sonames.
- `r-base` now pulls R 4.5.2, `libgdal-dev` pulls GDAL 3.12.2, `libgeos-dev` pulls GEOS 3.14.1, `libproj-dev` pulls PROJ 9.7.1 — all from native apt, **no PPAs added**.
- Confirm `r.walk.accessmod`'s `Makefile` builds against GRASS 8.4.2 headers (upstream uses the same Makefile against 8.3.2; 8.4.2 is API-compatible — but the GHA build is the proof).

### 1.2 Drop the 22.04-specific locale/package workarounds

Review and remove anything that existed only because of 22.04's age:

- The `musl` / `musl-tools` apt installs — these were for cross-arch/cairo edge cases on 22.04; verify whether 26.04's toolchain still needs them (likely not, but keep if the build complains).
- `libncurses5` → 26.04 ships `libncurses6`/`libtinfo6`; update or drop.
- The `python3-six` / `python3-numpy-dev` installs are bare package names (no version pins to update) — 26.04's python3 is much newer; `python3-dev` + `python3-numpy` should suffice. Re-confirm against the build.

### 1.3 R package install robustness

`build_r_packages.sh` currently `Rscript -e 'install("<pkg>")'` with no repo pinned and `quit('no',status=1)` only via the `.Rprofile` tryCatch. On R 4.5 + fresh CRAN this can flake. Two cheap fixes:

- Pin a CRAN snapshot via `.Rprofile` `options(repos=...)` to a dated Posit Pak URL (upstream does this with `R_PACKAGES_DATE=2024-12-31`). Reproducible + matches upstream's known-good set.
- Keep the existing per-package loop; just feed it the pinned repo.

No change to `requirements_r.txt` contents (space-separated, don't reformat — AGENTS.md calls this out).

### 1.4 uv digest pin

`COPY --from=ghcr.io/astral-sh/uv@sha256:93b61e21...` — leave as-is unless the build breaks; bumping is a deliberate act (the comment says so). Only revisit if the pinned image is unreachable from GHA.

### 1.5 CI path-filter sanity

`docker-publish-environment.yml` already rebuilds the env image on `build-env-image/**` / `requirements_r.txt` / `pyproject.toml` / `uv.lock` pushes to `main`. To exercise it from a branch, either:

- push to a `dev-*` branch and temporarily widen the env workflow's `branches:` + add `pull_request:` (revert before merge), **or**
- use `workflow_dispatch` on `main` after merging (riskier — bakes the image tag).

**Prefer the `dev-*` + temporary trigger widen**, so the env image builds on the branch. Note: `test.yml`/`docker-publish.yml` `container:` (and the app Dockerfile `FROM`) the published `:main` env image, so until the new env image is actually pushed to `:main`, those jobs run against the **old** env. Plan for this in stage-1 validation (below).

### Stage 1 validation (on the pushed branch)

1. `Env Dependencies` workflow builds the env image and pushes to `ghcr.io/.../vf-accessmod-cli_env:main` (or a tag). **Green = Dockerfile compiles, GRASS 8 builds, R 4.5 installs, R packages install, uv pip install works.**
2. Because the app/test workflows `FROM`/`container:` the *published* `:main` env image, they'll only reflect the new env **after** the env image is pushed. So validate the app image + tests in a follow-up push once `:main` is updated, or temporarily point the app Dockerfile / test container at the newly-pushed env tag.
3. Once green on `:main` for env, re-trigger `Docker` + `Tests` — `r-parse-check` (parses all `src/*.R` with the new R 4.5) and `pytest`/R testthat must pass. **Expect the R parse check to pass** (we're not changing R syntax), and `test_job_runner` / `test_file_path_handler` / `test_allowed_file` (Python, env-independent) to pass. The R testthat unit tests (`functions.R` helpers, no GRASS) should pass; `tests/integration/test_grass_session.R` will run **inside** the new env container — this is our first signal whether GRASS 8 + `initGRASS()` still works. If integration fails, that's stage 2's input, not a stage 1 blocker (the *image* still built).

**Stage 1 done = env image published green to `:main`, and Python tests green on that `:main` env.** (Per the sequencing note above, this implies the env image is already on `:main`; the Python tests are env-independent and will pass either way, but validating them against the new `:main` is the clean signal.) GRASS-session integration failures are expected and deferred to stage 2.

---

## Stage 2 — fix the R code for GRASS 8.4

After the new env image is on `:main`, the app/test workflows run against it. Fix whatever stage 1 surfaced.

### 2.1 `init_session.R` / `rgrass` call sites

Current code (`src/init_session.R`):

```r
grass_session_metadata <- tryCatch(gmeta(), error = function(e) NULL)
if (is.null(grass_session_metadata)) {
  initGRASS(gisBase = config$gisBase, gisDbase = config$GrassDataBase, override = TRUE)
}
```

GRASS 8 + modern `rgrass` (the renamed `rgrass` package started at 0.2-0 after `rgrass7` was archived; current is 0.5-3 — the code already uses `require(rgrass)`, so the migration is done in name) changes to verify:

- `gmeta()` → still present, but prefers an already-running GRASS session started via `grass --tmp-location` etc. Under GRASS 8 the session-detection semantics shifted; `gmeta()` may error differently when no session is active. Confirm the `tryCatch` still treats "no session" as the error path.
- `initGRASS()` — the `gisBase`/`gisDbase` args are unchanged, but GRASS 8 is stricter about mapset ownership (`GRASS_SKIP_MAPSET_OWNER_CHECK=1` is set in the env, good) and about the `LOCATION` being pre-created. If `config$GrassDataBase` mapset doesn't exist, `initGRASS` may now require an explicit `location=`/`mapset=` or a prior `g.mapset` create.
- `config$gisBase` comes from `Sys.getenv("GISBASE")` which the Dockerfile sets to `/usr/local/grass` (symlinked to `grass78` today; after the bump it self-heals to `grass84`). Should resolve fine; verify `grass --config path` agrees at runtime.

### 2.2 Three entrypoints

`accessibilityAnalysis.R`, `geoCoverageAnalysis.R`, `mergeLandCover.R` each `source("init_session.R")` after `config.R`. The logging convention (`open_startup_logs` / `migrate_to_run_logs`) is GRASS-version-independent. What to check per entrypoint:

- Any `execGRASS("r.walk.accessmod", ...)` calls — module interface unchanged (same binary, same flags), but GRASS 8 module JSON parser is stricter about flag/option names. Run each entrypoint against the demo region and watch `error_log.txt`.
- `r.reclass` (patched) — confirm the patched version still builds under GRASS 8.4.2. The patch is full-file replacements of `raster/r.reclass/{parse,main,input}.c`, so it can't fail to apply; the only risk is an API drift in `Rast_*` between 8.3 and 8.4 (none expected — verified stable). If a build error surfaces, regenerate the three files against the 8.4.2 source tree.

### 2.3 `config.R` / dictionary

`config.R` loads `dictionary/classes.json`. No GRASS dependency. Only revisit if a class id or flag name referenced by the entrypoints changed upstream between 7.8 and 8 (none expected — classes.json is our own).

### 2.4 Integration test as the gate

`tests/integration/test_grass_session.R` requires a live GRASS session and runs in the env container. Once the env image is rebuilt, this test runs in GHA `test.yml`. **Stage 2 done = this test passes green in GHA on the new env image.**

### 2.5 R parse-check note

`docker-publish.yml`'s `r-parse-check` job parses all `src/*.R` with the env image's R. With R 4.5 now in the image, R-4.5-only syntax (native pipe `_` placeholder, `if()` in more contexts) would parse fine — but we're not introducing any, so this stays green as long as we don't regress.

---

## Out of scope / deliberately not touched

- Switching base OS to Alpine (upstream did; we stay on Ubuntu — no musl/libc-compat churn, and 26.04 gives us the versions natively anyway).
- Shipping the full upstream R runtime package set (`shiny`, `leaflet`, `sf`, `terra`, etc.) — the CLI Flask+`Rscript` design only needs the tooling deps already in `requirements_r.txt`. Revisit only if a stage-2 entrypoint turns out to need one.
- `modules/r.walk.accessmod` / `patches/raster` source — identical to upstream 5.9.1, no porting.
- uv digest bump — pinned deliberately.
- `requirements_r.txt` format — space-separated, don't reformat (AGENTS.md).

## Risk summary

- **Highest risk:** GRASS 7→8 `initGRASS`/`gmeta` behavior change breaking `init_session.R`. Mitigation: stage 2 exists exactly for this; the image building (stage 1) doesn't depend on it.
- **Medium risk:** 26.04 apt package soname/name drift (`libgdal30`→`libgdal36`, `libicu70`→`libicu80`, `libncurses5`→`6`, etc.) — any wrong guess fails the build immediately at `apt-get install`. Mitigation: verify exact names from the 26.04 Packages index at edit time; the build error names the missing package.
- **Low risk:** patched `r.reclass` API drift vs GRASS 8.4.2 — patches are full-file replacements using stable `Rast_*` APIs, so near-zero; if it fails, fast build-time feedback, regenerate the three files against 8.4.2 source.
- **Low risk:** R 4.5 vs 4.1 syntax in our own R (we don't use 4.5-only syntax; parse-check guards it).
- **Low risk:** GDAL 3.12 behavior diffs in any raster I/O our entrypoints do via `rgrass` (not `terra`/`sf` directly in the CLI path). Mitigation: integration test.
