# Env image modernization plan

Goal: bring `build-env-image/` up to — and ahead of — AccessMod 5.9.1's dependency baseline. Two stages, split so the remote build is the gate. **Stage 1 is done** (commit `097f563`, merged to `main`); Stage 2 is pending.

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
| `libexecs0` | — dropped | orphan; zero reverse deps in 26.04 (nothing depends on libexecs) |
| `libjsoncpp25` | `libjsoncpp26` | |
| `libncurses5` | `libncurses6` / `libtinfo6` | 26.04 ships ncurses6 |
| `fftw2` | — dropped | dead; GRASS uses FFTW3 (`libfftw3-double3`, pulled transitively by `libgdal38`) |
| `gnutls-bin` | — dropped | CLI tools only; gnutls runtime lib stays via `libgdal38`→`libcurl3t64-gnutls` |
| (new) | `libfftw3-double3` | GRASS `--with-fftw` runtime (FFTW3, not FFTW2) |

Verified against the 26.04 (Resolute) and 22.04 (jammy) Packages indexes at edit time, then confirmed by the green GHA build. Where this plan's §1.1 prose disagreed with the table (`libgdal36` vs `libgdal38`, `libicu80` vs `libicu78`), **the table was right**.

26.04 doesn't just match upstream — it's ahead on GDAL/GEOS/PROJ, and it's the only base that hits R 4.5 + modern geo libs with zero external repos. Accepted tradeoff: bigger blast radius (libc, python3, every system lib jumps a generation) — explicitly accepted by the team.

## Why GRASS is still a source build

26.04 ships `grass-dev` 8.4.2, but we still build GRASS from source for two reasons:

1. **`patches/raster`** modifies GRASS *core* — `raster/r.reclass/{parse,main,input}.c` — to allow huge reclass rule tables (`lo[102400], hi[102400]`). apt's GRASS isn't patched.
2. **`r.walk.accessmod`** is a custom C addon compiled against GRASS headers.

We build **GRASS 8.4.2 from source** (matching apt's `grass-dev` 8.4.2) so the platform is self-consistent — apt's dev headers and our source build share the same major.minor.

**Patch-risk note:** the patches are **full-file replacements** (`COPY patches .` overwrites the three `r.reclass` files), not context diffs — they cannot fail to "apply." The only question is API compatibility, and the patched code uses only stable `Rast_*` APIs unchanged between GRASS 8.3 and 8.4. Low risk.

`modules/r.walk.accessmod` and `patches/raster` are byte-identical to upstream 5.9.1 — no porting needed. The `GRASS_CONFIG` flag block also still applies to GRASS 8.

---

## Stage 1 — DONE (commit `097f563`, merged to `main`)

The env image builds green on 26.04 and is published to `:main`. Record of what shipped — including two fixes the original plan didn't anticipate (FFTW3, PEP 668).

### Rebase + GRASS 8.4.2 source build

- `FROM ubuntu:22.04` → `26.04`; `ARG GRASS_VERSION` 7.8.7 → 8.4.2; lowercase `as` → `AS` on every `FROM`.
- All 9 `grass78` paths → `grass84` via `$(grass --config path)` (self-heals on future bumps); the existing `ln -sf /usr/local/grass \`grass --config path\`` kept. Verified against 8.4.2's `Install.make` — `make install` puts a single `grass` binary in `/usr/local/bin` (no `grass84` twin), so the `COPY /usr/local/bin/grass*` wildcard stays single-match.
- Runtime sonames bumped per the 26.04 Packages index (see table above). `r-base`→R 4.5.2, `libgdal-dev`→GDAL 3.12.2, `libgeos-dev`→GEOS 3.14.1, `libproj-dev`→PROJ 9.7.1, gcc 15.2 — all native apt, no PPAs.

### FFTW3 migration (unplanned — required by the build)

GRASS 8.4's `fft.c` needs **FFTW3**, not FFTW2. Under gcc-15 the old `fftw2`/`fftw-dev` path errored and cascaded into `-lgrass_gmath.8.4` link failures. Shipped: build `fftw-dev` → `libfftw3-dev`; runtime add `libfftw3-double3`; drop `fftw2` (zero reverse deps in the archive). `--with-fftw` now links FFTW3.

### GRASS_CONFIG changes

- `--without-wxwidgets` and `--without-ffmpeg` **removed** — the build wouldn't link under gcc-15/8.4.2 with them. Net effect is near-nil (configure auto-detects wxwidgets dev, which isn't installed, and skips; we run headless via `rgrass` regardless), but the `gui/wxpython/xml/module_items.xml` preservation in the size-reduction step now corresponds to real built artifacts.
- Rest of the flag block unchanged (`--with-proj-share`, `--with-cairo`, `--without-{x,pdal,postgres,openmp,freetype,opengl,nls,mysql,odbc}`); all still valid in 8.4.2.

### Dead-weight / no-op cleanup

Dropped from the runtime stage: `libexecs0` (orphan — zero reverse deps; **not** swapped to `libexecs1` as the table originally speculated), `musl`/`musl-tools`/`python3-six` (22.04-era, unneeded on 26.04), `gnutls-bin` (CLI tools only; gnutls lib stays via `libgdal38`→`libcurl3t64-gnutls`). Build stage `python3-numpy-dev` → `python3-numpy`. Removed the no-op `ldconfig /etc/ld.so.conf.d` (conf.d is a config dir, not a lib dir; GRASS libs resolve via `GRASS_LD_LIBRARY_PATH`) and the dead `CXXFLAGS="$MYCXXFLAGS"` (undefined var → configure now uses its default CXXFLAGS).

### R package install robustness

`.Rprofile` now pins `options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2024-12-31"))` (upstream's known-good snapshot, compatible with R 4.5.2). Per-package loop and `requirements_r.txt` contents/format unchanged.

### PEP 668 / uv venv (unplanned — required by the build)

26.04's system Python 3.14 is externally-managed (PEP 668), so `uv pip install --system` (fine on 22.04's Python 3.10) now exits 2. Fix: `uv venv /opt/venv && uv pip install --python /opt/venv/bin/python --no-cache .`, plus `ENV VIRTUAL_ENV=/opt/venv` and `PATH=/opt/venv/bin:$GISBASE/bin:$GISBASE/scripts:$PATH`. The app image inherits the venv via ENV (the `ENTRYPOINT flask run` resolves via PATH, so venv `flask` is found); `test.yml`'s `uv pip install --system pytest` → `uv pip install --python /opt/venv/bin/python pytest` to match. **uv digest pin unchanged** (deliberate).

### CI (was 1.5)

Env workflow triggered; image built and pushed green to `ghcr.io/.../vf-accessmod-cli_env:main`.

### Stage 1 result

Env image green on `:main`: Dockerfile compiles, GRASS 8.4.2 builds, R 4.5.2 + packages install, venv uv install works, Python tests pass on the new `:main` env. **`tests/integration/test_grass_session.R` is Stage 2's input** — its status under GRASS 8 + `initGRASS()` is not yet verified.

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
- uv digest bump — pinned deliberately (the venv approach was the chosen PEP 668 fix; uv itself unchanged).
- `r-base` → `r-base-core` + `r-base-dev` swap — would drop the ~15 `r-recommended` r-cran-* packages that `r-base` hard-depends on but `requirements_r.txt` doesn't need. Offered, not done; safe follow-up if image size matters.
- `requirements_r.txt` format — space-separated, don't reformat (AGENTS.md).

## Risk summary

- **Highest risk (stage 2):** GRASS 7→8 `initGRASS`/`gmeta` behavior change breaking `init_session.R`. The image builds without it; `tests/integration/test_grass_session.R` in GHA is the gate.
- **Resolved (stage 1):** 26.04 apt soname/name drift — verified against the 26.04 Packages index and confirmed by the green build. Correct names were `libgdal38` (not `libgdal36`), `libicu78` (not `libicu80`), `libncurses6`, `libtiff6`, `libpng16-16t64`, `libjsoncpp26`. Two unplanned build fixes also landed: the FFTW3 migration and the PEP 668 uv venv (see Stage 1).
- **Low risk:** patched `r.reclass` API drift vs GRASS 8.4.2 — patches are full-file replacements using stable `Rast_*` APIs, so near-zero; if it fails, fast build-time feedback, regenerate the three files against 8.4.2 source.
- **Low risk:** R 4.5 vs 4.1 syntax in our own R (we don't use 4.5-only syntax; parse-check guards it).
- **Low risk:** GDAL 3.12 behavior diffs in any raster I/O our entrypoints do via `rgrass` (not `terra`/`sf` directly in the CLI path). Mitigation: integration test.
