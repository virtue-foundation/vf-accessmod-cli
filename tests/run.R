# Runner for R unit tests (testthat + covr).
# Usage: Rscript tests/run.R  (from repo root)
library(testthat)

test_dir("tests/testthat", reporter = "summary")

# Coverage report (covr installed in the env image; gracefully skipped otherwise)
if (requireNamespace("covr", quietly = TRUE)) {
  cat("\n")
  # Signal test file not to re-source functions.R (covr loads it with tracing)
  Sys.setenv(AM_COVR_RUNNING = "1")
  cov <- covr::file_coverage(
    "src/functions.R",
    "tests/testthat/test_helpers.R"
  )
  Sys.unsetenv("AM_COVR_RUNNING")
  print(cov)
}
