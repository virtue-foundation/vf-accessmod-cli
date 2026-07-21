# Unit tests for pure R helper functions in src/functions.R
# Does NOT require a GRASS session — sources functions.R directly
# with a minimal config stub for functions that reference config.
#
# Usage: Rscript tests/run.R  (from repo root)

# Minimal config stub for functions that reference config at call time.
# (R's lazy evaluation means sourcing functions.R doesn't need this,
# only the test calls that exercise those branches do.)
config <- list(
  defaultNoData = NULL,
  dataClassList = list(
    "1" = list(
      type = "raster", colors = "#000000", importable = TRUE, internal = FALSE,
      en = "Raster layer"
    ),
    "2" = list(
      type = "vector", colors = "#FFFFFF", importable = TRUE, internal = FALSE,
      en = "Vector layer"
    )
  )
)
lang <- "en"

# Source functions.R so its functions are available to tests.
# When running under covr::file_coverage(), the source file is already
# pre-loaded with tracing — the runner sets AM_COVR_RUNNING to skip here.
if (!nzchar(Sys.getenv("AM_COVR_RUNNING", ""))) {
  # test_dir() sets wd to tests/testthat/, so ../../src/functions.R
  # works. The runner also sets AM_SRC_PATH for when it doesn't.
  src_path <- Sys.getenv("AM_SRC_PATH", unset = "../../src/functions.R")
  source(src_path, local = TRUE)
}

# ---- clean_filepath ----

test_that("clean_filepath converts backslashes to forward slashes", {
  expect_equal(clean_filepath("a\\b\\c"), "a/b/c")
})

test_that("clean_filepath leaves forward slashes unchanged", {
  expect_equal(clean_filepath("a/b/c"), "a/b/c")
})

test_that("clean_filepath handles mixed slashes and empty string", {
  expect_equal(clean_filepath(""), "")
  expect_equal(clean_filepath("a\\b/c"), "a/b/c")
})

# ---- amSubPunct ----

test_that("amSubPunct replaces punctuation and blanks with sep", {
  expect_equal(amSubPunct("hello.world"), "hello_world")
  expect_equal(amSubPunct("hello world"), "hello_world")
})

test_that("amSubPunct handles duplicate, leading, and trailing sep", {
  expect_equal(amSubPunct("hello..world"), "hello_world")
  expect_equal(amSubPunct("_hello"), "hello")
  expect_equal(amSubPunct("hello_"), "hello")
})

test_that("amSubPunct preserves when no punctuation", {
  expect_equal(amSubPunct("hello"), "hello")
  expect_equal(amSubPunct(""), "")
})

test_that("amSubPunct respects rm* flags", {
  expect_equal(amSubPunct("_hello_", rmLeadingSep = FALSE, rmTrailingSep = FALSE), "_hello_")
  # rmDuplicateSep=FALSE + rm*Sep=TRUE: both underscores stripped
  expect_equal(amSubPunct("_hello_", rmDuplicateSep = FALSE, rmLeadingSep = TRUE, rmTrailingSep = TRUE), "hello")
})

test_that("amSubPunct with custom sep", {
  expect_equal(amSubPunct("hello.world", sep = "-"), "hello-world")
})

test_that("amSubPunct removes newlines without replacement", {
  # sub("\n", "", ...) removes newlines, does not replace with sep
  expect_equal(amSubPunct("hello\nworld"), "helloworld")
})

# ---- amSubQuote ----

test_that("amSubQuote removes double quotes", {
  expect_equal(amSubQuote('hello"world'), "hello world")
})

test_that("amSubQuote removes single quotes", {
  expect_equal(amSubQuote("hello'world"), "hello world")
})

test_that("amSubQuote replaces newlines", {
  expect_equal(amSubQuote("hello\nworld"), "hello world")
})

test_that("amSubQuote handles mixed quotes and clean strings", {
  expect_equal(amSubQuote("hello world"), "hello world")
  expect_equal(amSubQuote("\"'\"\n"), "    ")
})

# ---- amParseOptions ----

test_that("amParseOptions parses normal string", {
  res <- amParseOptions("key1=val1;key2=val2")
  expect_equal(res$key1, "val1")
  expect_equal(res$key2, "val2")
})

test_that("amParseOptions handles single pair and NULL", {
  res <- amParseOptions("a=1")
  expect_equal(res$a, "1")
  expect_equal(amParseOptions(NULL), list())
})

test_that("amParseOptions handles empty string", {
  res <- amParseOptions("")
  expect_equal(res, list())
})

test_that("amParseOptions handles missing values", {
  # NB: R's strsplit drops trailing empty strings, so "a=" yields only "a"
  # with length 1, meaning the "a" entry is silently lost (existing behavior).
  res <- amParseOptions("a=;b=2")
  expect_equal(res$b, "2")
  expect_null(res$a)
})

test_that("amParseOptions supports custom separators", {
  res <- amParseOptions("a:1,b:2", sepItem = ",", sepAssign = ":")
  expect_equal(res$a, "1")
  expect_equal(res$b, "2")
})

# ---- amRandomName ----

test_that("amRandomName produces a non-empty string", {
  name <- amRandomName(n = 10)
  expect_true(nchar(name) > 0)
})

# NB: amRandomName currently uses round(runif(n) * 24) which can
# hit index 0 (~2%/pos), dropping a char.  The test above only
# checks non-empty.  If exact n-char is needed, change to
# sample(letters, n, replace = TRUE).

test_that("amRandomName prepends prefix and appends suffix", {
  name <- amRandomName(prefix = "pre", suffix = "suf", n = 5)
  expect_true(grepl("^pre_", name))
  expect_true(grepl("_suf$", name))
})

test_that("amRandomName with cleanString calls amSubPunct", {
  name <- amRandomName(prefix = "bad.", suffix = "bad.", n = 5, cleanString = TRUE)
  expect_true(grepl("^bad_", name))
  expect_true(grepl("_bad$", name))
})

# ---- amCleanTableFromGrass ----
# NB: amSubQuote replaces newlines with spaces, so multi-line text cannot be
# passed as a single string with \n. Pass a character vector instead (matching
# real usage: execGRASS(..., intern=TRUE) returns a character vector).

test_that("amCleanTableFromGrass parses pipe-delimited text with header", {
  txt <- c("name|value", "a|1", "b|2")
  tbl <- amCleanTableFromGrass(txt)
  expect_s3_class(tbl, "data.frame")
  expect_equal(nrow(tbl), 2)
  expect_equal(ncol(tbl), 2)
  expect_equal(tbl$name[1], "a")
})

test_that("amCleanTableFromGrass parses without header", {
  txt <- c("a|1", "b|2")
  tbl <- amCleanTableFromGrass(txt, header = FALSE)
  expect_equal(ncol(tbl), 2)
})

test_that("amCleanTableFromGrass supports column subset", {
  txt <- c("name|value|extra", "a|1|x", "b|2|y")
  tbl <- amCleanTableFromGrass(txt, cols = c("name", "value"))
  expect_equal(ncol(tbl), 2)
})

test_that("amCleanTableFromGrass errors on empty string (no lines available)", {
  expect_error(amCleanTableFromGrass(""))
})

# ---- amNoDataCheck / isEmpty ----

test_that("amNoDataCheck returns TRUE for NULL, empty df, empty list", {
  expect_true(amNoDataCheck(NULL))
  expect_true(amNoDataCheck(data.frame()))
  expect_true(amNoDataCheck(list()))
})

test_that("amNoDataCheck returns FALSE for non-empty values", {
  expect_false(amNoDataCheck(42))
  expect_false(amNoDataCheck("hello"))
  expect_false(amNoDataCheck(data.frame(x = 1:3)))
})

test_that("isEmpty delegates to amNoDataCheck", {
  expect_true(isEmpty(NULL))
  expect_false(isEmpty(42))
})

# ---- amClassListInfo ----

test_that("amClassListInfo retrieves class info", {
  res <- amClassListInfo("1")
  expect_true(length(res) > 0)
})

test_that("amClassListInfo returns NULL for NULL class", {
  # No else branch; the last expression (if) evaluates to NULL when FALSE
  expect_null(amClassListInfo(NULL))
})
