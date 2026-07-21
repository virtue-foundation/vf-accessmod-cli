library(testthat)

# Stage 4 is designed to run only inside the env container where GRASS is installed.
# We gate this test so it is harmlessly skipped in non-GRASS environments.
skip_if(!nzchar(Sys.getenv("GISBASE")), "GRASS session not initialized (GISBASE not set)")

# Initialize the session as the entrypoints do
source("../../src/config.R")
source("../../src/init_session.R")

test_that("GRASS session is operational", {
  # These should return FALSE and not crash, proving the execGRASS wiring is intact.
  expect_false(amRastExists("definitely_not_a_real_map_123"),
    info = "amRastExists should return FALSE for non-existent raster"
  )

  expect_false(amVectExists("definitely_not_a_real_map_123"),
    info = "amVectExists should return FALSE for non-existent vector"
  )
})
