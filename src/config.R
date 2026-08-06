require(rgrass)
require(terra)
require(sf)
require(tools)
require(jsonlite)
require(DBI)

require(optparse)

config <- list()

config$gisBase <- Sys.getenv(x = "GISBASE")
config$GrassDataBase <- Sys.getenv(x = "GISDBASE")
config$pathDictDir <- normalizePath("dictionary/")
config$pathClasses <- file.path(config$pathDictDir, "classes.json")
config$dataClass <- fromJSON(config$pathClasses)
# get a version grouped by class with class id as key
config$dataClassList <- split(config$dataClass, config$dataClass$class)

config$vectorKey <- "cat"

config$listTranspMod <- list(
  WALKING = list(rastVal = 1000),
  BICYCLING = list(rastVal = 2000),
  MOTORIZED = list(rastVal = 3000)
)

config$mapDem <- "r_dem"

source("functions.R")
print("Loaded all functions")
