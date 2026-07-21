require(rgrass)
require(tools)
require(jsonlite)
require(DBI)

require(optparse)
# require(geojsonio)

config <- list()

# config$sepClass <- ""


config$gisBase <- Sys.getenv(x = "GISBASE")
# setwd(dirname(dirname(parent.frame(2)$ofile)))
config$GrassDataBase <- Sys.getenv(x = "GISDBASE")
# config$pathCacheDir <- normalizePath("/data/cache/")
# config$pathGrassDemo <- normalizePath("config/data/demo")
config$pathDictDir <- normalizePath("dictionary/")
config$pathDictMain <- normalizePath(file.path(config$pathDictDir, "main.json"))
config$pathClasses <- file.path(config$pathDictDir, "classes.json")
config$dict <- fromJSON(config$pathDictMain)
config$dataClass <- fromJSON(config$pathClasses)
# get a version grouped by class with class id as key
config$dataClassList <- split(config$dataClass, config$dataClass$class)

config$vector_key <- "cat"

config$listTranspMod <- list(
  WALKING = list(rastVal = 1000),
  BICYCLING = list(rastVal = 2000),
  MOTORIZED = list(rastVal = 3000)
)

config$mapDem <- "r_dem"

# character separator
config$sepTagFile <- "_"
config$sepClass <- "__"

source("functions.R")
print("Loaded all functions")
# source("mergeLandCover.R")
