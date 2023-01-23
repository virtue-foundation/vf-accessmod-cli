require(rgrass)
require(tools)
require(jsonlite)
require(plyr)
require(magrittr)
require(stringr)
require(raster) # dev; for checking outputs in Rstudio

require(optparse)
# require(geojsonio)

config <- list()

#config$sepClass <- ""


config$gisBase = Sys.getenv(x = "GISBASE")
#setwd(dirname(dirname(parent.frame(2)$ofile)))
config$GrassDataBase <- Sys.getenv(x = "GISDBASE")
# config$pathCacheDir <- normalizePath("/data/cache/")
# config$pathGrassDemo <- normalizePath("config/data/demo")
config$pathDictDir <- normalizePath("dictionary/")
config$pathDictMain <- normalizePath(file.path(config$pathDictDir, "main.json"))
config$pathClasses <- file.path(config$pathDictDir, "classes.json")
config$dict <- fromJSON(config$pathDictMain)
config$dataClass <- fromJSON(config$pathClasses)
# get a version grouped by class with class id as key
config$dataClassList <- dlply(config$dataClass, .(class), c)

config$vector_key = "cat"

config$listTranspMod <- list(
  WALKING = list(rastVal = 1000),
  BICYCLING = list(rastVal = 2000),
  MOTORIZED = list(rastVal = 3000)
)

# character separator
config$sepTagFile <- "_"
config$sepClass <- "__"

grass_session_metadata <- tryCatch(gmeta(), error=function(e) NULL)
if (is.null(grass_session_metadata)) {
  print("Initializing GRASS session")
  initGRASS(gisBase = config$gisBase, gisDbase = config$GrassDataBase, override = T)
  print("Successfully initialized GRASS session")
} else print("GRASS session is already running")

source("functions.R")
print("Loaded all functions")
#source("mergeLandCover.R")
