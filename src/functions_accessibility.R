
# Accessibility analysis auxiliary functions

source("functions.R")

########################## Defining analysis functions

#' amIsotropicTraveTime
#' @export
amIsotropicTravelTime <- function(
    inputFriction,
    inputHf,
    inputStop = NULL,
    inputCoord = NULL,
    outputDir = NULL,
    outputTravelTime = NULL,
    outputNearest = NULL,
    maxTravelTime = 0,
    maxSpeed = 0,
    minTravelTime = NULL,
    timeoutValue = -1L,
    getMemDiskRequirement = FALSE,
    ratioMemory = 1,
    memory = NULL, # if set, absolute max memory
    rawMode = FALSE,
    knights_move=F) {
  
  vInfo <- amParseOptions(
    execGRASS(
      "v.info",
      flags = c("t"),
      map = inputHf,
      intern = T
    )
  )
  
  vHasLines <- as.numeric(vInfo$lines) > 0
  tmpStart <- NULL
  if (vHasLines) {
    tmpStart <- amRandomName("tmp__raster_start")
    on_exit_add({
      rmRastIfExists(tmpStart)
    })
    suppressWarnings({
      execGRASS(
        "v.to.rast",
        input = inputHf,
        output = tmpStart,
        use = "val",
        value = 1
      )
    })
    inputRaster <- tmpStart
    inputHf <- NULL
  } else {
    inputRaster <- NULL
  }
  
  # default memory allocation
  free <- 300
  disk <- 2000
  # dynamic memory allocation
  tryCatch(
    {
      free <- sysEvalFreeMbMem()
    },
    error = function(cond) {
      warning(cond$message)
    }
  )
  
  tryCatch(
    {
      disk <- as.integer(sysEvalFreeMbDisk())
    },
    error = function(cond) {
      warning(cond$message)
    }
  )
  
  if (isEmpty(memory)) {
    memory <- as.integer(free * 0.8 * ratioMemory)
  }
  
  amParam <- list(
    input = inputFriction,
    output = outputTravelTime,
    nearest = outputNearest,
    start_points = inputHf,
    start_raster = inputRaster,
    start_coordinates = inputCoord,
    stop_points = inputStop,
    outdir = outputDir,
    max_cost = as.integer(maxTravelTime * 60), # max cost in sec
    memory = as.integer(memory)
  )
  
  amParam <- amParam[!sapply(amParam, isEmpty)]
  
  diskRequire <- disk
  memRequire <- free
  
  tryCatch(
    {
      testSysLimit <- execGRASS("r.cost",
                                parameters = amParam,
                                flags = c("i", "overwrite"),
                                intern = T
      )
      # Sample output
      # [1] "Will need at least 1.02 MB of disk space"
      # [2] "Will need at least 1.50 MB of memory"
      # [3] "16 of 16 segments are kept in memory"
      diskRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("disk space", testSysLimit)
          ]
        )
      )
      memRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("of memory", testSysLimit)
          ]
        )
      )
    },
    error = function(cond) {
      warning(cond$message)
    }
  )
  
  if (!getMemDiskRequirement && diskRequire > disk * 0.8) {
    stop(
      sprintf(
        "Insufficient disk space. Required= %1$s MB, Available= %2$s MB",
        diskRequire,
        disk
      )
    )
  }
  if (!getMemDiskRequirement && memRequire > free * 0.8) {
    stop(
      sprintf(
        "Insufficient memory. Required= %1$s MB, Available= %2$s MB",
        memRequire,
        free
      )
    )
  }
  
  # if (!getMemDiskRequirement) {
  #   amMsg(
  #     type = "log",
  #     text = sprintf(
  #       "Memory required for r.cost = %1$s MB. Memory available = %2$s MB. Disk space required = %3$s MB. Disk space available = %4$s MB",
  #       memRequire,
  #       free,
  #       diskRequire,
  #       disk
  #     )
  #   )
  # }
  
  if (!getMemDiskRequirement) {
    if (maxSpeed > 0 && maxTravelTime > 0) {
      on_exit_add({
        amSpeedBufferRegionRestore()
      })
      amSpeedBufferRegionInit(
        c(inputHf, inputStop),
        maxSpeed / 3.6,
        maxTravelTime * 60
      )
    }
    
    #
    # Remove stops if not on current region
    #
    if (!isEmpty(inputStop)) {
      tblStopTest <- amGetRasterValueAtPoint(
        inputStop,
        config$mapDem
      )
      hasNoStopInRegion <- isEmpty(tblStopTest)
      
      if (hasNoStopInRegion) {
        amParam$stop_points <- NULL
      }
    }
    flags = c("overwrite", "n")
    if(knights_move) {flags = c(flags, "k")}
    execGRASS("r.cost",
              parameters = amParam,
              flags=flags
    )
    
    if (!rawMode) {
      amCleanTravelTime(
        map = outputTravelTime,
        maxTravelTime = maxTravelTime,
        minTravelTime = minTravelTime,
        timeoutValue = timeoutValue,
        convertToMinutes = TRUE
      )
    }
  } else {
    return(
      list(
        required = list(
          memory = memRequire,
          disk = diskRequire
        ),
        available = list(
          memory = free,
          disk = disk
        )
      )
    )
  }
}

#' amAnisotropicTravelTime
#' @param maxTravelTime maximum cost in minute
#' @export
amAnisotropicTravelTime <- function(
    inputSpeed,
    inputHf,
    inputCoord = NULL,
    inputStop = NULL,
    outputDir = NULL,
    outputTravelTime = NULL,
    outputNearest = NULL,
    towardsFacilities = FALSE,
    maxTravelTime = 0,
    minTravelTime = NULL,
    maxSpeed = 0,
    timeoutValue = "null()",
    getMemDiskRequirement = FALSE,
    ratioMemory = 1,
    memory = NULL, # if set, absolute max memory
    rawMode = FALSE, # skip minute conversion; skip value removal above maxTravelTime
    knights_move = F
) {
  
  walk_fn <- "r.walk.accessmod"
  flags <- c("overwrite", "s")
  if(towardsFacilities) {flags = c(flags, "t")}
  if(knights_move) {flags = c(flags, "k")}
  flags <- flags[!flags %in% character(1)]
  
  # default memory allocation
  free <- 300
  disk <- 2000
  
  # dynamic memory allocation
  tryCatch(
    {
      free <- as.integer(sysEvalFreeMbMem())
    },
    error = function(cond) {
      warning(cond$message)
    }
  )

  tryCatch(
    {
      disk <- as.integer(sysEvalFreeMbDisk())
    },
    error = function(cond) {
      warning(cond$message)
    }
  )
  
  #
  # Convert vector line starting point to raster
  #
  vInfo <- amParseOptions(
    execGRASS(
      "v.info",
      flags = c("t"),
      map = inputHf,
      intern = T
    )
  )
  
  vHasLines <- as.numeric(vInfo$lines) > 0
  
  tmpStart <- NULL
  
  if (vHasLines) {
    tmpStart <- amRandomName("tmp__raster_start")
    on_exit_add({
      rmRastIfExists(tmpStart)
    })
    suppressWarnings({
      execGRASS(
        "v.to.rast",
        input = inputHf,
        output = tmpStart,
        use = "val",
        value = 1
      )
    })
    inputRaster <- tmpStart
    inputHf <- NULL
  } else {
    inputRaster <- NULL
  }
  
  #
  # set
  #
  if (isEmpty(memory)) {
    memory <- as.integer(free * 0.8 * ratioMemory)
  }
  
  amParam <- list(
    elevation = "r_dem",
    friction = inputSpeed,
    output = outputTravelTime,
    nearest = outputNearest,
    start_points = inputHf,
    start_raster = inputRaster,
    start_coordinates = inputCoord,
    stop_points = inputStop,
    outdir = outputDir,
    memory = as.integer(memory),
    max_cost = as.integer(maxTravelTime * 60) # max cost in seconds.
  )
  
  amParam <- amParam[!sapply(amParam, isEmpty)]
  
  diskRequire <- 0
  memRequire <- 0
  
  tryCatch(
    {
      testSysLimit <- execGRASS(walk_fn,
                                parameters = amParam,
                                flags = c("i", flags),
                                intern = T
      )
      # Sample output
      # [1] "Will need at least 1.02 MB of disk space"
      # [2] "Will need at least 1.50 MB of memory"
      # [3] "16 of 16 segments are kept in memory"
      diskRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("disk space", testSysLimit)
          ]
        )
      )
      memRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("of memory", testSysLimit)
          ]
        )
      )
    },
    error = function(cond) {
      warning(cond$message)
    }
  )
  
  if (!getMemDiskRequirement && diskRequire > disk * 0.8) {
    stop(
      sprintf(
        "Insufficient disk space. Required= %1$s MB, Available= %2$s MB",
        diskRequire,
        disk
      )
    )
  }
  if (!getMemDiskRequirement && memRequire > free * 0.8) {
    stop(
      sprintf(
        "Insufficient memory. Required= %1$s MB, Available= %2$s MB",
        memRequire,
        free
      )
    )
  }
  # 
  # if (!getMemDiskRequirement) {
  #   amMsg(
  #     type = "log",
  #     text = sprintf(
  #       "Memory required for r.walk.accessmod = %1$s MB. Memory available = %2$s MB. Disk space required = %3$s MB. Disk space available = %4$s MB",
  #       memRequire,
  #       free,
  #       diskRequire,
  #       disk
  #     )
  #   )
  # }
  
  if (!getMemDiskRequirement) {
    if (maxSpeed > 0 && maxTravelTime > 0) {
      on_exit_add({
        amSpeedBufferRegionRestore()
      })
      if (towardsFacilities) {
        amSpeedBufferRegionInit(
          c(inputHf, inputStop),
          maxSpeed / 3.6,
          maxTravelTime * 60
        )
      } else {
        amSpeedBufferRegionInit(
          c(inputHf),
          maxSpeed / 3.6,
          maxTravelTime * 60
        )
      }
    }
    
    #
    # Remove stops if not on current region
    #
    if (!isEmpty(inputStop)) {
      tblStopTest <- amGetRasterValueAtPoint(
        inputStop,
        config$mapDem
      )
      hasNoStopInRegion <- isEmpty(tblStopTest)
      
      if (hasNoStopInRegion) {
        amParam$stop_points <- NULL
      }
    }
    
    #
    # Launch analysis
    #
    execGRASS(walk_fn,
              parameters = amParam,
              flags = flags
    )
    
    if (!rawMode) {
      amCleanTravelTime(
        map = outputTravelTime,
        maxTravelTime = maxTravelTime,
        minTravelTime = minTravelTime,
        timeoutValue = timeoutValue,
        convertToMinutes = TRUE
      )
    }
  } else {
    return(
      list(
        required = list(
          memory = memRequire,
          disk = diskRequire
        ),
        available = list(
          memory = free,
          disk = disk
        )
      )
    )
  }
}

#################################


#' clean travel time map
#' @param map Raster travel time map
#' @param maxTravelTime Number. Maximum cost/travel time in minutes
#' @param minTravelTime Number. Minium cost/travel time in minutes
#' @param convertToMinutes Boolean. Convert the cleaned map to minutes
#' @param timeoutValue Number Integer to use as timeout remplacement value when maxTravelTime = 0
amCleanTravelTime <- function(map,
                              maxTravelTime = 0,
                              minTravelTime = NULL,
                              convertToMinutes = TRUE,
                              timeoutValue = "null()") {
  # remove over passed values :
  # r.walk check for over passed value after last cumulative cost :
  # so if a new cost is added and the new mincost is one step further tan
  # the thresold, grass will keep it and stop algorithm from there.
  
  int16Max <- (2^16) / 2 - 1
  int32Max <- (2^32) / 2 - 1
  unlimitedMode <- maxTravelTime == 0
  maxSeconds <- 0
  divider <- 1
  timeoutMinutesLimit <- 0
  timeoutMinutesValue <- timeoutValue
  cutSecondsStart <- 0
  cutSecondsEnd <- 0
  hasTimeout <- FALSE
  
  if (convertToMinutes) {
    divider <- 60
  }
  
  if (unlimitedMode) {
    timeoutMinutesLimit <- int16Max
    cutSecondsEnd <- timeoutMinutesLimit * divider
  } else {
    timeoutMinutesLimit <- int32Max
    timeoutMinutesValue <- "null()"
    cutSecondsEnd <- maxTravelTime * divider
  }
  
  if (isEmpty(minTravelTime)) {
    cutSecondsStart <- 0
  } else {
    cutSecondsStart <- minTravelTime * divider
  }
  
  #
  # NOTE mapcalc has a bug where value bigger than 2147483647 are NOT handled
  #
  
  cmd <- sprintf(
    " %1$s = %1$s >= %2$d && %1$s <= %3$d ? round((( %1$s / %6$f) - (( %1$s / %6$f ) %% 1))) : %1$s / %6$d > %4$d ? %5$s : null() ",
    map # 1
    , cutSecondsStart # 2
    , cutSecondsEnd # 3
    , timeoutMinutesLimit # 4
    , timeoutMinutesValue # 5
    , divider # 6
  )
  
  execGRASS(
    "r.mapcalc",
    expression = cmd,
    flags = c("overwrite")
  )
}

amCreateSpeedMap <- function(tbl, mapMerged, mapSpeed) {
  # creation of new classes for speed map (class+km/h), used in r.walk.accessmod
  # Exemples of rules:
  # oldClasses = newClasses \t newlabels
  # 1 2 3 = 1002 \t WALKING:2
  # 4 =  2020 \t BICYCLING:20
  # 1002 = 3080 \t MOTORIZED:80
  tbl[, "newClass"] <- integer()
  # for each row of the model table...
  for (i in 1:nrow(tbl)) {
    # ... get the mode
    mod <- tbl[i, "mode"]
    # ... corrsponding to the predefined value listTranspMod + given speed
    tbl[i, "newClass"] <- (
      as.integer(
        config$listTranspMod[[mod]]$rastVal
      ) + tbl[i, "speed"]
    ) * 1000
  }

  #
  # Ignore speed = 0 in reclass
  #
  tbl <- tbl[tbl$speed != 0, ]
  
  #
  # For all other classes, create a reclass
  #
  uniqueNewClass <- unique(tbl$newClass)
  reclassRules <- character()
  for (u in uniqueNewClass) {
    oldClasses <- tbl[tbl$newClass == u, "class"]
    modeSpeedLabel <- paste(
      tbl[
        tbl$newClass == u,
        c("mode", "speed")
      ][1, ],
      collapse = ":"
    )
    classRule <- paste(
      paste(
        oldClasses,
        collapse = " "
      ), "=",
      u,
      "\t",
      modeSpeedLabel
    )
    reclassRules <- c(reclassRules, classRule)
  }
  if(!dir.exists("temp")) {dir.create("temp")}
  tmpFile <- tempfile(tmpdir = "temp")
  write(reclassRules, tmpFile)
  #
  # Reclass the merged landcover
  #
  execGRASS("r.reclass",
            input = mapMerged,
            output = mapSpeed,
            rules = tmpFile,
            flags = "overwrite"
  )
  unlink("temp", recursive=T)
}

amCreateFrictionMap <- function(tbl, mapMerged, mapFriction, mapResol) {
  
  # creaction of new classes for cost map (seconds) used in r.cost.
  tbl[, "newClass"] <- numeric()
  tbl[, "mode"] <- "isotropic"
  
  #
  # Ignore speed = 0 in reclass
  #
  tbl <- tbl[tbl$speed != 0, ]
  
  
  # for each row of the model table...
  for (i in 1:nrow(tbl)) {
    # km/h to s/m
    # the time to cover one unit of distance * actual i
    # distance (map resolution) == cost to cross a given cell.
    tbl[i, "newClass"] <- (1 / (tbl[i, "speed"] / 3.6)) * mapResol
  }
  
  # unique new class
  uniqueNewClass <- unique(tbl$newClass)
  reclassRules <- character()
  categoryRules <- character()
  
  for (u in uniqueNewClass) {
    oldClasses <- tbl[tbl$newClass == u, "class"]

    modeSpeedLabel <- paste(
      tbl[
        tbl$newClass == u,
        c("mode", "speed")
      ][1, ],
      collapse = ":"
    )
    reclassRule <- paste0(oldClasses, ":", oldClasses, ":", u, ":", u)
    reclassRules <- c(reclassRules, reclassRule)
    catLabel <- paste(
      paste(tbl[tbl$newClass == u, ]$label, collapse = "/"),
      u, "[s]/", mapResol, "[m]"
    )
    categoryRule <- paste0(u, ":", catLabel)
    categoryRules <- c(categoryRules, categoryRule)
  }
  
  tmpFile <- tempfile()
  write(reclassRules, tmpFile)
  execGRASS("r.recode",
            input = mapMerged,
            output = mapFriction,
            rules = tmpFile,
            flags = "overwrite"
  )
  
  write(categoryRules, tmpFile)
  execGRASS("r.category",
            map = mapFriction,
            separator = ":",
            rules = tmpFile
  )
}

###### Facilities tools

amGetRasterValueAtPoint <- function(inputPoint, inputRaster) {
  data <- execGRASS("v.what.rast",
                    map = inputPoint,
                    raster = inputRaster,
                    flags = "p",
                    intern = T
  )
  
  if (isEmpty(data)) {
    tbl <- data.frame(V1 = character(0), v2 = character(0))
  } else {
    data <- data[]
    tbl <- read.table(
      text = data,
      sep = "|",
      stringsAsFactors = FALSE,
      na.strings = "*",
      colClasses = c("character"),
      fill=T
    )
    tbl
    tbl <- tbl[!is.na(as.numeric(tbl[[1]])) & !is.na(as.numeric(tbl[[2]])), ]
  }
  
  names(tbl) <- c("cat", "val")
  return(tbl)
}

amGetFacilitiesTableWhatRast <- function(mapHf, mapRaster) {
  
  # on_exit_add({
  #   amRegionReset()
  # })
  
  amRegionSet(mapRaster, mapHf)
  
  tbl <- amGetRasterValueAtPoint(mapHf, mapRaster)
  
  return(tbl)
}

amValidateFacilitiesTable <- function(tblHf, mapHf, mapMerged, mapPop = NULL, mapDem, tblSpeed) {
  # mapHf : vector map of facilities
  # map merged : raster landcover merged map
  # mapPop : raster map of population
  # Return value :
  # Facilities attribute table with additional columns :
  # amOnBarrier : check if facilities is located on barrier (no landcover value)
  # amOnZero : check if facilities is located on landcover cell with speed of zero
  # amCatLandCover : get value of merged land cover for each facilities.
  
  # ONLY IF POP MAP IS PRESENT
  # amPopCell : count population in cells where facilities are located.
  
  if (!amRastExists(mapMerged) || is.null(tblHf)) {
    return(NULL)
  }
  
  if (nrow(tblHf) == 0) {
    return(NULL)
  }
  #
  # check if HF are located on barrier by querying merged land cover values.
  #
  tbl <- amGetFacilitiesTableWhatRast(mapHf, mapMerged)
  names(tbl) <- c("cat", "amCatLandCover")
  tbl$amOnBarrier <- is.na(tbl$amCatLandCover)
  
  if (!isEmpty(tblSpeed)) {
    classWithZero <- tblSpeed[tblSpeed$speed == 0, ]$class
    tbl$amOnZero <- tbl$amCatLandCover %in% classWithZero
  } else {
    tbl$amOnZero <- "unset"
  }
  #
  # count population on facilities sites
  #
  if (!is.null(mapPop)) {
    tblPop <- amGetFacilitiesTableWhatRast(mapHf, mapPop)
    names(tblPop) <- c("cat", "amPopCell")
    tblPop[is.na(tblPop$amPopCell), "amPopCell"] <- 0
    #
    # merge results
    #
    tbl <- merge(tbl, tblPop, by = "cat")
  }
  
  #
  # Check DEM values
  #
  if (!is.null(mapDem)) {
    tblDem <- amGetFacilitiesTableWhatRast(mapHf, mapDem)
    names(tblDem) <- c("cat", "amDemValue")
    tblDem$amOutsideDem <- is.na(tblDem$amDemValue)
    #
    # merge results
    #
    tbl <- merge(tbl, tblDem, by = "cat")
  }
  
  #
  # merge accessmod table with attribute table
  #
  tbl <- merge(tbl, tblHf, by = "cat")
  
  return(tbl)
}
