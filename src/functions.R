clean_filepath <- function(path) {
  gsub("\\\\", "/", path)
}

amRastExists <- function(filter = "", mapset = NULL) {
  return(amLayerExists(filter, mapset, "raster"))
}

amVectExists <- function(filter = "", mapset = NULL) {
  return(amLayerExists(filter, mapset, "vector"))
}

amSubPunct <- function(vect,
                       sep = "_",
                       rmTrailingSep = T,
                       rmLeadingSep = T,
                       rmDuplicateSep = T,
                       debug = F) {
  # vect<-substr("'",'',iconv(vect, to='ASCII//TRANSLIT'))
  res <- sub("[[:punct:]]+|[[:blank:]]+", sep, vect) # replace punctuation by sep
  res <- sub("\n", "", res)
  if (rmDuplicateSep) {
    if (nchar(sep) > 0) {
      res <- sub(paste0("(\\", sep, ")+"), sep, res) # avoid duplicate
    }
  }
  if (rmLeadingSep) {
    if (nchar(sep) > 0) {
      res <- sub(paste0("^", sep), "", res) # remove trailing sep.
    }
  }
  if (rmTrailingSep) {
    if (nchar(sep) > 0) {
      res <- sub(paste0(sep, "$"), "", res) # remove trailing sep.
    }
  }
  res
}


amNewName <- function(class, tags, sepClass = config$sepClass, sepTag = config$sepTagFile) {
  tags <- paste(tags, collapse = sepTag)
  tags <- amSubPunct(tags, sepTag)
  paste0(c(class, tags), collapse = sepClass)
}
# amNewName('land_cover',c('test','2012'),"$","_")
# return:
# [1] "land_cover$test_2012"


#' Get data tag
#'
#' @param amData name vector or list to evaluate
#' @return tags
amGetTag <- function(amData, type = "ui") {
  if (type == "ui") {
    if (is.list(amData)) amData <- names(amData)
    tmp <- gsub(".+(?=\\[)|(\\[)|(\\])", "", amData, perl = T)
    tmp <- gsub("\\_", " ", tmp)
    tmp
  } else {
    tag <- unlist(strsplit(amData, paste0("\\", config$sepClass)))
    tag <- unlist(strsplit(tag, config$sepTagFile))
  }
}


# Get category table for a previously imported raster
amGetRasterCategory <- function(raster = NULL) {
  if (isEmpty(raster)) stop("No raster map name provided")
  
  tbl <- data.frame(integer(0), character(0))
  
  tblText <- execGRASS("r.category",
                       map = raster,
                       intern = T
  )
  
  # execGRASS returns a really awkward non-table thing, so we reformat it
  if (!isEmpty(tblText)) {
    tbl <- read.csv(
      text = tblText,
      sep = "\t",
      header = F,
      stringsAsFactors = F
    )
    if (ncol(tbl) == 2) {
      tbl[, 1] <- as.integer(tbl[, 1])
    }
  }
  names(tbl) <- c("class", "label")
  return(tbl)
}

#' Check for no data
#' @param val Vector to check
#' @export
amNoDataCheck <- function(val = NULL) {
  isTRUE(
    is.null(val)
  ) ||
    isTRUE(
      isTRUE(is.data.frame(val) && nrow(val) == 0) ||
        isTRUE(is.list(val) && (length(val) == 0)) ||
        isTRUE(!is.list(val) && is.vector(val) && (
          length(val) == 0 ||
            isTRUE(val[[1]] %in% config$defaultNoData) ||
            is.na(val[[1]]) ||
            nchar(val[[1]], allowNA = TRUE) == 0))
    )
}

isEmpty <- function(val = NULL) {
  amNoDataCheck(val)
}

rmIfExists <- function(name, type) {
  if(!is_loaded(name)) {return(NULL)}
  
  execGRASS("g.remove", flags = "f", parameters=list(name=name, type=type))
}

amGetTableFeaturesCount <- function(vect, types = c("areas", "lines", "points")) {
  if (!is_loaded(vect)) {
    return(data.frame(type = character(0), count = numeric(0)))
  }
  tbl = execGRASS("v.info", map    = vect, flags  = "t", intern = T) %>%
    amCleanTableFromGrass(
      sep = "=",
      col.names = c("type", "count")
    )
  tbl <- tbl[tbl$type %in% types, ]
  return(tbl)
}

#' Clean and read table from grass strings output
#'
#' @param {Character} text Grass text output
#' @param {Character} sep Character used as separator
#' @param {Logical} header Use first line as header
#' @param {Vector} cols Optional column selection
#' @return {data.frame}
#' @export
amCleanTableFromGrass <- function(text, sep = "|", header = TRUE, cols = NULL, ...) {
  tbl <- amSubQuote(text) %>%
    read.table(
      text = .,
      sep = sep,
      header = isTRUE(header),
      stringsAsFactor = FALSE,
      ...
    )
  
  if (!isEmpty(cols)) {
    tbl <- tbl[cols]
  }
  return(tbl)
}

#' amSubQuote
#'
#' Remove simple and double quote and newlines. This can be usefull in message
#' send to javascript functions, popup message, etc.. For complete removal of
#' non ascii character, use amSubPunct
#'
#' @param txt character vector
#' @export
amSubQuote <- function(txt) {
  txt <- gsub("\"", " ", txt)
  txt <- gsub("\'", " ", txt)
  txt <- gsub("\n", " ", txt)
  return(txt)
}

#' Get data class info
#' @param class Data class
#' @param value Value to retrieve, by default, language specific class
#' @export
amClassListInfo <- function(class = NULL, value = NULL) {
  vals <- c("type", "colors", "importable", "internal")
  # lang <- amTranslateGetSavedLanguage()
  res <- character(0)
  if (!is.null(class)) {
    for (i in class) {
      if (is.null(value)) {
        res <- c(res, config$dataClassList[[i]][[lang]])
      } else {
        # if (!value %in% vals) {
        #   amDebugMsg(paste("value must be in ", paste(vals, collapse = ";")))
        #   return()
        # }
        res <- c(res, config$dataClassList[[i]][[value]])
      }
    }
    return(res)
  }
}

rmLayerIfExists <- function(filter = "", type = c("vector", "raster")) {
  tryCatch(
    {
      if (isEmpty(filter)) {
        return()
      }
      filter <- paste(filter, collapse = ",")
      layerList <- execGRASS("g.list",
                             type = type,
                             pattern = filter,
                             intern = TRUE
      )
      if (length(layerList) > 0) {
        execGRASS("g.remove",
                  flags = c("b", "f"),
                  type = type,
                  pattern = paste0(filter, sep = "|")
        )
      }
    },
    error = function(e) {
      warning(e)
    }
  )
}
rmRastIfExists <- function(filter = "") {
  return(rmLayerIfExists(filter, "raster"))
}

rmVectIfExists <- function(filter = "", names = "") {
  return(rmLayerIfExists(filter, "vector"))
}

amLayerExists <- function(filter = "",mapset="",
                          type = c("raster", "vector")) {
  if (isEmpty(filter)) {
    return(FALSE)
  }
  tryCatch(
    {
      filter <- strsplit(filter, "@")[[1]][[1]]
      filter <- paste0(filter, "*")
      layers <- execGRASS("g.list",
                          type = type,
                          pattern = filter,
                          intern = TRUE
      )
      return(!isEmpty(layers))
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}

grass_print_info <- function(map, type=c("raster", "vector")) {
  info_fn = switch(type, raster="r.info", vector="v.info")
  execGRASS(info_fn, parameters=list(map=map))
}

# Import a new data object, with different parameters based on object type
import_layer <- function(path, type, layer_name, ignore_proj=F, overwrite=F) {
  
  import_fn = switch(type, raster="r.in.gdal", vector="v.in.ogr")
  
  # import parameters
  if(type=="raster") { #e.g., .tif
    import_parameters = list(input=path, output=layer_name)
  } else if (type=="vector") { # could be shapefile directory or .geojson
    if(is_a_dir(path)) { #shapefile directory
      filename = get_shapefile_dir(path)
      import_parameters = list(input=path, output=layer_name, layer=filename
                               #, snap=0.0001
                               )
    } else { # probably geojson
      import_parameters = list(input=path, output=layer_name)
    }
  }
  
  flags = c()
  if(ignore_proj) flags=c(flags, "o")
  if(overwrite) flags=c(flags, "overwrite")

  execGRASS(import_fn, flags=flags, parameters=import_parameters)
}

# Check if an object by a certain name already exists
is_loaded <- function(name, type = "all", overwrite=F) {
  if (overwrite) return(FALSE)
  available_data = execGRASS("g.list", parameters = list(type=type), flags = "t", intern=T) %>%
    read.csv(text=., sep="/", header=F, col.names = c("type", "name"))
  is_avail = name %in% available_data$name
  #this_type = available_data[available_data$name == name, "type"][[1]]
  #if (is_avail) print(paste0(this_type, " by name of ", name, " is already loaded"))
  return(is_avail)
}

is_a_dir <- function(path) {
  file_ext(path) == ""
}

add_to_stack <- function(obj, stack=NULL, back=F) {
  if (back) new_stack = c(stack, obj)
  else new_stack = c(obj, stack)
  return(new_stack)
}

get_shapefile_dir <- function(path) {
  file_path_sans_ext(list.files(path)[1])
}

# TO-DO: Could convert type/name notation into a df
list_all_loaded_objs <- function(print_type=F, type="all") {
  if(print_type) {flags="t"} else {flags=NULL}
  execGRASS("g.list", parameters = list(type=type), flags = flags, intern=T)
}

get_att_table <- function(map, cla_col="class", lab_col="label") {
  raw = execGRASS("db.select", sql=paste0("select distinct ", cla_col, ",", lab_col, " from ", map), intern=T)
  table = read.csv(text = raw, header=T, stringsAsFactors = F, sep="|") 
  return(table)
}

#' Parse scaling up coefficient options
#' @param opt String of option with paired argument separated by sepAssign, separated by given sepItem
#' @param sepAssign Character. Separator of assignement. Default is "="
#' @param sepItem Character. Separarator of items. Default is ";"
amParseOptions <- function(opt, sepItem = ";", sepAssign = "=") {
  optList <- list()
  if (!is.null(opt)) {
    opt <- unlist(strsplit(opt, sepItem))
    if (length(opt) > 0) {
      opt <- strsplit(opt, sepAssign)
      for (o in opt) {
        l <- length(o)
        optList[o[l - 1]] <- o[l]
      }
    }
  }
  return(optList)
}


# find  one cell diagonal bridge between multiple raster maps (e.g. road) and destination map (e.g. merged lcv)
# warning : only tested from rasterized lines with densified option.
amBridgeFinder <- function(fromMap, toMap, bridgeMap) {
  #
  # If the cell of one from map is not null
  #
  exprOneFromAsValue <- paste(
    sprintf(
      "!isnull(%1$s)",
      fromMap
    ),
    collapse = " || "
  )
  
  # Analyse diagonal value to extract bridge
  #
  # X=non-null cell in <road_map>; N=null in <merged_map>; A=non-null cell in <merged_map>
  # X will be set as null in fallowing cases:
  #
  # X N   N X   A N   N A
  # N A   A N   N X   X N
  #
  exprDiag <- sprintf("
    isnull(%1$s[0,-1]) &&
      !isnull(%1$s[1,-1]) &&
      isnull(%1$s[1,0]) ||

      isnull(%1$s[0,1]) &&
      !isnull(%1$s[1,1]) &&
      isnull(%1$s[1,0]) ||

      isnull(%1$s[-1,0]) &&
      !isnull(%1$s[-1,1]) &&
      isnull(%1$s[0,1]) ||

      isnull(%1$s[0,-1]) &&
      !isnull(%1$s[-1,-1]) &&
      isnull(%1$s[-1,0])
    ", toMap)
  
  exprBridge <- sprintf("if(%1$s,if(%2$s,1,null()),null())", exprOneFromAsValue, exprDiag)
  
  execGRASS("r.mapcalc",
            expression = sprintf(
              "%1$s=%2$s",
              bridgeMap,
              gsub("\\n", "", exprBridge)
            ),
            flags = "overwrite"
  )
  stat <- execGRASS("r.univar",
                    map = bridgeMap,
                    flags = "t",
                    intern = T
  ) %>%
    read.table(text = ., sep="|", header=T, stringsAsFactors = F, nrows = 1)
  
  nBridges <- stat[1, "non_null_cells"]
  print(paste("Found", nBridges, "bridges"))
}

# remove cell defined in bridgeMap from removeFromMap.
amBridgeRemover <- function(bridgeMap, removeFromMap) {
  tmpRules <- tempfile()
  write(execGRASS("r.category", map = removeFromMap, intern = T), tmpRules)
  expr <- paste0(removeFromMap, "=if(!isnull(", bridgeMap, "),null(),", removeFromMap, ")")
  execGRASS("r.mapcalc", expression = expr, flags = "overwrite")
  execGRASS("r.category", map = removeFromMap, rules = tmpRules)
  print(paste("Bridges from", bridgeMap, "removed from", removeFromMap))
}

#' amSubQuote
#'
#' Remove simple and double quote and newlines. This can be usefull in message
#' send to javascript functions, popup message, etc.. For complete removal of
#' non ascii character, use amSubPunct
#'
#' @param txt character vector
#' @export
amSubQuote <- function(txt) {
  txt <- gsub("\"", " ", txt)
  txt <- gsub("\'", " ", txt)
  txt <- gsub("\n", " ", txt)
  return(txt)
}

#' Clean and read table from grass strings output
#'
#' @param {Character} text Grass text output
#' @param {Character} sep Character used as separator
#' @param {Logical} header Use first line as header
#' @param {Vector} cols Optional column selection
#' @return {data.frame}
#' @export
amCleanTableFromGrass <- function(text, sep = "|", header = TRUE, cols = NULL, ...) {
  tbl <- amSubQuote(text) %>%
    read.table(
      text = .,
      sep = sep,
      header = isTRUE(header),
      stringsAsFactor = FALSE,
      ...
    )
  
  if (!isEmpty(cols)) {
    tbl <- tbl[cols]
  }
  return(tbl)
}

#' Compose random char name
#' @param prefix Prefix of the resulting string
#' @param suffix Suffix of the resultiing string
#' @param n Number of random letters
#' @param collapse Character to join strings
#' @return String with random letters
#' @export
amRandomName <- function(prefix = NULL, suffix = NULL, n = 20, cleanString = FALSE, collapse = "_") {
  if (cleanString) {
    prefix <- amSubPunct(prefix, "_")
    suffix <- amSubPunct(suffix, "_")
  }
  rStr <- paste(letters[round(runif(n) * 24)], collapse = "")
  str <- c(prefix, rStr, suffix)
  paste(str, collapse = collapse)
}

#' @param tableFacilities Facilities table with config$vectorKey attr
#' @param inputFacilities Facilities layer name
#' @return Name of the final facility layer
amFacilitiesSubset <- function(tableFacilities, inputFacilities, select_col) {
  #
  # WORKAROUND for solving the issue #209
  # That produced a "Argument list to long in v.extract"
  # The error visible was "Cannot open connection", but it's
  # unrelated to the actual error.
  # Strategy :
  # Using smallest subset OR if all selected, don't extract
  
  if(grepl("-", select_col) | grepl("^_", select_col)) {
    select_col <- sub("-", "_", select_col)
    select_col <- sub("^_", "x", select_col)
    print(paste("Reformatted subset column name to", select_col))
  }
  is_valid_select_col <- select_col %in% names(tableFacilities)
  if(!is_valid_select_col) stop("Choose a valid subset column")

  idHfAll <- tableFacilities[[config$vector_key]]
  idHfSelect <- tableFacilities[tableFacilities[select_col]==1, config$vector_key]
  fName <- amRandomName("tmp__")
  idHfNotSelect <- idHfAll[!idHfAll %in% idHfSelect]
  hasMoreSelect <- length(idHfNotSelect) < length(idHfSelect)
  hasAllSelect <- identical(idHfSelect, idHfAll)
  inputHfFinal <- ifelse(hasAllSelect, inputFacilities, fName)
  
  if (!hasAllSelect) {
    if (hasMoreSelect) {
      qSql <- paste0(config$vector_key, " NOT IN ( ", paste0("'", idHfNotSelect, "'", collapse = ","), " )")
    } else {
      qSql <- sprintf(
        " %1$s IN ( %2$s )",
        config$vector_key,
        paste0("'", idHfSelect, "'", collapse = ",")
      )
    }
    
    print("Subsetting the facilities table using the following query:")
    print(qSql)
    
    #
    # Create a temporary copy
    #

    execGRASS(
      "v.extract",
      flags = "overwrite",
      input = inputFacilities,
      where = qSql,
      output = inputHfFinal
    )
  }
  
  return(inputHfFinal)
}




#' Evaluate disk space available
#' @return disk space available in MB
sysEvalFreeMbDisk <- function() {
  # free <- system('df --output=avail -BM "$PWD" | sed "1d;s/[^0-9]//g"',intern=T)
  # Alpine
  #                                                  * - > $4
  # Filesystem           1M-blocks      Used Available Use% Mounted on
  # overlay                 120695    117784         0 100% /
  free <- system("df -BM $GISDBASE | tail -n1 | awk '{print $4}'", intern = T)
  free <- gsub("\\D+", "", free)
  return(as.integer(free))
}

#' Evaluate disk space total
#' @return disk space available in MB
sysEvalSizeMbDisk <- function() {
  # free <- system('df --output=size -BM "$PWD" | sed "1d;s/[^0-9]//g"',intern=T)
  #
  # Alpine
  #                                        * -> $3
  # Filesystem           1M-blocks      Used Available Use% Mounted on
  # overlay                 120695    117784         0 100% /
  used <- system("df -BM $GISDBASE | tail -n1 | awk '{print $3}'", intern = T)
  return(as.integer(used))
}

#' Evalutate memory available. This is experimental
#' @return Available memory in MB
sysEvalFreeMbMem <- function() {
  sys <- Sys.info()["sysname"]
  free <- 300
  
  switch(sys,
         "Darwin" = {
           memTot <- as.integer(system("sysctl hw.memsize | awk '{ print $2 / (2^10)^2}'", intern = T))
           memActive <- as.integer(system("vm_stat | awk '/^Pages active/ { print ($3 * 4096) / (2^10)^2}'", intern = T))
           memFree <- as.integer(system("vm_stat | awk '/^Pages free/ { print ($3 * 4096) / (2^10)^2}'", intern = T))
           memPurgeable <- as.integer(system("vm_stat | awk '/^Pages purgeable/ { print ($3 * 4096) / (2^10)^2}'", intern = T))
           
           free <- memTot - memActive
         },
         "Linux" = {
           memTot <- as.integer(system("cat /proc/meminfo | awk '/^MemTotal:/ {print $2/ (2^10)}'", intern = T))
           memActive <- as.integer(system("cat /proc/meminfo | awk '/^Active:/ {print $2/ (2^10)}'", intern = T))
           memFree <- as.integer(system("cat /proc/meminfo | awk '/^MemFree:/ {print $2/ (2^10)}'", intern = T))
           memCached <- as.integer(system("cat /proc/meminfo | awk '/^Cached:/ {print $2/(2^10)}'", intern = T))
           
           free <- memTot - memActive
         }
  )
  
  return(as.integer(free))
}

#' Reset AccessMod region
#' @param {Character} rasters Rasters to set the region
#' @param {Character} vectors vectors to set the region
amRegionSet <- function(rasters = character(0), vectors = character(0)) {
  
  hasRasters <- !amRastExists(rasters)
  hasVectors <- !amVectExists(vectors)
  
  if (!hasRasters && !hasVectors) {
    warnings("amRegionSet : no layer available to update region")
    return
  }
  print("Setting region using the following:")
  print(rasters)
  print(vectors)
  print("current region")
  execGRASS("g.region", flags="p")
  print("new region")
  execGRASS("g.region",
            raster = rasters,
            vector = vectors,
            #align = config$mapDem,
            flags = c("p")
  )
}

####################################### ACCESSIBILITY


# Accessibility analysis auxiliary functions

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
  
  amRegionSet(mapRaster, mapHf)
  tbl <- amGetRasterValueAtPoint(mapHf, mapRaster)
  
  amRegionReset()
  return(tbl)
}

amRegionReset <- function() {
  amRegionSet(
    rasters = config$mapDem
  )
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
    tblPop <- amGetFacilitiesTableWhatRast(map, mapPop)
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


check_region <- function(prev_region=NULL, tag=NULL) {
  current_region <- execGRASS("g.region", flags="p", intern=T)
  print(current_region)
  print(paste("REGION TAG:  ", tag))
}

#' Import temporary shapefile catchment to final directory
#' @param shpFile Full path to temp catchment file . eg. /tmp/super.shp
#' @param outDir Directory path where are stored shapefile. eg. /home/am/data/shapefiles/
#' @param outName Name of the final catchment shapefile, without extension. e.g. catchments_001
#' @return Boolean Done
amMoveShp <- function(shpFile, outDir, outName) {
  #
  # Collect all shp related file and copy them to final directory.
  # NOTE: make sure that:
  # - pattern of shapefile is unique in its directory
  
  # in case of variable in path, convert outdir to fullpath
  if (length(shpFile) < 1) {
    return()
  }
  outDir <- system(sprintf("echo %s", outDir), intern = T)
  
  fe <- file.exists(shpFile)
  de <- dir.exists(outDir)
  so <- isTRUE(grep(".*\\.geojson$", shpFile) > 0)
  
  if (!fe) {
    warning(
      sprintf("amMoveShp: %s input file does not exists", shpFile)
    )
  }
  if (!de) {
    warning(
      sprintf("amMoveShp: %s output directory does not exists", outDir)
    )
  }
  if (!so) {
    warning(
      sprintf("amMoveShp: %s input file does not have .shp extension", shpFile)
    )
  }
  
  ok <- c(fe, de, so)
  
  if (all(ok)) {
    # base name file for pattern.
    baseShape <- gsub(".geojson", "", basename(shpFile))
    # list files (we can also use )
    allShpFiles <- list.files(dirname(shpFile), pattern = paste0("^", baseShape), full.names = TRUE)
    # Copy each files in final catchment directory.
    for (s in allShpFiles) {
      sExt <- file_ext(s)
      newPath <- file.path(outDir, paste0(outName, ".", sExt))
      file.copy(s, newPath, overwrite = T)
    }
  }
  return(all(ok))
}

debug_header <- function(text) {
  print(paste("######################", text))
}

debug_raster_report <- function(map) {
  report <- execGRASS("r.report", map=map, units=c("k","c", "p"), intern=T)
  write.table(report,
              file = paste0(map, "_report.txt"),
              row.names = F, quote=FALSE)
}