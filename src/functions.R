clean_filepath <- function(path) {
  gsub("\\\\", "/", path)
}

# Names of required options whose parsed value is NULL (i.e. not supplied on
# the CLI). optparse::parse_args() always returns every defined option name,
# so presence in names(opt) is not a valid missing-check; the value must be
# tested instead.
amMissingOpts <- function(required, opt) {
  required[vapply(required, function(n) is.null(opt[[n]]), logical(1))]
}

amRastExists <- function(filter = "", mapset = NULL) {
  amLayerExists(filter, mapset, "raster")
}

amVectExists <- function(filter = "", mapset = NULL) {
  amLayerExists(filter, mapset, "vector")
}

amSubPunct <- function(vect,
                       sep = "_",
                       rmTrailingSep = TRUE,
                       rmLeadingSep = TRUE,
                       rmDuplicateSep = TRUE,
                       debug = FALSE) {
  res <- gsub("[[:punct:]]+|[[:blank:]]+", sep, vect) # replace punctuation by sep
  res <- gsub("\n", "", res)
  if (rmDuplicateSep) {
    if (nchar(sep) > 0) {
      res <- gsub(paste0("(\\", sep, ")+"), sep, res) # avoid duplicate
    }
  }
  if (rmLeadingSep) {
    if (nchar(sep) > 0) {
      res <- sub(paste0("^", sep), "", res) # remove leading sep.
    }
  }
  if (rmTrailingSep) {
    if (nchar(sep) > 0) {
      res <- sub(paste0(sep, "$"), "", res) # remove trailing sep.
    }
  }
  res
}

#' Check for no data
#' @param val Vector to check
#' @export
amNoDataCheck <- function(val = NULL) {
  if (isTRUE(is.null(val))) {
    return(TRUE)
  }
  if (isTRUE(is.data.frame(val)) && nrow(val) == 0) {
    return(TRUE)
  }
  if (isTRUE(is.list(val)) && length(val) == 0) {
    return(TRUE)
  }
  if (!is.list(val) && is.vector(val)) {
    if (length(val) == 0) {
      return(TRUE)
    }
    v1 <- val[[1]]
    if (is.na(v1) || nchar(v1, allowNA = TRUE) == 0) {
      return(TRUE)
    }
  }
  FALSE
}

isEmpty <- function(val = NULL) {
  amNoDataCheck(val)
}

rmIfExists <- function(name, type) {
  if (!is_loaded(name)) {
    return(NULL)
  }

  execGRASS("g.remove", flags = "f", parameters = list(name = name, type = type))
}

amGetTableFeaturesCount <- function(vect, types = c("areas", "lines", "points")) {
  if (!is_loaded(vect)) {
    return(data.frame(type = character(0), count = numeric(0)))
  }
  tbl <- execGRASS("v.info", map = vect, flags = "t", intern = TRUE) |>
    amCleanTableFromGrass(
      sep = "=",
      col.names = c("type", "count")
    )
  tbl <- tbl[tbl$type %in% types, ]
  tbl
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
  clean <- amSubQuote(text)
  tbl <- read.table(
    text = clean,
    sep = sep,
    header = isTRUE(header),
    stringsAsFactor = FALSE,
    ...
  )

  if (!isEmpty(cols)) {
    tbl <- tbl[cols]
  }
  tbl
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
  txt
}

#' Get data class info
#' @param class Data class
#' @param value Value to retrieve (e.g. "type", "colors")
#' @export
amClassListInfo <- function(class = NULL, value = NULL) {
  res <- character(0)
  if (!is.null(class)) {
    for (i in class) {
      res <- c(res, config$dataClassList[[i]][[value]])
    }
    res
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
          pattern = filter
        )
      }
    },
    error = function(e) {
      warning(e)
    }
  )
}
rmRastIfExists <- function(filter = "") {
  rmLayerIfExists(filter, "raster")
}

rmVectIfExists <- function(filter = "", names = "") {
  rmLayerIfExists(filter, "vector")
}

amLayerExists <- function(filter = "", mapset = "",
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
      FALSE
    }
  )
}

grass_print_info <- function(map, type = c("raster", "vector")) {
  info_fn <- switch(type,
    raster = "r.info",
    vector = "v.info"
  )
  execGRASS(info_fn, parameters = list(map = map))
}

# Import a new data object, with different parameters based on object type
import_layer <- function(path, type, layer_name, ignore_proj = FALSE, overwrite = FALSE) {
  import_fn <- switch(type,
    raster = "r.in.gdal",
    vector = "v.in.ogr"
  )

  # import parameters
  if (type == "raster") { # e.g., .tif
    import_parameters <- list(input = path, output = layer_name)
  } else if (type == "vector") { # .geojson
    import_parameters <- list(input = path, output = layer_name)
  }

  flags <- c()
  if (ignore_proj) flags <- c(flags, "o")
  if (overwrite) flags <- c(flags, "overwrite")

  execGRASS(import_fn, flags = flags, parameters = import_parameters)
}

# Check if an object by a certain name already exists
is_loaded <- function(name, type = "all", overwrite = FALSE) {
  if (overwrite) {
    return(FALSE)
  }
  grass_list <- execGRASS("g.list", parameters = list(type = type), flags = "t", intern = TRUE)
  available_data <- read.csv(text = grass_list, sep = "/", header = FALSE, col.names = c("type", "name"))
  is_avail <- name %in% available_data$name
  # this_type = available_data[available_data$name == name, "type"][[1]]
  # if (is_avail) print(paste0(this_type, " by name of ", name, " is already loaded"))
  is_avail
}

add_to_stack <- function(obj, stack = NULL, back = FALSE) {
  if (back) {
    new_stack <- c(stack, obj)
  } else {
    new_stack <- c(obj, stack)
  }
  new_stack
}

# TO-DO: Could convert type/name notation into a df
list_all_loaded_objs <- function(print_type = FALSE, type = "all") {
  if (print_type) {
    flags <- "t"
  } else {
    flags <- NULL
  }
  execGRASS("g.list", parameters = list(type = type), flags = flags, intern = TRUE)
}

get_att_table <- function(map, cla_col = "class", lab_col = "label") {
  raw <- execGRASS("db.select", sql = paste0("select distinct ", cla_col, ",", lab_col, " from ", map), intern = TRUE)
  table <- read.csv(text = raw, header = TRUE, stringsAsFactors = FALSE, sep = "|")
  table
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
  optList
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
  univar_out <- execGRASS("r.univar",
    map = bridgeMap,
    flags = "t",
    intern = TRUE
  )
  stat <- read.table(text = univar_out, sep = "|", header = TRUE, stringsAsFactors = FALSE, nrows = 1)

  nBridges <- stat[1, "non_null_cells"]
  print(paste("Found", nBridges, "bridges"))
}

# remove cell defined in bridgeMap from removeFromMap.
amBridgeRemover <- function(bridgeMap, removeFromMap) {
  tmpRules <- tempfile()
  write(execGRASS("r.category", map = removeFromMap, intern = TRUE), tmpRules)
  expr <- paste0(removeFromMap, "=if(!isnull(", bridgeMap, "),null(),", removeFromMap, ")")
  execGRASS("r.mapcalc", expression = expr, flags = "overwrite")
  execGRASS("r.category", map = removeFromMap, rules = tmpRules)
  print(paste("Bridges from", bridgeMap, "removed from", removeFromMap))
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
  rStr <- paste(sample(letters, n, replace = TRUE), collapse = "")
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

  if (grepl("-", select_col) || grepl("^_", select_col)) {
    select_col <- sub("-", "_", select_col)
    select_col <- sub("^_", "x", select_col)
    print(paste("Reformatted subset column name to", select_col))
  }
  is_valid_select_col <- select_col %in% names(tableFacilities)
  if (!is_valid_select_col) stop("Choose a valid subset column")

  idHfAll <- tableFacilities[[config$vectorKey]]
  idHfSelect <- tableFacilities[tableFacilities[select_col] == 1, config$vectorKey]
  fName <- amRandomName("tmp__")
  idHfNotSelect <- idHfAll[!idHfAll %in% idHfSelect]
  hasMoreSelect <- length(idHfNotSelect) < length(idHfSelect)
  hasAllSelect <- identical(idHfSelect, idHfAll)
  inputHfFinal <- ifelse(hasAllSelect, inputFacilities, fName)

  if (!hasAllSelect) {
    #
    # Remove NA category values — they can't be used in a SQL IN/NOT IN clause
    # and the rgrass7 execGRASS function passes the clause through system()
    # without shell-escaping, so parentheses and other metacharacters must be
    # double-quoted to prevent sh: Syntax error.
    #
    idHfSelect <- idHfSelect[!is.na(idHfSelect)]
    idHfNotSelect <- idHfNotSelect[!is.na(idHfNotSelect)]

    # Guard against an empty IN list, which would build invalid SQL
    # ("cat IN ()") and crash v.extract. Checked after NA removal so a
    # selection whose ids are all NA is caught too.
    if (length(idHfSelect) == 0) {
      stop("No facilities selected in the subset column")
    }

    if (hasMoreSelect) {
      qSql <- sprintf(
        '"%s NOT IN (%s)"',
        config$vectorKey,
        paste0("'", idHfNotSelect, "'", collapse = ",")
      )
    } else {
      qSql <- sprintf(
        '"%s IN (%s)"',
        config$vectorKey,
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

  inputHfFinal
}


#' Evaluate disk space available
#' @return disk space available in MB
sysEvalFreeMbDisk <- function() {
  # free <- system('df --output=avail -BM "$PWD" | sed "1d;s/[^0-9]//g"',intern=T)
  # Alpine
  #                                                  * - > $4
  # Filesystem           1M-blocks      Used Available Use% Mounted on
  # overlay                 120695    117784         0 100% /
  free <- system("df -BM $GISDBASE | tail -n1 | awk '{print $4}'", intern = TRUE)
  free <- gsub("\\D+", "", free)
  as.integer(free)
}


#' Evalutate memory available. This is experimental
#' @return Available memory in MB
sysEvalFreeMbMem <- function() {
  sys <- Sys.info()["sysname"]
  free <- 300

  switch(sys,
    "Darwin" = {
      memTot <- as.integer(system("sysctl hw.memsize | awk '{ print $2 / (2^10)^2}'", intern = TRUE))
      memActive <- as.integer(system("vm_stat | awk '/^Pages active/ { print ($3 * 4096) / (2^10)^2}'", intern = TRUE))
      memFree <- as.integer(system("vm_stat | awk '/^Pages free/ { print ($3 * 4096) / (2^10)^2}'", intern = TRUE))
      memPurgeable <- as.integer(system(
        "vm_stat | awk '/^Pages purgeable/ { print ($3 * 4096) / (2^10)^2}'",
        intern = TRUE
      ))

      free <- memTot - memActive
    },
    "Linux" = {
      memTot <- as.integer(system("cat /proc/meminfo | awk '/^MemTotal:/ {print $2/ (2^10)}'", intern = TRUE))
      memActive <- as.integer(system("cat /proc/meminfo | awk '/^Active:/ {print $2/ (2^10)}'", intern = TRUE))
      memFree <- as.integer(system("cat /proc/meminfo | awk '/^MemFree:/ {print $2/ (2^10)}'", intern = TRUE))
      memCached <- as.integer(system("cat /proc/meminfo | awk '/^Cached:/ {print $2/(2^10)}'", intern = TRUE))

      free <- memTot - memActive
    }
  )

  as.integer(free)
}

#' Reset AccessMod region
#' @param {Character} rasters Rasters to set the region
#' @param {Character} vectors vectors to set the region
amRegionSet <- function(rasters = character(0), vectors = character(0)) {
  hasRasters <- amRastExists(rasters)
  hasVectors <- amVectExists(vectors)

  if (!hasRasters && !hasVectors) {
    warning("amRegionSet: no layer available to update region")
    return()
  }
  print("Setting region using the following:")
  print(rasters)
  print(vectors)
  print("current region")
  execGRASS("g.region", flags = "p")
  print("new region")
  execGRASS("g.region",
    raster = rasters,
    vector = vectors,
    align = config$mapDem,
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
  minTravelTime = NULL,
  timeoutValue = -1L,
  getMemDiskRequirement = FALSE,
  ratioMemory = 1,
  memory = NULL, # if set, absolute max memory
  rawMode = FALSE,
  knights_move = FALSE
) {
  vInfo <- amParseOptions(
    execGRASS(
      "v.info",
      flags = c("t"),
      map = inputHf,
      intern = TRUE
    )
  )

  if (as.numeric(vInfo$lines) > 0) {
    stop("Health facilities must be a point vector; found line geometry")
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
        intern = TRUE
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

  if (!getMemDiskRequirement) {
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
    flags <- c("overwrite", "n")
    if (knights_move) {
      flags <- c(flags, "k")
    }
    execGRASS("r.cost",
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
  timeoutValue = "null()",
  getMemDiskRequirement = FALSE,
  ratioMemory = 1,
  memory = NULL, # if set, absolute max memory
  rawMode = FALSE, # skip minute conversion; skip value removal above maxTravelTime
  knights_move = FALSE
) {
  walk_fn <- "r.walk.accessmod"
  flags <- c("overwrite", "s")
  if (towardsFacilities) {
    flags <- c(flags, "t")
  }
  if (knights_move) {
    flags <- c(flags, "k")
  }
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
      intern = TRUE
    )
  )

  if (as.numeric(vInfo$lines) > 0) {
    stop("Health facilities must be a point vector; found line geometry")
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
        intern = TRUE
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

  if (!getMemDiskRequirement) {
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
  divider <- 1
  timeoutMinutesLimit <- 0
  timeoutMinutesValue <- timeoutValue
  cutSecondsStart <- 0
  cutSecondsEnd <- 0

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
    paste0(
      " %1$s = %1$s >= %2$d && %1$s <= %3$d ? round((( %1$s / %6$f) - (( %1$s / %6$f ) %% 1)))",
      " : %1$s / %6$d > %4$d ? %5$s : null() "
    ),
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
  for (i in seq_len(nrow(tbl))) {
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
  if (!dir.exists("temp")) {
    dir.create("temp")
  }
  tmpFile <- tempfile(tmpdir = "temp")
  write(reclassRules, tmpFile)
  #
  # Reclass the merged landcover
  #
  reclassRes <- execGRASS("r.reclass",
    input = mapMerged,
    output = mapSpeed,
    rules = tmpFile,
    flags = "overwrite"
  )
  # ponytail: debug — patched r.reclass under GRASS 8 may exit 0 yet write no
  # map; execGRASS does not raise on a clean exit, so capture the return code
  # and verify the output exists. Remove once §2.2 r.reclass is confirmed.
  print(sprintf(
    "[debug] amCreateSpeedMap: r.reclass exit=%s; output '%s' exists=%s",
    reclassRes, mapSpeed, amRastExists(mapSpeed)
  ))
  unlink("temp", recursive = TRUE)
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
  for (i in seq_len(nrow(tbl))) {
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
  # v.what.rast defaults to comma-separated output under GRASS 8 (main.c
  # overrides G_OPT_F_SEP's standard "pipe" default). The parser below reads
  # sep="|", so request pipe explicitly -- version-agnostic, restores the
  # 7.8 output format. Without this, comma output parses as one column,
  # every row is filtered out, and the facilities table silently empties.
  data <- execGRASS("v.what.rast",
    map = inputPoint,
    raster = inputRaster,
    flags = "p",
    separator = "pipe",
    intern = TRUE
  )
  print(sprintf(
    "[debug] amGetRasterValueAtPoint: v.what.rast returned %d lines; head=%s",
    length(data), paste(head(data, 3), collapse = " | ")
  ))

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
      fill = TRUE
    )
    tbl
    tbl <- tbl[!is.na(as.numeric(tbl[[1]])) & !is.na(as.numeric(tbl[[2]])), ]
  }

  names(tbl) <- c("cat", "val")
  tbl
}

amGetFacilitiesTableWhatRast <- function(mapHf, mapRaster) {
  amRegionSet(mapRaster, mapHf)
  tbl <- amGetRasterValueAtPoint(mapHf, mapRaster)

  amRegionReset()
  tbl
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
  print(sprintf("[debug] amValidateFacilitiesTable: pre-merge nrow(tblHf)=%d, nrow(tbl)=%d", nrow(tblHf), nrow(tbl)))
  tbl <- merge(tbl, tblHf, by = "cat")
  print(sprintf("[debug] amValidateFacilitiesTable: post-merge nrow=%d", nrow(tbl)))

  tbl
}

#' Import temporary catchment vector to final directory
#' @param vectFile Full path to temp catchment file. eg. /tmp/super.gpkg
#' @param outDir Directory path where the output vector is stored.
#' @param outName Name of the final catchment vector, without extension. e.g. catchments_001
#' @return Boolean Done
amMoveGpkg <- function(vectFile, outDir, outName) {
  #
  # Collect all gpkg related files and copy them to final directory.
  # NOTE: make sure that:
  # - pattern of vector is unique in its directory

  # in case of variable in path, convert outdir to fullpath
  if (length(vectFile) < 1) {
    return()
  }
  outDir <- path.expand(outDir)

  fe <- file.exists(vectFile)
  de <- dir.exists(outDir)
  so <- grepl(".*\\.gpkg$", vectFile)

  if (!fe) {
    warning(
      sprintf("amMoveGpkg: %s input file does not exists", vectFile)
    )
  }
  if (!de) {
    warning(
      sprintf("amMoveGpkg: %s output directory does not exists", outDir)
    )
  }
  if (!so) {
    warning(
      sprintf("amMoveGpkg: %s input file does not have .gpkg extension", vectFile)
    )
  }

  ok <- c(fe, de, so)

  if (all(ok)) {
    # base name file for pattern.
    baseShape <- gsub(".gpkg", "", basename(vectFile))
    # list files matching the base name.
    allVectFiles <- list.files(dirname(vectFile), pattern = paste0("^", baseShape), full.names = TRUE)
    # Copy each file into the final catchment directory.
    for (s in allVectFiles) {
      sExt <- file_ext(s)
      newPath <- file.path(outDir, paste0(outName, ".", sExt))
      file.copy(s, newPath, overwrite = TRUE)
    }
  }
  all(ok)
}

debug_header <- function(text) {
  print(paste("######################", text))
}

debug_raster_report <- function(map) {
  tryCatch(
    {
      report <- execGRASS("r.report", map = map, units = c("k", "c", "p"), intern = TRUE)
      write.table(report,
        file = paste0(map, "_report.txt"),
        row.names = FALSE, quote = FALSE
      )
    },
    error = function(cond) {
      print(sprintf("  [debug_raster_report] r.report FAILED for map=%s: %s", map, conditionMessage(cond)))
    }
  )
}

amCleanupTmpLayers <- function() {
  try(
    execGRASS("g.remove", flags = c("b", "f"), type = "raster", pattern = "tmp_*"),
    silent = TRUE
  )
  try(
    execGRASS("g.remove", flags = c("b", "f"), type = "vector", pattern = "tmp_*"),
    silent = TRUE
  )
  if (dir.exists("temp")) {
    unlink("temp", recursive = TRUE)
  }
}

# --- Log handling ----------------------------------------------------------
# Each entrypoint opens a startup log in ../logs/ before validating inputs, then
# migrates to per-run logs inside its output_dir once that path is known.
# R constraints: a message sink needs a file() connection (not a filename
# string), and only ONE message diversion may be active at a time -- so the
# startup message sink is closed before the run-specific one opens. append=FALSE
# truncates on open, so each run starts with a fresh log.

open_startup_logs <- function(name) {
  if (!dir.exists("../logs")) {
    dir.create("../logs")
  }
  sink(paste0("../logs/", name, ".log"), append = FALSE, split = TRUE, type = "output")
  .errCon <- file(paste0("../logs/", name, "_error.log"), open = "wt")
  sink(.errCon, type = "message")
  # sink(type="message") captures message()/warning() but NOT top-level
  # stop() errors -- R prints those to C-level stderr, which app.py does not
  # capture, so error_log.txt stays silently empty. Route uncaught errors
  # through message() (which the sink does capture) and quit non-zero so
  # JobRunner still marks the job failed.
  options(error = function(e) {
    message("ERROR: ", conditionMessage(e))
    if (!is.null(e$call)) {
      message("Call: ", paste(deparse(e$call), collapse = " "))
    }
    quit(save = "no", status = 1)
  })
  invisible(.errCon)
}

migrate_to_run_logs <- function(output_dir, .errCon) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  sink(paste0(output_dir, "/output_log.txt"), append = FALSE, split = TRUE, type = "output")
  sink(type = "message")
  close(.errCon)
  .errCon <- file(paste0(output_dir, "/error_log.txt"), open = "wt")
  sink(.errCon, type = "message")
  invisible(.errCon)
}
