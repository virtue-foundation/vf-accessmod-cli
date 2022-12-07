clean_filepath <- function(path) {
  gsub("\\\\", "/", path)
}

amRastExists <- function(filter = "", mapset = NULL) {
  return(amLayerExists(filter, mapset, "raster"))
}

amSubPunct <- function(vect,
                       sep = "_",
                       rmTrailingSep = T,
                       rmLeadingSep = T,
                       rmDuplicateSep = T,
                       debug = F) {
  # vect<-str_sub("'",'',iconv(vect, to='ASCII//TRANSLIT'))
  res <- str_sub("[[:punct:]]+|[[:blank:]]+", sep, vect) # replace punctuation by sep
  res <- str_sub("\n", "", res)
  if (rmDuplicateSep) {
    if (nchar(sep) > 0) {
      res <- str_sub(paste0("(\\", sep, ")+"), sep, res) # avoid duplicate
    }
  }
  if (rmLeadingSep) {
    if (nchar(sep) > 0) {
      res <- str_sub(paste0("^", sep), "", res) # remove trailing sep.
    }
  }
  if (rmTrailingSep) {
    if (nchar(sep) > 0) {
      res <- str_sub(paste0(sep, "$"), "", res) # remove trailing sep.
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
import_layer <- function(path, type, layer_name, ignore_proj=F) {
  
  import_fn = switch(type, raster="r.in.gdal", vector="v.in.ogr")
  
  # import parameters
  if(type=="raster") { #e.g., .tif
    import_parameters = list(input=path, output=layer_name)
  } else if (type=="vector") { # could be shapefile directory or .geojson
    if(is_a_dir(path)) { #shapefile directory
      filename = get_shapefile_dir(path)
      import_parameters = list(input=path, output=layer_name, layer=filename, snap=0.0001)
    } else { # probably geojson
      import_parameters = list(input=path, output=layer_name)
    }
  }
  
  if(ignore_proj) {
    execGRASS(import_fn, flags="o", parameters=import_parameters)
  } else {
    execGRASS(import_fn, parameters=import_parameters)
  }
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
