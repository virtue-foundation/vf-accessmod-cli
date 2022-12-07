
#    AccessMod 5 refactor for Virtue Foundation
#    re-writing features of AccessMod 5 as streamlined R functions accessible from command line

# TO-DO: WORKFLOW: get rid of overwrite logic

# TO-DO: NAMING CONVENTIONS - need to be comprehensive of: object type (r/v/t), stage (raw/intermediate/final),
#------- map (lcv/road/barrier), if it's a temp object and name will be re-applied to something else,etc.
#------- Function naming

# TO-DO: BARRIERS - polygons as skeletons?

# TO-DO: BRIDGE CLEANING / ARTIFACT REMOVAL


###################

# DONE: INPUT - input args to be entered via command prompt

# DONE: ENVIRONMENT SETTINGS - pull proj info from roads file before importing anything - works on geojson

# DONE: IMPORT - rewrite import fn so that it assumes geojson - hits a snag if not geojson

# DONE: ROADS - need to include all roads

# DONE: DIAGNOSTICS - cleaned up verbose/diagnostics logic into debug_print / debug_store

# DONE: ENV SETTINGS - avoid making new GRASS session+location every time using gmeta() and proj checks

###############################################################################################################################

########
##
########

###############################################################################################################################
  
########################## Setup
#!/usr/bin/env Rscript
source("config.R")
option_list = list(
  make_option("--lcv", type="character", default=NULL, 
              help="path to base land cover file", metavar="character"),
  make_option("--roads", type="character", default=NULL, 
              help="path to roads file", metavar="character"),
  make_option("--b1", type="character", default=NULL, 
              help="path to barrier file (rivers/lakes)", metavar="character"),
  make_option("--b2", type="character", default=NULL, 
              help="path to barrier file (rivers/lakes)", metavar="character"),
  make_option("--table", type="character", default=NULL, 
              help="path to land use key", metavar="character"),
  make_option("--debug-print", type="character", default=F, action="store_true",
              help="Print diagnostic info to std-out", metavar="character"),
  make_option("--debug-store", type="character", default=F, action="store_true",
              help="Write GRASS objs into R session env", metavar="character"),
  make_option("--clean-bridges", type="character", default=F, action="store_true",
              help="Clean up artefacts of the merge", metavar="character"),
  make_option("--name", type="character", default=NULL, 
              help="region name for output", metavar="character"),
  make_option("--output_dir", type="character", default=".", 
              help="output directory path", metavar="character")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
desired_inputs <- c("lcv", "roads", "b1", "b2", "help")
missing_inputs <- setdiff(desired_inputs, names(opt))
if( length(missing_inputs)>0 && missing_inputs != "b2" ) {
  print("Check your input args")
  stop()
}

path_lcv <- clean_filepath(opt$lcv)
path_road <- clean_filepath(opt$roads)
path_barrier <- clean_filepath(opt$b1)
path_barrier_poly <- clean_filepath(opt$b2)
path_table <- clean_filepath(opt$table)
input_region_name <- opt$name
debug_print <- opt$`debug-print`
debug_store <- opt$`debug-store`
clean_bridges <- opt$`clean-bridges`
output_dir <- clean_filepath(opt$output_dir)

sink(paste0(output_dir, "/", "output_log.txt"), append=FALSE, split=TRUE, type = "output")

if(debug_print) print("Arguments accepted. Setting projection")


# Projection setting
# First check if a proj is already loaded, if there is then we likely want to keep it
# If not though, get proj info from input lcv and pass it to GRASS
# Make sure to stay in the same mapset
current_mapset <- execGRASS("g.mapset", flags="p", intern=T)
if(debug_print) print(paste("Current mapset is ", current_mapset))
current_proj <- tryCatch(execGRASS("g.proj", flags="p", intern=T), error=function(e) NULL)
current_proj_isempty <- any(str_detect(current_proj, "WARNING")) || (length(current_proj) == 0)
if(debug_print) print(current_proj)
if(current_proj_isempty) {
  print("Current proj is empty; setting new proj")
  execGRASS("g.mapset", parameters=list(mapset="PERMANENT"))
  execGRASS("g.proj", flags="c", parameters=list(georef=path_lcv))
  execGRASS("g.mapset", parameters=list(mapset=current_mapset))
  print("Set new proj")
}

########################## Land cover
# Import

if(is_loaded("r_lcv")) {
  print("Land cover is already loaded")
} else {
  import_layer(path = path_lcv, layer_name="r_lcv", type="raster")
  
  # Region
  execGRASS("g.region", parameters=list(raster="r_lcv"), flags=c("m", "a"))
  
  # Categories
  lcv_table <- read.table(path_table, sep=",", header=T)
  tblOut <- tempfile()
  write.table(lcv_table,
              file = tblOut,
              row.names = F,
              col.names = F,
              sep = "\t",
              quote = F
  )
  execGRASS("r.category", map="r_lcv", rules=tblOut)
  
  # Colors
  execGRASS("r.colors", map = "r_lcv", color = "random")
}

# Add to stack
raster_stack <- "r_lcv"

if (debug_store) { r_lcv <<- read_RAST("r_lcv") }

######################### Barrier stack

barrier_stack <- NULL

# Barrier - linear

outNameStack_barrier_line <- "r_barrier_line"

if(path_barrier != "null") {
  # Import
  if(is_loaded("v_barrier")) {
    print("Linear barrier vector file is already loaded")
  } else {
    import_layer(path = path_barrier, layer="v_barrier", type="vector", ignore_proj=T)
    
    if (debug_store) {
      v_barrier <<- read_VECT("v_barrier")
    }
  }
  
  # prepare and add to stack
  
  if(is_loaded(outNameStack_barrier_line)) {
    print("Linear barrier raster is already loaded")
  } else {
    
    # preview table of barrier features
    barrier_table = amGetTableFeaturesCount("v_barrier", types = c("lines", "areas", "points"))
    barrier_type = barrier_table[which.max(barrier_table$count), "type"] %>% str_remove("s")
    if(barrier_type != "line") {
      print("Please check that linear barrier file is lines")
      stop()
    }
    
    cl = 1
    la = "barrier"
    tmpFile <- tempfile()
    write(paste0(cl, "\t", la), tmpFile)
    execGRASS("v.to.rast",
              use = "val",input = "v_barrier",output = outNameStack_barrier_line, type = barrier_type,value = cl,
              flags = c("d")
    )
    execGRASS("r.category", map = outNameStack_barrier_line, rules = tmpFile) 
    
    if (debug_store) {
      r_barrier <<- read_RAST(outNameStack_barrier_line)
    }
  }
  
  # Add to stack
  barrier_stack <- add_to_stack(outNameStack_barrier_line, barrier_stack, back=F) 
}

# Barrier - poly
if(path_barrier_poly != "null") {
  if(is_loaded("v_barrier_poly")) {
    print("Area barrier vector file is already loaded")
  } else {
    import_layer(path = path_barrier_poly, layer="v_barrier_poly", type="vector", ignore_proj=T)
    if (debug_store) {
      v_barrier_poly <<- read_VECT("v_barrier_poly")
    }
  }
  
  # prepare and add to stack
  outNameStack_barrier_poly <- "r_barrier_poly"
  
  if(is_loaded(outNameStack_barrier_poly)) {
    print("Area barrier raster is already loaded")
  } else {
    # preview table of barrier features
    barrier_poly_table = amGetTableFeaturesCount("v_barrier_poly", types = c("lines", "areas", "points"))
    if(debug_print) print(barrier_poly_table)
    barrier_poly_type = barrier_poly_table[which.max(barrier_poly_table$count), "type"] %>% str_remove("s")
    if(barrier_poly_type != "area") {
      print("Please check that area barrier file is areas")
      stop()
    }
    cl = 1
    la = "barrier"
    tmpFile <- tempfile()
    write(paste0(cl, "\t", la), tmpFile)
    
    execGRASS("v.to.rast",
              use = "val",input = "v_barrier_poly",output = outNameStack_barrier_poly, type = barrier_poly_type,
              value = cl, 
    )
    execGRASS("r.category", map = outNameStack_barrier_poly, rules = tmpFile) 
    
    if (debug_store) {
      r_barrier_poly <<- read_RAST(outNameStack_barrier_poly)
    }
  }
  
  # Add to stack
  barrier_stack <- add_to_stack(outNameStack_barrier_poly, barrier_stack, back=F)
}


# Barrier hack - patching barriers together

if(length(barrier_stack) > 1) {
  execGRASS("r.patch",
            input = paste(barrier_stack, collapse = ","),
            output = "r_barrier",
            flags = c("overwrite")
  )
  raster_stack <- add_to_stack("r_barrier", raster_stack, back=F)
} else if (length(barrier_stack) == 1) {
  raster_stack <- add_to_stack(barrier_stack, raster_stack, back=F)
}
# If there's no barrier don't add anything to stack

######################### Road
# Import

if(!is_loaded("v_road")) {
  import_layer(path = path_road, layer="v_road", type="vector", ignore_proj=T)
}

if (debug_store) {
  v_road <<- read_VECT("v_road")
}

# Get attribute table
# Ultimately want dynamic column selection based on a db query, hardcoding for now as "class" and "label"
stack_class = "rStackRoad"
road_class_key = "class"
road_label_key = "label"
road_table = get_att_table(map = "v_road", cla_col = "class", lab_col = "label")
road_table = arrange(road_table, desc(class))
tblN = nrow(road_table)

if(debug_print) print(road_table)

for (i in 1:tblN) {
  class = road_table[i, road_class_key]
  label = road_table[i, road_label_key]
  outNameTmp_road = "v_road_temp_byclass"
  outNameStack_road = paste0("r_road_", class)
  if (is_loaded(outNameStack_road)) next
  colorSetting <- amClassListInfo(stack_class, "colors")
  
  execGRASS("v.extract",
            input  = "v_road",
            output = outNameTmp_road,
            where  = paste0(road_class_key, "=", class),
            flags  = "overwrite"
  )
  
  if(debug_print) {
    grass_print_info(outNameTmp_road, type="vector")
  }
  if (debug_store) {
    v_road_tmp <<- read_VECT(outNameTmp_road)
  }
  
  # Class and label cleaning
  # if (isTRUE(class < 1000)) {
  #   class <- 1000 + class
  # }
  label <- str_replace(label, "/", "_")
  labelRule <- str_replace(label, "/", " ")
  tmpFile <- tempfile()
  tmpRules <- paste0(class, "\t", labelRule)
  if (debug_print) print(tmpRules)
  
  # Write temp rules
  write(tmpRules, file = tmpFile)
  
  # Raster creation
  execGRASS("v.to.rast",
            use = "val",
            type = "line",
            input = outNameTmp_road,
            output = outNameStack_road,
            value = class,
            flags = c("d")
  )
  execGRASS("r.colors", map = outNameStack_road, color = colorSetting[1])
  execGRASS("r.category",
            map = outNameStack_road,
            rules = tmpFile
  )
  
  # Diagnostics
  if (debug_store) {
    r_road <<- read_RAST(outNameStack_road)
  }
  if (debug_print) {
    grass_print_info(outNameStack_road, "raster")
    # road_report <- execGRASS("r.report", map=outNameStack_road, units="kilometers,percent", intern=T)
    # write.table(road_report, file="", row.names = F)
  }
  
  # Clean up
  rmIfExists(outNameTmp_road, "vector")
}

# Make road stack
road_stack <- paste0("r_road_", road_table[[road_class_key]])

# Add road stack to raster stack
raster_stack <- add_to_stack(road_stack, raster_stack, back=F)

######################### Merge
stackTag = "merge_stack"
if(debug_print) print(raster_stack)
if(all(is_loaded(raster_stack))) {
  merged_lcv_name <- paste0(input_region_name, "_merged_landcover")
  
  mapPosition <- 1
  tempBase <- "tmp__"
  #isFirstMap <- TRUE
  #rmRastIfExists("tmp_*")
  if (is_loaded("MASK")) execGRASS("r.mask", flags = "r")
  
  # Use barrier as mask for each stack element
  # keep order in tempMap name. eg. tmp__12_stack_road_test
  stack_length = length(raster_stack)
  for (i in 1:stack_length) {
    # extract stack item
    map <- raster_stack[i]
    if (debug_print) print(map)
    
    # If it's a barrier
    if (length(grep("r_barrier", map)) > 0) {
      if (is_loaded("MASK")) {
        # If a mask already exist, update it
        if(debug_print) print('Updating mask')
        execGRASS("r.mapcalc",
                  expression = paste("MASK=isnull(", map, ")?MASK:null()"),
                  #expression = paste("MASK=(!isnull(", map, "))||isnull(MASK)?MASK:null()"),
                  flags = "overwrite"
        )
      } else {
        if(debug_print) print("Making a new mask")
        # If not mask exist, use it as inverse mask
        execGRASS("r.mask", raster = map, flags = c("i"))
      }
    } else {
      # it's not a barrier : create temporary version of it using MASK context.
      # convert number to character eg. 12 "00012"
      classPos <- paste0(
        paste0(rep(0, 5 - nchar(mapPosition)),
               collapse = ""
        ),
        mapPosition
      )
      tempMap <- paste0(tempBase, classPos, "_", map)
      if(debug_print) print("Copying into MASK context")
      execGRASS("r.mapcalc",
                expression = paste(tempMap, "=", map),
                flags = "overwrite"
      )
    }
    mapPosition <- mapPosition + 1
  }
  # removing temp mask and active mask
  rmRastIfExists("tmp_mask__*")
  if (amRastExists("MASK")) execGRASS("r.mask", flags = "r")
  
  # get list of tmp__stack... maps.
  # these are non-barrier rasters we just created
  tempMapList <- execGRASS("g.list",
                           type = "raster",
                           pattern = paste0(tempBase, "*"),
                           intern = TRUE
  )
  if (debug_print) {
    print(tempMapList)
  }
  
  if (length(tempMapList) > 1) {
    if(debug_print) print("Patching ")
    execGRASS("r.patch",
              input = paste(tempMapList, collapse = ","),
              output = merged_lcv_name,
              flags = c("overwrite")
    )
  } else {
    execGRASS("g.copy",
              raster = paste0(tempMapList, ",", merged_lcv_name),
              flags = "overwrite"
    )
  }
  
  # Cleaning bridge artefacts
  if (clean_bridges) {
    fromRoad <- raster_stack[grep("r_road", raster_stack)]
    amBridgeFinder(fromRoad, merged_lcv_name, "bridge_layer")
    amBridgeRemover("bridge_layer", removeFromMap = merged_lcv_name)
  }
    
    # set colors
    execGRASS("r.colors",
              map = merged_lcv_name,
              color = "random"
    )
    
  execGRASS("g.remove", flags=c("b", "f"), type="raster", pattern="tmp__*")
  
  execGRASS("r.out.gdal", parameters=list(input=merged_lcv_name,
                                          output=paste0(output_dir, "/", merged_lcv_name, ".img"),
                                          createopt="COMPRESSED=YES",
                                          format="HFA"),
            flags=c("overwrite", "f", "c", "m"))
  report <- execGRASS("r.report", map=merged_lcv_name, units=c("k","c","p"), intern=T)
  write.table(report, file=paste0(output_dir, "/", merged_lcv_name, "_report.txt"), row.names = F)
  if(debug_store) {
    r_merged_lcv <<- read_RAST(merged_lcv_name)
  }
}


