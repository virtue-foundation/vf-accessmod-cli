
#    AccessMod 5 refactor for Virtue Foundation
#    re-writing features of AccessMod 5 as streamlined R functions accessible from command line

# Accessibility Analysis

########################## TO-DO

# TO-DO: FUNCTION STRUCTURE: friction / speed map as sequential / intermediary output here and in accessibility
# TO-DO: ANNOTATION: annotate all debug outputs

# TO-DO: BUG: examine pop covered percent denominator for csv outputs

########


########


# DONE: INPUT: Incorporate and properly parse new parameters (facility columns / zonal column)
# DONE: VALIDATION: admin boundary input checks, pop on barrier check, table column name checks
# DONE: MAIN FUNCTION: properly line up parameters to amCapacityAnalysis.R
# DONE: MAIN FUNCTION: refactor capacity analysis initial pre-processing (max speed, hf table, friction/speed map, initialize pop raster)
# DONE: MAIN FUNCTION: capacity analysis loop on HFs - transplant code, compute cost map, line up parameters to sub-function
# DONE: OUTPUTS: zonal table using stats pulled from GRASS by r.univar(amCatchmentAnalyst.R)
# DONE: OUTPUT: get outputs from capacity function, write outputting (writing-to-disk) logic


########################## Setup
#!/usr/bin/env Rscript
source("config.R")
source("functions-geo.R")

option_list = list(
  # Required Inputs
  make_option("--lcv", type="character", default=NULL, 
              help="Path to merged land cover raster file (required)"),
  make_option("--dem", type="character", default=NULL, 
              help="Path to elevation file (required)"),
  make_option("--pop", type="character", default=NULL, 
              help="Path to population raster file (required)"),
  make_option("--scenarios", type="character", default=NULL,
              help="Path to scenario table file (required)"),
  make_option("--facilities", type="character", default=NULL,
              help="Path to facilities file (required)"),
  
  # Optional Inputs
  make_option("--admin", type="character", default=NULL,
              help="Path to admin boundaries vector (not required)"),
  
  # Required Parameters
  make_option("--name", type="character", default=NULL,
              help="Name of the region / output save  name (required)"),
  make_option("--analysis_type", type="character", default=NULL,
              help="Type of the analysis (aniso/isotropic) (required)"),
  make_option("--f_order", type="character", default=NULL,
              help="Name of facilities column for order in which to analyze coverage"),
  make_option("--f_name", type="character", default=NULL,
              help="Name of column in facilities table that contains facility name",metavar="character"),
  
  # Optional parameters
  make_option("--max_time", type="double", default=0,
              help="Maximum duration of travel considered"),
  make_option("--knights_move", type="character", default=F, action="store_true",
              help="Allow knight's move for traversing the landscape"),
  make_option("--output_dir", type="character", default=".", 
              help="output directory path"),
  make_option("--f_subset", type="character", default="all",
              help="Name of boolean facilities column for inclusion in subset"),
  make_option("--f_capacity", type="character", default=NULL,
              help="Name of capacity column in facilities table, if left unset will run without considering capacity"),
  make_option("--zonal_column", type="character", default=NULL,
              help="Name of column in admin boundaries table that contains zone name",metavar="character"),
  
  # Diagnostic
  make_option("--debug_print", type="character", default=T, action="store_true",
              help="Print diagnostic info to std-out")
)

# Input validation
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
required_inputs <- c("lcv", "dem", "pop", "scenarios", "facilities")
missing_inputs <- required_inputs[!required_inputs %in% names(opt)]
sink(paste0("../logs/", "geographic_coverage_analysis.log"), append=FALSE, split=TRUE, type = "output")
if(length(missing_inputs)>0) {
  print("Missing the following required file inputs, please check")
  print(paste(missing_inputs, collapse = ", "))
  stop()
}
required_params <- c("name", "analysis_type", "f_order", "f_name")
missing_params <- required_params[!required_params %in% names(opt)]
if(length(missing_params)>0) {
  print("Missing the following required parameters, please check")
  print(paste(missing_params, collapse=", "))
  stop()
}

# Parsing inputs
path_lcv <- clean_filepath(opt$lcv)
path_dem <- clean_filepath(opt$dem)
path_pop <- clean_filepath(opt$pop)
path_scenarios <- clean_filepath(opt$scenarios)
path_facilities <- clean_filepath(opt$facilities)
if("admin" %in% names(opt)) {path_admin <- clean_filepath(opt$admin)} else {path_admin <- NULL}

# Parsing parameters
input_region <- opt$name
input_analysis_type <- opt$analysis_type
if(!(input_analysis_type %in% c("isotropic", "anisotropic"))) {
  print("Check your input args: analysis type must be one of: isotropic, anisotropic")
  stop()
}
input_f_order_col <- opt$f_order
input_f_name_col <- opt$f_name
input_max_time <- opt$max_time
input_knights_move <- opt$knights_move
input_f_subset_col <- opt$f_subset

ignoreCapacity <- FALSE
input_f_capacity_col <- opt$f_capacity
if(is.null(input_f_capacity_col)) {
  ignoreCapacity <- TRUE
  print("Will run analysis without considering capacities")
}

zonalCoverage <- FALSE
input_zonal_col <- opt$zonal_column
print(path_admin)
print(input_zonal_col)
# If admin is set, we need zonal column to be set
if ((!is.null(path_admin)) & (!is.null(input_zonal_col))) {
  zonalCoverage <- TRUE
} else if (is.null(path_admin) & !is.null(input_zonal_col)) {
  print("If you want zonal statistics, please supply both the path to the admin file (--admin) and the name of the zonal column in that file (--zonal_column)")
  stop()
} else {
  zonalCoverage <- FALSE
}


debug_print <- opt$debug_print

output_dir <- clean_filepath(opt$output_dir)
if(!dir.exists(output_dir)) {dir.create(output_dir)}
sink(paste0(output_dir, "/", "output_log.txt"), append=FALSE, split=TRUE, type = "output")

if(debug_print) print("Arguments accepted. Setting projection")

# Projection setting
# First check if a proj is already loaded, if there is then we likely want to keep it
# If not though, get proj info from input lcv and pass it to GRASS
# Make sure to stay in the same mapset
current_mapset <- execGRASS("g.mapset", flags="p", intern=T)
if(debug_print) print(paste("Current mapset is ", current_mapset))
print("Setting projection")
execGRASS("g.mapset", parameters=list(mapset="PERMANENT"))
execGRASS("g.proj", flags="c", parameters=list(georef=path_lcv))
execGRASS("g.mapset", parameters=list(mapset=current_mapset))
print("Set new proj")

########################## Import and process inputs

if(is_loaded("r_merged_lcv")) {
  print("Land cover is already loaded")
} else {
  import_layer(path = path_lcv, layer_name="r_merged_lcv", type="raster")
  
  # Region
  region <- execGRASS("g.region", parameters=list(raster="r_merged_lcv"), flags=c("a", "p"), intern=T)
  
  # Categories
  # lcv_table <- read.table(path_table, sep=",", header=T)
  # tblOut <- tempfile()
  # write.table(lcv_table,
  #             file = tblOut,
  #             row.names = F,
  #             col.names = F,
  #             sep = "\t",
  #             quote = F
  # )
  # execGRASS("r.category", map="r_merged_lcv", rules=tblOut)
  #   
  # # Colors
  # execGRASS("r.colors", map = "r_merged_lcv", color = "random")
  
}

# Import elevation
if(is_loaded("r_dem")) {
  print("Elevation raster is already loaded")
} else {
  import_layer(path=path_dem, layer_name="r_dem", type="raster", ignore_proj=T)
}

# Import population
if(is_loaded("r_pop")) {
  print("Population raster is already loaded")
} else {
  import_layer(path=path_pop, layer_name="r_pop", type="raster", ignore_proj=T)
}

# Import facilities
if(is_loaded("v_hf")) {
  print("Health facilities vector is already loaded")
} else {
  import_layer(path=path_facilities, layer_name="v_hf", type="vector", ignore_proj=T)
}


# Getting facilities table
b <- execGRASS("db.select", parameters=list(table="v_hf"), intern=TRUE)
con <- textConnection(b)
tableFacilities <- read.delim(con, header=TRUE, sep="|", allowEscapes = T, encoding = "UTF-8", quote="", comment.char="")
close(con)

# Subset facilities based on boolean column in table
if (input_f_subset_col != "all") {
  print(input_f_subset_col)
  inputHfFinal <- amFacilitiesSubset(
    tableFacilities = tableFacilities,
    inputFacilities = "v_hf",
    select_col = input_f_subset_col
  )
} else {
  inputHfFinal <- "v_hf"
}
if(debug_print) print(inputHfFinal)

# Import scenario table
t_scenarios <- read.csv(path_scenarios, check.names=FALSE)
names(t_scenarios) <- c("class", "label", "speed", "mode")

# Import admin boundaries table
if(!is.null(path_admin)) {
  if(is_loaded("v_admin")) {
    print("Admin boundaries vector is already loaded")
  } else {
    import_layer(path=path_admin, layer_name="v_admin", type="vector", ignore_proj=T)
  }
}

########################## Validation

# Facilities on barrier (or speed 0)

validated_hf <- amValidateFacilitiesTable(tblHf = tableFacilities,
                                          mapHf = inputHfFinal,
                                          mapMerged = "r_merged_lcv",
                                          mapDem = "r_dem",
                                          tblSpeed = t_scenarios)


if(any(validated_hf$amOnZero)) {
  stop("Some facilities are on zero-speed positions on the land cover, please check")
}

if(any(validated_hf$amOnBarrier)) {
  stop("Some facilities are on positions in the land cover that are classified as a barrier, please check")
}

# Population on barrier

t_pop_on_barrier <- popOnBarrierCheck("r_pop", "r_merged_lcv")

if(t_pop_on_barrier$cells > 0) {
  stop("Some of the population seems to be located on barrier cells, please adjust your pop raster")
}


######################### Main function

# # get current mapset and loc
# currentMapset <- amGrassSessionGetLocation()
# currentLocation <- amGrassSessionGetMapset()

# field selection
hfIdx <- "xid"
hfLab <- input_f_name_col
hfIdxTo <- "xid"
hfLabTo <- input_f_name_col
capField <- input_f_capacity_col
orderField <- input_f_order_col

# parameters
maxTravelTime <- input_max_time
maxTravelTimeOrder <- 120
dirAnalysis <- "toHF"
typeAnalysis <- input_analysis_type
#limitClosest <- input$checkReferralLimitClosest
#useParallel <- input$checkReferralParallel
#snapToGrid <- input$checkReferralSnapToGrid
# permuteGroups <- input$checkReferralPermute
# keepNetDist <- input$checkReferralKeepNetwork
useMaxSpeedMask <- FALSE
# selectedAnalysis <- input$moduleSelector
# hfOrder <- input$hfOrder
# hfOrderSorting <- input$hfOrderSorting
# popBuffer <- input$popBufferRadius
# modParam <- input$mod3param
keepFullHfTable <- FALSE
# configSettingsOnly <- FALSE

# logic
# return path = towards facilities.
towardsFacilities <- ifelse(dirAnalysis == "toHf", TRUE, FALSE)

#
# Start analysis
#

if (!keepFullHfTable) {
  validated_hf <- validated_hf[c(
    config$vectorKey,
    # "amSelect",
    orderField,
    capField,
    hfIdx,
    hfLab)
  ]
}

args <- list(
  inputMerged = "r_merged_lcv",
  inputPop = "r_pop",
  inputHf = inputHfFinal,
  inputZoneAdmin = "v_admin",
  outputPopResidual = "r_pop_resid",
  outputHfCatchment = paste0(input_region, "_catchment_", input_f_subset_col),
  outputPopBarrier = "r_pop_barrier",
  outputTableCapacity = "t_capacity",
  outputTableZonal = "t_zonal",
  outputSpeed = "r_speed",
  outputFriction = "r_friction",
  typeAnalysis = typeAnalysis,
  removeCapted = TRUE,
  vectCatch = TRUE,
  popOnBarrier = TRUE,
  towardsFacilities = towardsFacilities,
  radius = 5000,
  maxTravelTime = maxTravelTime,
  maxTravelTimeOrder = maxTravelTimeOrder,
  useMaxSpeedMask = useMaxSpeedMask,
  hfIdx = hfIdx,
  nameField = hfLab,
  capField = capField,
  orderField = orderField,
  ignoreCapacity = ignoreCapacity,
  addColumnPopOrigTravelTime = FALSE,
  addColumnsPopCoverageExtended = FALSE,
  zonalCoverage = zonalCoverage,
  zoneFieldId = "cat",
  zoneFieldLabel = input_zonal_col,
  hfOrder = "tableOrder",
  hfOrderSorting = "hfOrderDesc",
  tableScenario = t_scenarios,
  tableFacilities = validated_hf,
  debug_print = debug_print,
  outdir = output_dir
)

output_list <- do.call("amCapacityAnalysis", args)

debug_header("EXAMINING OUTPUTS")
print(str(output_list))
debug_header("EXAMINING AVAILABLE OBJECTS IN GRASS")
list_all_loaded_objs(T)


# Managing outputs

# Write capacity table
write.csv(output_list$capacityTable, 
          file = paste0(output_dir, "/", input_region, "_coverage_analysis_", input_f_subset_col, ".csv"))

# Write zonal table
if (zonalCoverage) {
  write.csv(output_list$zonalTable,
            file = paste0(output_dir, "/", input_region, "_zonal_stats_", input_f_subset_col, ".csv"))
}

# Check on catchment?

# Export pop raster
execGRASS("r.out.gdal", parameters=list(input=output_list$popResidualRaster,
                                        output=paste0(output_dir, "/", input_region,
                                                      "_pop_resid_", input_f_subset_col, ".img"),
                                        createopt="COMPRESSED=YES",
                                        format="HFA"),
          flags=c("overwrite", "f", "c", "m"))
report_popresid <- execGRASS("r.report", map=output_list$popResidualRaster, units=c("k","c", "p"), intern=T)
write.table(report_popresid,
            file = paste0(output_dir, "/", input_region, "_pop_resid_", input_f_subset_col, "report.txt"),
            row.names = F, quote=FALSE)


