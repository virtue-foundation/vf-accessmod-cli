
#    AccessMod 5 refactor for Virtue Foundation
#    re-writing features of AccessMod 5 as streamlined R functions accessible from command line

# Accessibility Analysis
# Inputs:
### merged land cover raster
### scenario table (travel speeds associated with different modes of transport)
### health facilities vector
### maximum travel time

# Outputs:
# Speed raster of class "speed": Raster format layer containing the spatial distribution of the speeds used in the analysis. The values in the raster correspond to a composite suite of numbers. The first number is the code for the travelling mode (1: walking; 2: bicycling; 3: motorized). The six following numbers is the speed in km/h, with the first 3 numbers for the integer part, and the three following ones for the decimal part. As an example, the number "2012500" corresponds to bicycle with a speed of 12.5 km/h.

########################## TO-DO

# TO-DO: INPUT VALIDATION: facilities on barrier
# TO-DO: MAIN FUNCTION: check each arg, refactor?
# TO-DO: OUTPUT REFACTOR: check which intermediate outputs are useful for geo coverage
# TO-DO: OUTPUT: output directories

# DONE: PREP: Subset facilities
# DONE: MAIN FUNCTION: iso/aniso
# DONE: MAIN FUNCTION: knights move option
# DONE: MAIN FUNCTION: knights move glitch with r.cost
# DONE: OUTPUT REFACTOR: organize functions into one call
# DONE: OUTPUT: output names

########################## Setup
#!/usr/bin/env Rscript
source("config.R")
source("functions_accessibility.R")

option_list = list(
  # Inputs
  make_option("--lcv", type="character", default=NULL, 
              help="Path to merged land cover file (required)", metavar="character"),
  make_option("--dem", type="character", default=NULL, 
              help="Path to elevation file (required)", metavar="character"),
  make_option("--scenarios", type="character", default=NULL,
              help="Path to scenario table file (required)", metavar="character"),
  make_option("--facilities", type="character", default=NULL,
              help="Path to scenario table file (required)", metavar="character"),
  # Required Parameters
  make_option("--name", type="character", default=NULL,
              help="Name of the region / output save  name (required)", metavar="character"),
  make_option("--analysis_type", type="character", default=NULL,
              help="Type of the analysis (aniso/isotropic) (required)", metavar="character"),
  
  # Optional parameters
  make_option("--max_time", type="double", default=0,
              help="Maximum duration of travel considered", metavar="character"),
  make_option("--facilities_subset", type="character", default="all",
              help="Name of boolean facilities column for inclusion in subset", metavar="character"),
  make_option("--knights_move", type="character", default=F, action="store_true",
              help="Allow knight's move for traversing the landscape", metavar="character"),
  make_option("--output_dir", type="character", default=".", 
              help="output directory path", metavar="character"),
  
  # Diagnostic
  make_option("--debug_print", type="character", default=T, action="store_true",
              help="Print diagnostic info to std-out", metavar="character")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
required_inputs <- c("lcv", "dem", "scenarios", "facilities", "name", "analysis_type")
missing_inputs <- setdiff(required_inputs, names(opt))
if(length(missing_inputs)>0) {
  print("Check your input args")
  stop()
}

##### Parsing inputs
path_lcv <- clean_filepath(opt$lcv)
path_dem <- clean_filepath(opt$dem)
path_scenarios <- clean_filepath(opt$scenarios)
path_facilities <- clean_filepath(opt$facilities)

# Parsing options
output_region <- opt$name
analysis_type <- opt$analysis_type
if(!(analysis_type %in% c("isotropic", "iso", "anisotropic", "aniso"))) {
  print("Check your input args: analysis type must be one of: isotropic, iso, anisotropic, aniso")
  stop()
}
max_time <- opt$max_time
facilities_subset <- opt$facilities_subset
knights_move <- opt$knights_move
output_dir <- clean_filepath(opt$output_dir)
debug_print <- opt$debug_print

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
  execGRASS("g.region", parameters=list(raster="r_merged_lcv"), flags=c("m", "a"))
  
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

# Import facilities
if(is_loaded("v_hf")) {
  print("Health facilities vector is already loaded")
} else {
  import_layer(path=path_facilities, layer_name="v_hf", type="vector", ignore_proj=T)
}

# Getting facilities table
b <- execGRASS("db.select", parameters=list(table="v_hf"), intern=TRUE)
con <- textConnection(b)
tableFacilities <- read.delim(con, header=TRUE, sep="|")
close(con)

# Subset facilities based on boolean column in table
if (facilities_subset != "all") {
  print(facilities_subset)
  inputHfFinal <- amFacilitiesSubset(
    tableFacilities = tableFacilities,
    inputFacilities = "v_hf",
    select_col = facilities_subset
  )
} else {
  inputHfFinal <- "v_hf"
}

# Import scenario table
t_scenarios <- read.csv(path_scenarios, check.names=FALSE)
names(t_scenarios) <- c("class", "label", "speed", "mode")

########################## Validation

# Facilities on barrier (or speed 0)

validated_hf <- amValidateFacilitiesTable(tblHf = inputHfFinal, mapMerged = "r_lcv", mapDem = "r_dem", tblSpeed = scenario_table)
if(any(validated_hf$amOnZero)) {
  stop("Some facilities are on zero-speed positions on the land cover, please check")
}

if(any(validated_hf$amOnBarrier)) {
  stop("Some facilities are on positions in the land cover that are classified as a barrier, please check")
}


########################## MAIN FUNCTION

# Set up switches for speed/friction and travel time rasters/reports
fn_speed_friction <- switch(analysis_type, isotropic = "amCreateFrictionMap", anisotropic = "amCreateSpeedMap")
bln_speed_or_friction <- switch(analysis_type, isotropic = "friction", anisotropic = "speed")
bln_knights_or_blank <- ifelse(knights_move, "knights", "")
name_speed_friction_raster <- paste0("r_", bln_speed_or_friction)

args_speed_friction <- switch(analysis_type, 
                              isotropic = list(tbl = t_scenarios, mapMerged = "r_merged_lcv",
                                               mapFriction = name_speed_friction_raster, mapResol = gmeta()$nsres),
                              anisotropic = list(tbl = t_scenarios, mapMerged = "r_merged_lcv",
                                                 mapSpeed = name_speed_friction_raster)
)

fn_traveltime <- switch(analysis_type, isotropic = "amIsotropicTravelTime", anisotropic = "amAnisotropicTravelTime")
args_traveltime <- switch(analysis_type,
                          isotropic = list(inputFriction = "r_friction", inputHf = inputHfFinal, inputStop = NULL,
                                           inputCoord = NULL, outputDir = NULL, 
                                           outputTravelTime = "r_traveltime",
                                           outputNearest = NULL, maxTravelTime = max_time, maxSpeed = 0, minTravelTime = NULL,
                                           timeoutValue = -1L, getMemDiskRequirement = FALSE, ratioMemory = 1, memory=NULL,
                                           rawMode=FALSE, knights_move = knights_move),
                          anisotropic = list(inputSpeed = "r_speed", inputHf = inputHfFinal, inputStop = NULL,
                                             inputCoord = NULL, outputDir = NULL, 
                                             outputTravelTime = "r_traveltime",
                                             outputNearest = NULL, towardsFacilities = T, maxTravelTime = max_time, minTravelTime = NULL,
                                             maxSpeed = 0, timeoutValue = -1L, getMemDiskRequirement = FALSE, ratioMemory = 1,
                                             memory = NULL, rawMode = FALSE, knights_move = knights_move)
)

filename_speed_friction_raster <- paste(output_region, bln_speed_or_friction, ".img", sep="_")
filename_speed_friction_report <- paste(output_region, bln_speed_or_friction, "report.txt", sep="_")
filename_traveltime_raster <- paste0(paste(output_region, "travel_time", facilities_subset, sep="_"), ".img")
filename_traveltime_report <- paste(output_region, facilities_subset, "travel_time.txt", sep="_")
dir_output_speed_friction <- paste("raster", bln_speed_or_friction, analysis_type, sep="_")
dir_output_speed_friction <- paste0(output_dir, "/")
dir_output_traveltime <- paste0(output_dir, "/")
# dir.create(dir_output_speed_friction)
# dir.create(dir_output_traveltime)

# Run main speed/friction fn
do.call(fn_speed_friction, args = args_speed_friction)
execGRASS("r.out.gdal", parameters=list(input=name_speed_friction_raster,
                                        output=paste(dir_output_speed_friction, filename_speed_friction_raster, sep=""),
                                        createopt="COMPRESSED=YES",
                                        format="HFA"),
          flags=c("overwrite", "f", "c", "m"))
report_speed_friction <- execGRASS("r.report", map = name_speed_friction_raster, units=c("k", "p"), intern=T)
write.table(report_speed_friction, file=paste(dir_output_speed_friction, filename_speed_friction_report, sep=""),
            row.names=F, quote=F)

# Run main travel time fn
do.call(fn_traveltime, args=args_traveltime)
execGRASS("r.out.gdal", parameters=list(input="r_traveltime",
                                        output=paste(dir_output_traveltime, filename_traveltime_raster, sep=""),
                                        createopt="COMPRESSED=YES",
                                        format="HFA"),
          flags=c("overwrite", "f", "c", "m"))
report_traveltime <- execGRASS("r.report", map="r_traveltime", units=c("k","c", "p"), intern=T)
write.table(report_traveltime, file=paste(dir_output_traveltime, filename_traveltime_report, sep=""),
            row.names = F, quote=FALSE) 




