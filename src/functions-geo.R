
#    AccessMod 5 refactor for Virtue Foundation
#    re-writing features of AccessMod 5 as streamlined R functions accessible from command line

#' amCapacityAnalysis
#' @export
amCapacityAnalysis <- function(
  preAnalysis = FALSE,
  inputPop,
  inputMerged,
  inputHf,
  tableFacilities,
  tableScenario,
  inputZoneAdmin = NULL,
  outputSpeed,
  outputFriction,
  outputPopResidual,
  outputHfCatchment,
  outputPopBarrier,
  outputTableZonal,
  outputTableCapacity,
  idHfOrderField,
  removeCapted = FALSE,
  vectCatch = FALSE,
  popOnBarrier = FALSE,
  typeAnalysis,
  towardsFacilities,
  maxTravelTime,
  useMaxSpeedMask = FALSE,
  maxTravelTimeOrder = NULL,
  radius,
  hfIdx,
  nameField,
  capField = NULL,
  ignoreCapacity = FALSE,
  addColumnPopOrigTravelTime = FALSE,
  addColumnsPopCoverageExtended = FALSE,
  orderField = NULL,
  zonalCoverage = FALSE,
  zoneFieldId = NULL,
  zoneFieldLabel = NULL,
  hfOrder = c("tableOrder", "travelTime", "circlBuffer"),
  hfOrderSorting = c("hfOrderDesc", "hfOrderAsc"),
  outdir,
  debug_print = FALSE
) {

  #
  # Set default
  #

  # if cat is set as index, change to cat_orig
  if (hfIdx == config$vector_key) {
    hfIdxNew <- paste0(config$vector_key, "_orig")
  } else {
    hfIdxNew <- hfIdx
  }

  orderResult <- data.frame(
    id = character(0),
    value = numeric(0)
  )

  # Labels
  labelField <- "amLabel"

  # Set maxSpeed
  maxSpeed <- ifelse(isTRUE(useMaxSpeedMask), max(tableScenario$speed), 0)
  
  # # We would subset the table here, but it is already being subset prior to calling this fn
  # (Also this code is old and we should use amFacilitiesSubset())
  # tableFacilities <- tableFacilities[
  #   vapply(tableFacilities$amSelect, isTRUE, FALSE),
  # ]

  #
  # Create friction / Speed map
  #
  if (!isTRUE(preAnalysis)) {
    switch(typeAnalysis,
      "anisotropic" = {
        amCreateSpeedMap(
          tableScenario,
          inputMerged,
          outputSpeed
        )
      },
      "isotropic" = {
        amCreateFrictionMap(
          tableScenario,
          inputMerged,
          outputFriction,
          mapResol = gmeta()$nsres
        )
      }
    )
  }

  #
  # Compute hf processing order
  #

  if (hfOrder == "tableOrder" || isTRUE(preAnalysis)) {

    #
    # order by given field value, take index field values
    #

    orderResult <- tableFacilities[orderField] %>%
      order(
        decreasing = hfOrderSorting == "hfOrderDesc"
      ) %>%
      tableFacilities[., c(hfIdx, orderField)]
  } else {

    #
    # Do a pre analysis to sort hf with population coverage
    #


    # extract population under max time/distance
    preAnalysisResult <- amCapacityAnalysis(
      preAnalysis = TRUE,
      inputPop = inputPop,
      inputHf = inputHf,
      tableFacilities = tableFacilities,
      tableScenario = tableScenario,
      outputSpeed = outputSpeed,
      outputFriction = outputFriction,
      outputPopResidual = "tmp_nested_p",
      outputHfCatchment = "tmp_nested_catch",
      typeAnalysis = ifelse(hfOrder == "circBuffer", "circular", typeAnalysis),
      towardsFacilities = towardsFacilities,
      radius = radius,
      maxTravelTime = maxTravelTimeOrder,
      hfIdx = hfIdx,
      nameField = nameField,
      capField = capField,
      ignoreCapacity = ignoreCapacity,
      removeCapted = FALSE,
      vectCatch = FALSE,
      orderField = hfIdx,
      hfOrder = "tableOrder",
      hfOrderSorting = "hfOrderAsc"
    )

    #
    # get popTimeMax column from capacity table
    #
    preAnalysisResult <- preAnalysisResult[[
    "capacityTable"
    ]][
      c(
        hfIdx,
        "amPopTravelTimeMax"
      )
    ]
    


    #
    # order by given field value, take index field values
    #
    orderPosition <- order(
      preAnalysisResult$amPopTravelTimeMax,
      decreasing = isTRUE(hfOrderSorting == "hfOrderDesc")
    )
    orderResult <- preAnalysisResult[orderPosition, ]
  }

  amOrderField <- switch(hfOrder,
    "tableOrder" = sprintf(
      "amOrderValues_%s",
      amSubPunct(orderField)
    ),
    "circBuffer" = sprintf(
      "amOrderValues_popDistance%sm",
      radius
    ),
    "travelTime" = sprintf(
      "amOrderValues_popTravelTime%smin",
      maxTravelTimeOrder
    )
  )

  names(orderResult) <- c(
    hfIdx,
    amOrderField
  )
  
  orderId <- orderResult[[hfIdx]]


  # temp. variable
  tmpHf <- "tmp__h" # vector hf tmp
  tmpCost <- "tmp__c" # cumulative cost tmp
  tmpPop <- "tmp__p" # population catchment to substract
  tblOut <- data.frame() # empty data frame for storing capacity summary
  tblPopByZone <- data.frame()
  inc <- 100 / length(orderId) # init increment for progress bar
  incN <- 0 # init counter for progress bar
  tmpVectCatchOut <- NA
  


  # create residual population
  amInitPopResidual(
    inputPopResidual = inputPop,
    inputFriction = outputFriction,
    inputSpeed = outputSpeed,
    outputPopResidual = outputPopResidual
  )
  



  #
  # Population on barrier : map and stat
  #
  if (popOnBarrier && !preAnalysis) {
    amMapPopOnBarrier(
      inputPop = inputPop,
      inputFriction = outputFriction,
      inputSpeed = outputSpeed,
      outputMap = outputPopBarrier
    )
  }
  



  #
  # Start loop on facilities according to defined order
  #
  # if (debug_print) print("facility processing order:")
  # if (debug_print) print(orderId)
  
  for (i in orderId) {
    print(paste("Examining facility", i))
    #
    # Increment
    #
    incN <- incN + 1

    #
    # extract capacity and name
    #
    hfCap <- ifelse(
      test = ignoreCapacity,
      yes = 0,
      no = sum(tableFacilities[tableFacilities[hfIdx] == i, capField])
    )
    #
    hfName <- tableFacilities[tableFacilities[hfIdx] == i, nameField]



    #
    # extract temporary facility point
    #
    rmVectIfExists(tmpHf)
    execGRASS(
      "v.extract",
      flags = "overwrite",
      input = inputHf,
      where = sprintf(" %1$s = '%2$s'", hfIdx, i),
      output = tmpHf
    )

    #
    # compute cumulative cost map
    #

    switch(typeAnalysis,
      "anisotropic" = amAnisotropicTravelTime(
        inputSpeed = outputSpeed,
        inputHf = tmpHf,
        outputTravelTime = tmpCost,
        towardsFacilities = towardsFacilities,
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        timeoutValue = "null()"
      ),
      "isotropic" = amIsotropicTravelTime(
        inputFriction = outputFriction,
        inputHf = tmpHf,
        outputTravelTime = tmpCost,
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        timeoutValue = "null()"
      ),
      "circular" = amCircularTravelDistance(
        inputHf          = tmpHf,
        outputBuffer     = tmpCost,
        radius           = radius
      )
    )


    #
    # Catchment analysis
    #

    listSummaryCatchment <- amCatchmentAnalyst(
      inputMapTravelTime = tmpCost,
      inputMapPopInit = inputPop,
      inputMapPopResidual = outputPopResidual,
      outputCatchment = outputHfCatchment,
      facilityCapacityField = capField,
      facilityCapacity = hfCap,
      facilityLabelField = labelField,
      facilityLabel = NULL,
      facilityIndexField = hfIdx,
      facilityId = i,
      facilityNameField = nameField,
      facilityName = hfName,
      maxTravelTime = maxTravelTime,
      ignoreCapacity = ignoreCapacity,
      addColumnPopOrigTravelTime = addColumnPopOrigTravelTime,
      iterationNumber = incN,
      removeCapted = removeCapted,
      vectCatch = vectCatch
    )

    # get actual file path to catchment
    tmpVectCatchOut <- listSummaryCatchment$amCatchmentFilePath

    # Add row to output table
    if (incN == 1) {
      tblOut <- listSummaryCatchment$amCapacitySummary
    } else {
      tblOut <- rbind(
        tblOut,
        listSummaryCatchment$amCapacitySummary
      )
    }

  } # end of loop

  # if (debug_print) print(names(orderResult))
  # if (debug_print) print(names(tblOut))
  # merge ordering by column,circle or travel time with the capacity analysis
  tblOut <- merge(orderResult, tblOut, by = hfIdx)
  tblOut <- tblOut[order(tblOut$amOrderComputed), ]

  if (popOnBarrier && addColumnsPopCoverageExtended) {
    nOnBarrier <- amGetRasterStat(outputPopBarrier, "sum")
    #
    # Case when output pop is full of nodata
    #
    if (isEmpty(nOnBarrier)) {
      nOnBarrier <- 0
    }
    tblOut["amPopTotalOnBarrier"] <- nOnBarrier
  }

  colOrder <- c(
    hfIdx,
    nameField,
    if (!ignoreCapacity) capField,
    amOrderField,
    "amOrderComputed",
    "amTravelTimeMax",
    "amPopTravelTimeMax",
    if (addColumnPopOrigTravelTime) "amPopOrigTravelTimeMax",
    "amCorrPopTime",
    "amTravelTimeCatchment",
    "amPopCatchmentTotal",
    "amCapacityRealised",
    "amCapacityResidual",
    "amPopCatchmentDiff",
    if (addColumnsPopCoverageExtended) "amPopTotal",
    if (addColumnsPopCoverageExtended) "amPopTotalNotOnBarrier",
    if (addColumnsPopCoverageExtended && popOnBarrier) "amPopTotalOnBarrier",
    if (addColumnsPopCoverageExtended) "amPopResidualBefore",
    if (addColumnsPopCoverageExtended) "amPopResidualAfter",
    "amPopCoveredPercent"
  )

  tblOut <- tblOut[, colOrder]


  if (zonalCoverage) {
    #
    # optional zonal coverage using admin zone polygon
    #
    

    execGRASS("v.to.rast",
      input            = inputZoneAdmin,
      output           = "tmp_zone_admin",
      type             = "area",
      use              = "attr",
      attribute_column = zoneFieldId,
      label_column     = zoneFieldLabel,
      flags            = c("overwrite")
    )

    tblAllPopByZone <- execGRASS(
      "r.univar",
      flags  = c("g", "t", "overwrite"),
      map    = inputPop,
      zones  = "tmp_zone_admin",
      intern = T
    ) %>%
      amCleanTableFromGrass(
        cols = c("zone", "label", "sum")
      )

    tblResidualPopByZone <- execGRASS(
      "r.univar",
      flags  = c("g", "t", "overwrite"),
      map    = outputPopResidual,
      zones  = "tmp_zone_admin",
      intern = T
    ) %>%
      amCleanTableFromGrass(
        cols = c("zone", "label", "sum")
      )

    tblPopByZone <- merge(
      tblResidualPopByZone,
      tblAllPopByZone,
      by = c("zone", "label")
    )

    tblPopByZone$covered <- tblPopByZone$sum.y - tblPopByZone$sum.x
    tblPopByZone$percent <- (tblPopByZone$covered / tblPopByZone$sum.y) * 100
    tblPopByZone$sum.x <- NULL
    names(tblPopByZone) <- c(
      zoneFieldId,
      zoneFieldLabel,
      "amPopSum",
      "amPopCovered",
      "amPopCoveredPercent"
    )
  }


  #
  #  move catchment shp related file into one place
  #
  if (vectCatch) {
    print("All the files in the temporary catchment vector dir")
    print(tmpVectCatchOut)
    
    amMoveShp(
      shpFile = tmpVectCatchOut,
      outDir = outdir,
      outName = outputHfCatchment
    )
  }
  
  #
  #  create final pop-resid raster result for output
  #
  execGRASS("g.copy", raster=paste(outputPopResidual, ",r_pop_resid"))

  #
  # finish process
  #

  out <- list(
    capacityTable = tblOut,
    zonalTable = tblPopByZone,
    popResidualRaster = outputPopResidual,
    catchmentVectorLocation = file.path(outdir, outputHfCatchment)
  )


  # if (!preAnalysis) {
  #   #
  #   # Local db connection
  #   #
  #   dbCon <- amMapsetGetDbCon()
  #   on_exit_add({
  #     dbDisconnect(dbCon)
  #   })
  # 
  #   #
  #   # Write summary table in db
  #   #
  #   dbWriteTable(
  #     dbCon,
  #     outputTableCapacity,
  #     tblOut,
  #     overwrite = T
  #   )
  #   #
  #   # Write zonal stat table if exists
  #   #
  #   if (!is.null(tblPopByZone) && nrow(tblPopByZone) > 0) {
  #     dbWriteTable(
  #       dbCon,
  #       outputTableZonal,
  #       tblPopByZone,
  #       overwrite = T
  #     )
  #   }
  # }

  return(out)
}

popOnBarrierCheck <- function(r_pop, r_lcv) {
  if (!is.null(r_pop) & !is.null(r_lcv)) {
    tmpMapPop <- "tmp__test_pop_on_barrier"
    execGRASS("r.mask", flags = "i", raster = r_lcv)
    execGRASS("r.mapcalc",
              flags = "overwrite",
              expression = paste(tmpMapPop, " = ", r_pop, "")
    )
    execGRASS("r.mask", flags = "r")
    
    sumPop <- execGRASS("r.univar",
                        map = tmpMapPop,
                        flags = c("g", "t"),
                        intern = T
    ) %>%
      amCleanTableFromGrass(
        cols = c("non_null_cells", "sum")
      )
    
    origPop <- execGRASS("r.univar",
                         map = r_pop,
                         flags = c("g", "t"),
                         intern = T
    ) %>%
      amCleanTableFromGrass(
        cols = c("sum")
      )
    
    return(
      list(
        sum = round(sumPop$sum, 2),
        cells = sumPop$non_null_cells,
        percent = round(100 * (sumPop$sum / origPop$sum), 2)
      )
    )
  }
  #   }
  return(list())
}

#' Initialize output residual population layer
#'
#' Create the residual population layer (outputPopResidual) where friction layer is greated than 0 (remove population on barrier).
#'
#' @param inputPopResidual Raster layer of initial residual population
#' @param inputFriction Raster layer of speed (or friction)
#' @param inputFriction Raster layer of friction (or speed)
#' @param outputPopResidual Raster layer of population residual to update and return at the end
#' @export
amInitPopResidual <- function(inputPop = NULL,
                              inputPopResidual = NULL,
                              inputFriction = NULL,
                              inputSpeed = NULL,
                              outputPopResidual = NULL) {
  inputTest <- inputFriction
  
  if (isEmpty(inputTest) || !amRastExists(inputTest)) {
    inputTest <- inputSpeed
    if (!amRastExists(inputTest)) {
      stop("amInitPopResidual: no valid test layer")
    }
  }
  
  if (isEmpty(inputPopResidual)) {
    inputPopResidual <- inputPop
  }
  
  expPopResidual <- sprintf(
    "%1$s = if(((%2$s > 0)&&&(%3$s > 0)), %2$s,0)",
    outputPopResidual,
    inputPopResidual,
    inputTest
  )
  
  if (isEmpty(expPopResidual)) {
    stop(
      "Missing the residual layer, something's up"
    )
  }
  
  execGRASS(
    "r.mapcalc",
    expression = expPopResidual,
    flags = "overwrite"
  )

}


#' Set inner ring
#'
#' set residual pop map to zero at given value of travelTime
#'
#' @param inputMapPopResidual Residual population map tu update
#' @param inputMapTravelTime Travel time map
#' @param lowerOrEqualToZone Time limit
amInnerRing <- function(inputMapTravelTime, inputMapPopResidual, lowerOrEqualToZone = 0, value = 0) {
  expInner <- sprintf(
    "%1$s = if( !isnull(%2$s) &&& %2$s <= %3$s, %4$s, %1$s )",
    inputMapPopResidual,
    inputMapTravelTime,
    lowerOrEqualToZone,
    value
  )
  if (isEmpty(expInner)) {
    stop("amInnerRing issue, empty expression!")
  }
  execGRASS("r.mapcalc",
            expression = expInner,
            flags = "overwrite"
  )
}

#' Set outer ring
#'
#' Reduce population at given travel time iso band
#'
#' @param inputMapPopResidual Residual population map tu update
#' @param inputMapTravelTime Travel time map
#' @param lowerOrEqualToZone Time limit
amOuterRing <- function(inputMapTravelTime, inputMapPopResidual, propToRemove = 0, zone = 0) {
  expOuter <- sprintf(
    "%1$s = if( !isnull(%2$s) &&& %2$s == %4$s,  %1$s - %1$s * %3$s, %1$s )",
    inputMapPopResidual,
    inputMapTravelTime,
    propToRemove,
    zone
  )
  if (isEmpty(expOuter)) {
    stop("amOuterRing issue, empty expression!")
  }
  execGRASS("r.mapcalc",
            expression = expOuter,
            flags = "overwrite"
  )
}


#' Compute catchment from a table of cumulated population by cumulated cost map
#' @param inputTablePopByZone Table containing at least zone, sum, and cumSum columns from an zonal analysis between an isotropic or anisotropic cumulative cost layer (travel time) and a population layer.
#' @param inputMapPopInit Name of the layer containing the original population
#' @param inputMapPopResidual Name of the layer containing the residual population (could be the original population but this layer will be modified)
#' @param inputMapTravelTime Name of the layer containing the travel time
#' @param outputCatchment Name of the layer for the output vector catchment
#' @param facilityId Id of the facility analysed
#' @param facilityIndexField Name of the field/column of containing Id's
#' @param facilityName Name of the facility
#' @param facilityNameField Name of field/column containing the facilities name
#' @param facilityCapacity Capacity of the facility
#' @param facilityCapacityField Name of the column containint capacities
#' @param facilityLabel (optional) Label describing the capacity
#' @param facilityLabelField (optional) Name of the column for the label describing the capacity
#' @param iterationNumber Number (integer) of the iteration currently processed. Is used to determine if the shapefile in output should be overwrite or if we append the geometry to it
#' @param maxTravelTime Maximum cost allowed
#' @param ignoreCapacity Ignore capacity, use maximum population.
#' @param removeCapted Should this analysis remove capted population ?
#' @param vectCatch Should this analysis create a shapefile as output ?
#' @return A named list Containing the capacity analysis (amCapacityTable), the path to the shapefile (amCatchmentFilePath) and a message (msg).
#' @export
amCatchmentAnalyst <- function(
    inputTablePopByZone = NULL,
    inputMapTravelTime,
    inputMapPopInit,
    inputMapPopResidual,
    outputCatchment,
    facilityId,
    facilityIndexField,
    facilityName,
    facilityNameField,
    facilityCapacity,
    facilityCapacityField,
    facilityLabel = NULL,
    facilityLabelField,
    iterationNumber,
    maxTravelTime,
    ignoreCapacity = FALSE,
    addColumnPopOrigTravelTime = FALSE,
    removeCapted = TRUE,
    vectCatch = TRUE,
    outdir,
    language = config$language) {
  
  
  #
  # Check input before going further
  #
  if (!ignoreCapacity && isEmpty(facilityCapacity)) {
    stop(sprintf(ams("analysis_catchment_error_capacity_not_valid"), facilityId))
  }
  
  
  #
  # Init variables
  #
  
  # population residual, not covered by this facility
  outputMapPopResidualPatch <- "tmp__map_residual_patch"
  outputMapPopResidual <- inputMapPopResidual
  

  
  # retrieve the path to the catchment
  pathToCatchment <- file.path(tempdir(), paste0(outputCatchment, ".gpkg"))
  
  # travel time / cost map
  travelTime <- inputMapTravelTime
  
  # limit of the travel time map used to create the vector catchment
  timeLimitVector <- as.numeric(NA)
  
  # correlation between the population and the time zone
  #   negative value = covered pop decrease with dist,
  #   positive value = covered pop increase with dist
  corPopTime <- as.numeric(NA)
  
  # popByZone inner ring
  pbzIn <- as.numeric(NA)
  
  # popByZone outer ring
  pbzOut <- as.numeric(NA)
  
  # capacity of the facility at start, will be updated with the capacity not used
  capacityResidual <- facilityCapacity
  
  # capacity realised
  capacityRealised <- as.numeric(NA)
  
  # total pop in catchment area
  popCatchment <- as.numeric(NA)
  
  # total pop in maximum travel time area
  popTravelTimeMax <- as.numeric(NA)
  
  # population of the first zone with at least one indivual
  popTravelTimeMin <- as.numeric(NA)
  
  # total pop in maximum travel time area with original population
  popOrigTravelTimeMax <- as.numeric(NA)
  
  # percent of the initial population not in population residual
  popCoveredPercent <- as.numeric(NA)
  
  # population in the catchment not covered (outer ring residual)
  popNotIncluded <- as.numeric(NA)
  
  # other pop reporting
  popResidualBefore <- as.numeric(NA)
  popResidualAfter <- as.numeric(NA)
  
  # population by zone is empty
  isEmpty <- TRUE
  
  # If pop by zone is not given, extract it
  if (is.null(inputTablePopByZone)) {
    pbz <- amGetRasterStatZonal(
      mapZones = inputMapTravelTime,
      mapValues = inputMapPopResidual
    )
  } else {
    pbz <- inputTablePopByZone
  }
  
  #
  # Total pop under travel time with original population
  #
  if (addColumnPopOrigTravelTime) {
    tryCatch(
      finally = {
        #
        # mask remove
        #
        hasMask <- amRastExists("MASK")
        if (hasMask) {
          execGRASS("r.mask",
                    flags = "r"
          )
        }
      },
      {
        #
        # Set a mask to extract catchment
        #
        execGRASS("r.mask",
                  raster = inputMapTravelTime
        )
        popOrigTravelTimeMax <- amGetRasterStat(inputMapPopInit, "sum")
      }
    )
  }
  
  # check if whe actually have zone
  isEmpty <- isTRUE(nrow(pbz) == 0)
  
  # starting population
  
  popTotal <- amGetRasterStat(inputMapPopInit, "sum")
  popTotalNotOnBarrier <- amGetRasterStat(inputMapPopResidual, "sum")
  popResidualBefore <- amGetRasterStat(inputMapPopResidual, "sum")
  #
  # get stat
  #
  if (!isEmpty) {
    # After cumulated sum, order was not changed, we can use tail/head to extract min max
    popTravelTimeMax <- tail(pbz, n = 1)$cumSum
    # popTravelTimeMin <- head(pbz,n=1)$cumSum
    popTravelTimeMin <- head(pbz[pbz$cumSum > 0, ], n = 1)$cumSum
    
    # if ignore capacity, use all
    if (ignoreCapacity) {
      facilityCapacity <- popTravelTimeMax
      capacityResidual <- popTravelTimeMax
    }
    
    # Check time vs pop correlation :
    corPopTime <- cor(pbz$zone, pbz$sum)
    
    #
    # Last iso band where pop is lower or equal the capacity
    #
    pbzIn <- pbz[pbz$cumSum <= facilityCapacity, ] %>% tail(1)
    if (isEmpty(pbzIn)) {
      #
      # Capacity is lower than pop in first zone
      # popInner is zero in the zoneInner ( first zone )
      #
      popInner <- 0
      zoneInner <- head(pbz, n = 1)$zone
    } else {
      popInner <- pbzIn$cumSum
      zoneInner <- pbzIn$zone
    }
    
    
    #
    # First iso bad where pop is greater than the capacity
    #
    pbzOut <- pbz[pbz$cumSum > facilityCapacity, ] %>% head(1)
    popOuter <- pbzOut$cumSum
    popOuterBand <- pbzOut$sum
    zoneOuter <- pbzOut$zone
    
    
    # Case evaluation.
    # e.g.
    # Capacity = 10 (pop)
    # Time limit = 4 (zone)
    
    #     A         | B           | C         | D
    #     z  p      | z  p        | z  p      | z  p
    #     ----      | ----        | ----      | ----
    #     0 1       | 0 11 ¯ 0;10 | 0 1       | 0 2
    #     1 3       | 1 13        | 1 3       | 1 4
    #     2 5       | 2 20        | 2 4       | 2 9
    #     3 9 _ 9;1 | 3 25        | 3 5       | 3 10 -- 10;0
    #     4 15      | 4 50        | 4 6 _ 6;0 | 4 11
    #
    
    #   case       | A   | B   | C  | D
    #   --------------------------------
    #   inner      | 9   | 0   | 6  | 10
    #   outer      | 1   | 10  | 0  | 0
    #   --------------------------------
    #   residual   | 0   | 0   | 4  | 0
    #   catchment  | out | out | in | in
    #   pop remove |  T  |  F  | T  | T
    #   out ratio  |  T  |  T  | F  | F
    
    # ignore capacity
    isE <- isTRUE(ignoreCapacity)
    
    # D
    # Inner catchment match the facility capacity
    isD <- !isE && isTRUE(facilityCapacity == popInner)
    
    # C
    # Capacity greater than max available pop
    isC <- !isE && isTRUE(facilityCapacity > popTravelTimeMax)
    
    # test for B
    isB <- !isE && isTRUE(facilityCapacity < popTravelTimeMin)
    
    # test for A
    isA <- !isE && isTRUE(popInner > 0 && popOuter > 0)
    
    type <- NULL
    
    if (isE) {
      type <- "E"
      
      timeLimitVector <- maxTravelTime
      facilityCapacity <- popTravelTimeMax
      capacityResidual <- 0
      
      if (removeCapted) {
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )
      }
    } else if (isD) {
      type <- "D"
      
      capacityResidual <- 0
      timeLimitVector <- zoneInner
      
      if (removeCapted) {
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )
      }
    } else if (isC) {
      type <- "C"
      
      capacityResidual <- facilityCapacity - popTravelTimeMax
      timeLimitVector <- zoneInner
      
      if (removeCapted) {
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )
      }
    } else if (isB) {
      type <- "B"
      
      capacityResidual <- 0
      timeLimitVector <- zoneOuter
      
      if (removeCapted) {
        amOuterRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          propToRemove = (facilityCapacity - popInner) / popOuterBand,
          zone = zoneOuter
        )
      }
    } else if (isA) {
      type <- "A"
      
      capacityResidual <- 0
      timeLimitVector <- zoneOuter
      
      
      if (removeCapted) {
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )
        
        amOuterRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          propToRemove = (facilityCapacity - popInner) / popOuterBand,
          zone = zoneOuter
        )
      }
    } else {
      amMsg(
        type = "warning",
        text = paste(
          "amCatchmentAnalyst. Catchment type not found.",
          "facilityId:", facilityId,
          "facilityCapacity:", facilityCapacity,
          "popInner:", popInner,
          "popOuter:", popOuter,
          "popTravelTimeMin:", popTravelTimeMin,
          "popTravelTimeMax:", popTravelTimeMax
        )
      )
    }
    
    # cat("Type", type, " | ", "id", facilityId, "\n" );
    
    #
    # get other value to return
    #
    capacityRealised <- facilityCapacity - capacityResidual
    popCatchment <- max(pbz[pbz$zone <= timeLimitVector, "cumSum"])
    popNotIncluded <- round(popCatchment - capacityRealised, 6)
    
    if (vectCatch) {
      #
      # Extract the catchment as vector
      #
      tryCatch(
        finally = {
          #
          # mask remove
          #
          hasMask <- amRastExists("MASK")
          if (hasMask) {
            execGRASS("r.mask",
                      flags = "r"
            )
          }
        },
        {
          #
          # Set a mask to extract catchment
          #
          execGRASS("r.mask",
                    raster   = inputMapTravelTime,
                    maskcats = sprintf("0 thru %s", timeLimitVector),
                    flags    = c("overwrite")
          )
          #
          # Catchment additional attributes
          #
          aCols <- list()
          aCols[facilityIndexField] <- facilityId
          aCols[facilityNameField] <- facilityName
          aCols["type"] <- type
          
          #
          # extraction process
          #
          amRasterToShape(
            pathToCatchment   = pathToCatchment,
            idField           = facilityIndexField,
            idPos             = facilityId,
            append            = iterationNumber > 1,
            inputRaster       = inputMapTravelTime,
            outputShape       = outputCatchment,
            listColumnsValues = aCols
          )
        }
      )
    }
  }
  
  # Population covered
  #
  # ( 1346 tot - 0 residual ) / 1346 => 100% coverage
  # ( 1346 tot - 673 residual ) / 1346 => 50% coverage
  #
  popResidualAfter <- amGetRasterStat(inputMapPopResidual, "sum")
  popCoveredPercent <- (popTotalNotOnBarrier - popResidualAfter) / popTotalNotOnBarrier * 100
  
  #
  # Output capacity table
  #
  outList <- list(
    amId                     = facilityId,
    amOrderComputed          = iterationNumber,
    amName                   = facilityName,
    amTravelTimeMax          = maxTravelTime,
    amPopTravelTimeMax       = popTravelTimeMax,
    amCorrPopTime            = corPopTime,
    amLabel                  = facilityLabel,
    amCapacity               = facilityCapacity,
    amTravelTimeCatchment    = timeLimitVector,
    amPopCatchmentTotal      = popCatchment,
    amCapacityRealised       = capacityRealised,
    amCapacityResidual       = capacityResidual,
    amPopCatchmentDiff       = popNotIncluded,
    amPopCoveredPercent      = popCoveredPercent,
    amPopTotal               = popTotal,
    amPopTotalNotOnBarrier   = popTotalNotOnBarrier,
    amPopResidualAfter       = popResidualAfter,
    amPopResidualBefore      = popResidualBefore
  )
  
  #
  # renaming table
  #
  names(outList)[names(outList) == "amId"] <- facilityIndexField
  names(outList)[names(outList) == "amName"] <- facilityNameField
  names(outList)[names(outList) == "amLabel"] <- facilityLabelField
  
  if (!ignoreCapacity) {
    names(outList)[names(outList) == "amCapacity"] <- facilityCapacityField
  }
  
  outList <- outList[!sapply(outList, is.null)]
  
  if (addColumnPopOrigTravelTime) {
    outList$amPopOrigTravelTimeMax <- popOrigTravelTimeMax
  }
  
  #
  # Result message
  #
  msg <- sprintf(
    "Completed catchment analysis %1s, %2s percent completed",
    iterationNumber,
    round(popCoveredPercent, 4)
  )
  
  list(
    amCatchmentFilePath = pathToCatchment,
    amCapacitySummary = as.data.frame(outList),
    msg = msg
  )
}

#' Get cumulative sum table based on raster (cell) zonal stat
#'
#' pbz table give the sum of person by travel time iso band
#'
#' @param mapValues Raster for values
#' @param mapZones Raster for zones
amGetRasterStatZonal <- function(mapValues, mapZones) {
  #
  # compute integer version of cumulative cost map to use with r.univar
  #
  ttIsCell <- amRasterMeta(mapZones)[["datatype"]] == "CELL"
  
  if (!ttIsCell) {
    exprIntCost <- sprintf(
      "%1$s = %1$s >= 0 ? round( %1$s ) : null() ",
      mapZones
    )
    execGRASS("r.mapcalc", expression = exprIntCost, flags = "overwrite")
  }
  
  #
  # compute zonal statistic : time isoline as zone
  #
  zStat <- execGRASS(
    "r.univar",
    flags  = c("g", "t", "overwrite"),
    map    = mapValues,
    zones  = mapZones,
    intern = T
  ) %>%
    amCleanTableFromGrass()
  
  #
  # rm na/nan (case when corresponding zone have no value)
  # -> column sum selection is arbitrary, but used in cumSum, which
  # fails when having na/nan value.
  zStat <- zStat[!is.na(zStat$sum), ]
  zStat$cumSum <- cumsum(zStat$sum)
  zStat[c("zone", "sum", "cumSum")]
}


#' Get raster meta info
#'
#' @param {Character} raster Raster layer id
#' @return {data.frame} Raster info:
#' north          south           east           west          nsres
#' "-1632035.586" "-1828035.586"  "811692.6445"  "584692.6445"         "1000"
#'         ewres           rows           cols          cells       datatype
#'        "1000"          "196"          "227"        "44492"         "CELL"
#'         ncats
#'           "0"
#' @export
amRasterMeta <- function(raster = NULL) {
  tblMeta <- execGRASS("r.info",
                       map = raster,
                       flags = "g",
                       intern = T
  ) %>%
    amCleanTableFromGrass(
      sep = "=",
      header = FALSE,
      col.names = c("name", "value")
    )
  out <- tblMeta$value
  names(out) <- tblMeta$name
  return(out)
}

#' amGetRasterStat
#'
#' Extract cells stat using r.univar
#'
#' @param rasterMap grass raster map name
#' @param stats Stat to compute. Should be in c('n','cells','max','mean','stdev','coeff_var','null_cells','min','range','mean_of_abs','variance','sum','percentile')
#' @param quantile Percentiles to extract
#' @return cells stat
#' @export
amGetRasterStat <- function(rasterMap, metric = c("n", "cells", "max", "mean", "stddev", "coeff_var", "null_cells", "min", "range", "mean_of_abs", "variance", "sum", "percentile"), percentile = 99) {
  # validation
  if (!amRastExists(rasterMap)) {
    return()
  }
  stopifnot(length(metric) == 1)
  # set options
  metric <- match.arg(metric)
  # if quantiles use r.quantile
  if (isTRUE("percentile" %in% metric)) {
    val <- amParseOptions(execGRASS("r.quantile", input = rasterMap, percentiles = percentile, intern = T), sepAssign = ":")
  } else {
    val <- amParseOptions(execGRASS("r.univar", map = rasterMap, flags = "g", intern = T))[[metric]]
  }
  val <- as.numeric(val)
  
  if (isTRUE(length(val) == 0 || isEmpty(val))) val <- 0L
  
  return(val)
}

#' amRasterToShape
#'
#' Extract area from raster and create a shapefile or
#' append to it if the files already exist.
#'
#' @param idField Name of the facility id column.
#' @param idPos String id currently processed.
#' @param append Append to existing.
#' @param inputRaster Raster to export
#' @param outCatch Name of shapefile layer
#' @param listColumnsValue Alternative list of value to
#'        put into catchment attributes. Must be a named list.
#' @return Shapefile path
#' @export
amRasterToShape <- function(
    pathToCatchment,
    idField,
    idPos,
    append = FALSE,
    inputRaster,
    outputShape = "tmp__vect_catch",
    listColumnsValues = list(),
    oneCat = TRUE
) {
  #
  # Local db connection
  #
  dbCon <- amMapsetGetDbCon()
  
  idField <- ifelse(
    idField == config$vectorKey,
    paste0(config$vectorKey, "_join"),
    idField
  )
  
  listColumnsValues[idField] <- idPos
  listColumnsValues <- listColumnsValues[
    !names(listColumnsValues) %in% config$vectorKey
  ]
  
  tmpRaster <- amRandomName("tmp__r_to_shape")
  tmpVectDissolve <- amRandomName("tmp__vect_dissolve")
  
  
  execGRASS("g.copy", raster = c(inputRaster, tmpRaster))
  
  if (oneCat) {
    expOneCat <- sprintf("%1$s = !isnull(%1$s) ? 1 : null()", tmpRaster)
    execGRASS("r.mapcalc", expression = expOneCat, flags = "overwrite")
  }
  
  #
  # Export input raster to vector
  #
  execGRASS("r.to.vect",
            input  = tmpRaster,
            output = outputShape,
            type   = "area",
            flags  = c("overwrite")
  )
  
  #
  # Dissolve result to have unique id by feature
  #
  execGRASS("v.dissolve",
            input  = outputShape,
            output = tmpVectDissolve,
            column = "value",
            flags  = c("overwrite")
  )
  
  #
  # Create a table for catchment
  #
  
  execGRASS("v.db.addtable",
            map = tmpVectDissolve,
            columns="length DOUBLE",
            flags=c("verbose")
  )
  
  outPath <- pathToCatchment
  
  # for the first catchment : overwrite if exists, else append.
  if (append) {
    outFlags <- c("a", "m", "s")
  } else {
    # overwrite returns warnings...
    if (file.exists(outPath)) {
      file.remove(outPath)
    }
    outFlags <- c("overwrite", "m", "s")
  }
  #
  # update attributes

  dbRec <- dbGetQuery(
    dbCon,
    sprintf(
      "select * from %s",
      tmpVectDissolve
    )
  )
  
  if (length(listColumnsValues) > 0) {
    for (n in names(listColumnsValues)) {
      dbRec[n] <- listColumnsValues[n]
    }
  } else {
    dbRec[idField] <- idPos
  }
  
  # rewrite
  dbWriteTable(dbCon, tmpVectDissolve, dbRec, overwrite = T)
  
  # export to shapefile.
  execGRASS("v.out.ogr",
            input = tmpVectDissolve,
            output = outPath,
            format = "GPKG",
            flags = outFlags,
            output_layer = outputShape
  )
  rmVectIfExists(tmpVectDissolve)
  rmVectIfExists(outputShape)
  rmRastIfExists(tmpRaster)
  dbDisconnect(dbCon)
    
  return(outPath)
}


#' Create con object for SQLite
#' Based on LOCATION_NAME, GISDBASE env + mapset name or MAPSET env.
#'
#' @param {Character} mapset Optional mapset name. Default is $MAPSET
#' @return {RSQLite} dbCon object
#'
amMapsetGetDbCon <- function(mapset = NULL) {
  gmeta <- gmeta()
  pathGrass <- gmeta$GISDBASE
  location <- gmeta$LOCATION_NAME
  if (isEmpty(mapset)) {
    mapset <- gmeta$MAPSET
  }
  sqlitePath <- file.path(pathGrass, location, mapset, "sqlite/sqlite.db")
  dbCon <- dbConnect(RSQLite::SQLite(), sqlitePath)
  
  if (!dbIsValid(dbCon)) {
    stop("dbCon not valid")
  }
  
  return(dbCon)
}


#' Get am_grass current item
#'
#' @param name {Character} Name of the env variable. E.g. GISRC
#' @return am_grass
amGrassSessionGetEnv <- function(name) {
  amg <- gmeta(ignore.stderr = T)
  print(amg)
  item <- amg[[name]]
  return(item)
}

