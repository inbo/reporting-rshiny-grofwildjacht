# Create data
#
# Functions should only be called when background data changed
# 
# Author: mvarewyck
###############################################################################

#' Create all shape data from geojson files
#' 
#' @param jsonDir character, path to json shape files
#' @inheritParams loadRawData
#' @param tolerance numeric, defines the tolerance in the Douglas-Peuker algorithm;
#' larger values will impose stronger simplification; default value is 0.001
#' 
#' @return boolean, whether file is successfully saved;
#' save to \code{bucket} list with spatial list for WBE and non-WBE level.
#' Each contain a list with for each spatial level a SpatialPolygonsDataFrame object, 
#' with polygons and data as provided in the \code{jsonDir}; spatial levels are 
#' (1) flanders, provinces, communes, faunabeheerzones, fbz_gemeentes, utm5, utm1
#' and provincesVoeren (Voeren as separate province)
#' (2) WBE_buitengrenzen and WBE
#' @importFrom sp CRS spTransform SpatialPolygonsDataFrame
#' @importFrom methods slot
#' @importFrom maptools unionSpatialPolygons spRbind
#' @importFrom rgdal readOGR
#' @importFrom rgeos gSimplify
#' @importFrom shiny incProgress
#' @importFrom raster area
#' @importFrom utils write.csv read.csv
#' @importFrom aws.s3 s3save
#' @importFrom config get
#' 
#' @examples 
#' \dontrun{createShapeData()}
#' @export
createShapeData <- function(
  jsonDir = "~/git/reporting-rshiny-grofwildjacht/data", 
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  tolerance = 0.0001) {
  
  
  allLevels <- c("Vlaanderen" = "flanders", "Provincies" = "provinces", 
    "Gemeenten" = "communes", "FBZ" = "faunabeheerzones", "FBDZ" = "fbz_gemeentes",
    "UTM5" = "utm5")
  
  # WBE per year
  wbeLevels <- gsub(".geojson", "", list.files(path = jsonDir, pattern = "WBE_binnengrenzen_"))
  # jachtterein per shared year
  jachtLevels <- gsub(".geojson", "", list.files(path = jsonDir, pattern = "Jachtter_"))
  
  allLevels <- c(allLevels, wbeLevels, jachtLevels)
  
  ## New code for geojson files
  spatialData <- lapply(allLevels, function(iLevel) {
      
      file <- file.path(jsonDir, paste0(iLevel, ".geojson"))
#        # Check whether we can use readOGR()
#        "GeoJSON" %in% rgdal::ogrDrivers()$name
      shapeData <- readOGR(dsn = file, verbose = TRUE)
      shapeData <- sp::spTransform(shapeData, CRS("+proj=longlat +datum=WGS84"))
      
      # Create factor for region names
      if (iLevel == "provinces") {
        
        shapeData$NAAM <- factor(shapeData$NAAM, levels = c("West-Vlaanderen",
            "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg")) 
        
      } else if (iLevel == "faunabeheerzones") {
        
        shapeData$NAAM <- factor(shapeData$Code)
        
      } else if (iLevel == "fbz_gemeentes") {
        
        # Create fbz_gemeente
        shapeData$NAAM <- factor(paste0(shapeData$Code, "_", shapeData$NAAM))
        
      } else if (iLevel == "utm5") {
        
        shapeData$NAAM <- factor(shapeData$TAG)
        
      } else if (grepl("WBE_binnengrenzen", iLevel)) {
        
        shapeData$NAAM <- factor(shapeData$WBE_NR)
        
      } else if (grepl("Jachtter_", iLevel)) {
        
        shapeData$NAAM <- factor(shapeData$WBE_NR_wbe)
        
      }
      
      return(shapeData)
      
    })
  
  # Rename WBE levels  
  allLevels <- gsub("WBE_binnengrenzen", "WBE_buitengrenzen", allLevels)
  allLevels[grepl("Jachtter", allLevels)] <- 
    sapply(strsplit(allLevels[grepl("Jachtter", allLevels)], split = "-"), function(x) x[1])
  allLevels <- gsub("Jachtter", "WBE", allLevels)
  
  names(spatialData) <- allLevels
  
  
  ## Create "province" Voeren
  
  # Define provinces based on NIS codes
  provinceIds <- substr(spatialData$communes$NISCODE, start = 1, stop = 1)
  # Give Voeren unique code, different from any other province
  voerenId <- which(spatialData$communes$NAAM == "Voeren")
  provinceIds[voerenId] <- 100
  # Select Limburg and Voeren
  isLimburg <- provinceIds %in% c(7, 100)
  isLimburgProvince <- spatialData$provinces$NAAM == "Limburg"
  
  # Create new polygon for Limburg
  limburgPolygon <- unionSpatialPolygons(SpP = spatialData$communes[isLimburg,],
    IDs = provinceIds[isLimburg])
  limburgData <- spatialData$provinces@data[isLimburgProvince, ]
  voerenData <- limburgData
  voerenData$NAAM <- "Voeren"
  voerenData$NISCODE <- spatialData$communes@data$NISCODE[spatialData$communes@data$NAAM == "Voeren"]
  
  # Bind all province polygons and data
  allPolygons <- spRbind(spatialData$provinces[!isLimburgProvince, ],
    limburgPolygon)
  tmpData <- rbind(spatialData$provinces@data[!isLimburgProvince, ],
    voerenData, limburgData)
  rownames(tmpData) <- sapply(slot(allPolygons, "polygons"), function(x) slot(x, "ID")) 
  
  newProvinceData <- SpatialPolygonsDataFrame(Sr = allPolygons,
    data = tmpData)
  
  # Attach new province data to spatialData
  spatialData$provincesVoeren <- newProvinceData
  
  
  
  
  newNames <- names(spatialData)
  
  # Try to simplify polygons
  spatialData <- lapply(names(spatialData), function(iName) {
      
      iData <- spatialData[[iName]]
      
      # Calculate area for each polygon
      iData@data$AREA <- raster::area(iData)/1e06
      
      # No simplification
      if (iName %in% c("fbz_gemeentes", "utm5") | grepl("WBE_[[:digit:]]", iName))
        return(iData)
      
      simpleShapeData <- gSimplify(spgeom = iData, tol = tolerance)
      
      if (length(simpleShapeData) != length(iData))
        stop("The number of polygons in original shapeData for ", 
          iName, " is: ", length(iData),
          "\nThe number of polygons in simplified shapeData is: ", length(simpleShapeData),
          "\nPlease decrease value for tolerance")
      
      iData <- SpatialPolygonsDataFrame(Sr = simpleShapeData, 
        data = data.frame(iData@data, stringsAsFactors = FALSE))
      
      
      return(iData)
      
    })
  
  names(spatialData) <- newNames
  
  
  # Update commune names for later matching: geo/wildschade data with shape data
  # Note: You can use gemeentecode.csv for matching NIS to NAAM
  # First install the package again!
  gemeenteData <- loadGemeentes(bucket = bucket)
  
  communeData <- spatialData$communes@data
  gemeenteData$Gemeente <- communeData$NAAM[match(gemeenteData$NIS.code, communeData$NISCODE)]
  gemeenteFile <- file.path(tempdir(), "gemeentecodes.csv")
  write.csv(gemeenteData, file = gemeenteFile, row.names = FALSE)
  writeS3(dataFiles = gemeenteFile, bucket = bucket)
  
  # IF any NIS code not in gemeenteData -> throw error
  if (any(!spatialData$communes@data$NISCODE %in% gemeenteData$NIS.code))
    stop("Sommige NIS codes in shape data zijn niet gekend voor matching\n",
      "Gelieve het referentiebestand gemeentecodes.csv aan te vullen")
  
  
  # Save WBE data separately
  spatialDataWBE <- spatialData[grep("WBE", names(spatialData))]
  s3save(spatialDataWBE, bucket = bucket, object = "spatialDataWBE.RData")
  
  spatialData <- spatialData[grep("WBE", names(spatialData), invert = TRUE)]
  s3save(spatialData, bucket = bucket, object = "spatialData.RData")
  
  return(TRUE)
  
}




#' Enrich waarnemingen data with CELLCODE and gemeentecode
#' 
#' @param dataFile path to read current waarnemeningen data
#' @inheritParams createShapeData
#' @return boolean, whether file is successfully saved
#' 
#' @author mvarewyck
#' @importFrom sf st_as_sf st_transform
#' @importFrom data.table fread
#' 
#' @examples 
#' \dontrun{createWaarnemingenData()}
#' @export
createWaarnemingenData <- function(
  dataFile = "~/git/reporting-rshiny-grofwildjacht/data/waarnemingen_2022.csv",
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))) {
  
  
  waarnemingen <- data.table::fread(
    dataFile, 
    select = c("jaar", "NAAM", "TAG", "aantal"),
    dec = ","
  )
  colnames(waarnemingen) <- c("afschotjaar", "gemeente_afschot_locatie", "UTM5", "aantal") 
  
  waarnemingen <- waarnemingen[, c("wildsoort", "dataSource") := list("Wild zwijn", "waarnemingen.be")]
  
  waarnemingenFile <- file.path(tempdir(), "waarnemingen_wild_zwijn_processed.csv")
  write.csv(waarnemingen, file = waarnemingenFile, row.names = FALSE)
  writeS3(dataFiles = waarnemingenFile, bucket = bucket)
  
  
  return(TRUE)   
  
}


#' Create shape data for dashboard wild zwijn - verkeer (traffic)
#' @inheritParams createShapeData
#' @return boolean, whether file is successfully saved
#'  
#' @author mvarewyck
#' @importFrom sf st_read
#' 
#' @examples 
#' \dontrun{createTrafficData()}
#' @export
createTrafficData <- function(jsonDir = "~/git/reporting-rshiny-grofwildjacht/data",
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))) {
  
  trafficData <- list(
    ecorasters = st_read(dsn = file.path(jsonDir, "wildrasters.shp")), #F06_1
    oversteek = st_read(dsn = file.path(jsonDir, "oversteekplaatsen.shp")) #F06_2 & F06_3
  )
  
  s3save(trafficData, bucket = bucket, object = "trafficData.RData")
  
  return(TRUE)
  
}




#' Create shape data for dashboard wild zwijn - future spread F17_4
#' 
#' @inheritParams createShapeData
#' @param spatialData list, with spatialPolygonsDataFrame for each spatial level 
#' @return boolean, whether file is successfully saved
#' save list of SpatialPolygonsDataFrame for each spatial level (pixels and municipalities)
#' as used in \code{\link{mapSpread}}
#' 
#' @author mvarewyck
#' 
#' @examples 
#' \dontrun{createSpreadData()}
#' @export
createSpreadData <- function(
  jsonDir = "~/git/reporting-rshiny-grofwildjacht/data",
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  spatialData = NULL) {
  
  if (is.null(spatialData))
    readS3(file = "spatialData.RData")
  
  # currently only unit of interest
  unit <- "model_EP"
  
  tmpFiles <- list(
    # pixels
    pixels = "Pixels_ModelOutput_toekomst_verspr",
    # gemeente
    municipalities = "Municipalities_ModelOutput_toekomst_verspr"
  )
  
  spatialFiles <- unlist(sapply(names(tmpFiles), function(spatialLevel) {
      
      spatialFile <- sort(grep(tmpFiles[[spatialLevel]], list.files(jsonDir, pattern = ".shp"), value = TRUE))
      # Most recent 2 files
      toReturn <- tail(spatialFile, n = 2)
      names(toReturn) <- paste0(spatialLevel, "_",
        sapply(strsplit(tools::file_path_sans_ext(spatialFile), split = "_"), function(x)
                tail(x, n = 1)))
      
      toReturn
            
    }, USE.NAMES = FALSE))
  
  spreadData <- sapply(names(spatialFiles), function(iFile) {
      
      unitChoices <- if (grepl("pixels", iFile))
          c("Mdl_EP_", "Mdl_OH_", "Rsc_ExP", "Rsc_OpH") else
          c("M_EP_A_", "M_OH_A_", "M_EP__G_", "M_OH__G_")
      unitVariable <- unitChoices[match(unit, c("model_EP", "model_OH", "risk_EP", "risk_OH"))]
      
      baseMap <- rgdal::readOGR(file.path(jsonDir, spatialFiles[iFile])) %>%
        sp::spTransform(CRS("+proj=longlat +datum=WGS84"))
      
      # Modify data
      ## Risico
      riskLevels <- c("Hoog risico", "Gemiddeld risico", "Laag risico", "Verwaarloosbaar risico") 
      if (grepl("pixels", iFile)) {
        baseMap$Rsc_ExP <- factor(baseMap$Rsc_ExP, levels = riskLevels)
        baseMap$Rsc_OpH <- factor(baseMap$Rsc_OpH, levels = riskLevels)
      } else {
        baseMap$M_EP__G_ <- factor(baseMap$M_EP__G_, levels = riskLevels)
        baseMap$M_OH__G_ <- factor(baseMap$M_OH__G_, levels = riskLevels)
      }
      
      # Outcome
      modelShape <- subset(baseMap, !is.na(baseMap@data[, unitVariable]))
      modelShape[[unitVariable]] <- as.factor(modelShape[[unitVariable]])
      
      modelShape$outcome <- modelShape[[unitVariable]]
        
      # Start
      startVariable <- switch(unitVariable,
        Mdl_EP_ = "Strt_EP",
        Mdl_OH_ = "Strt_OH",
        NULL
      )
      
      if (!is.null(startVariable))
        modelShape$start <- modelShape[[startVariable]]
      
      modelShape@data <- modelShape@data[, c(if (grepl("pixels", iFile)) "ID" else "Communs", 
          "outcome", 
          if (!is.null(startVariable)) "start")]
      
      # Add Voeren
      if (grepl("municipalities", iFile)) {
        voerenShape <- spatialData$communes[spatialData$communes@data$NAAM == "Voeren", ]
        voerenShape@data <- data.frame(Communs = "Voeren", outcome = "Al aanwezig")
        modelShape <- rbind(modelShape, voerenShape)
      }
      
      attr(modelShape, "unit") <- unit
      attr(modelShape, "spatialLevel") <- strsplit(iFile, split = "_")[[1]][1]
      attr(modelShape, "year") <- strsplit(iFile, split = "_")[[1]][2]
      
      modelShape
      
    })
    
    s3save(spreadData, bucket = bucket, object = "spreadData.RData")
    
    return(TRUE)

} 

