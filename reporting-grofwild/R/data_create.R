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




#' Preprocess the raw data files for use in the app
#' 
#' E.g. clean data by dropping unused variables, handle NA and match with shape data
#' 
#' @param dataDir data folder with file to be processed
#' @inheritParams loadRawData
#' @return boolean, whether file is successfully saved
#' 
#' @author mvarewyck
#' @importFrom sf st_as_sf st_transform
#' @importFrom data.table fread
#' @importFrom aws.s3 s3write_using
#' 
#' @examples 
#' \dontrun{createWaarnemingenData()}
#' @export
createRawData <- function(
  dataDir = "~/git/reporting-rshiny-grofwildjacht/dataS3",
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  type = c("eco", "geo", "wildschade", "kbo_wbe", "waarnemingen")) {
  
  dataFile <- switch(type,
    eco = "rshiny_reporting_data_ecology.csv",
    geo = "rshiny_reporting_data_geography.csv",
    wildschade = "WildSchade_georef.csv",
    kbo_wbe = "Data_Partij_Cleaned.csv",
    waarnemingen = "waarnemingen_wild_zwijn.csv"
  )

  
  rawData <- read.csv(file.path(dataDir, dataFile), 
    sep = if (type == "waarnemingen") "," else ";", 
    dec = ",")
  
  
  # Uniform provincie
  if ("provincie" %in% colnames(rawData)) {
    
    rawData$provincie[is.na(rawData$provincie)] <- "Onbekend"
    
    ## Mismatch names with spatial (shape) data for "Vlaams Brabant"
    rawData$provincie[rawData$provincie == "Vlaams-Brabant"] <- "Vlaams Brabant"
    
    ## Only for "Wild zwijn" separate province "Voeren" is considered, otherwise part of "Limburg"
    ## Re-order factor levels for plots
    if ("wildsoort" %in% names(rawData))
      rawData$provincie[rawData$wildsoort != "Wild zwijn" & rawData$provincie == "Voeren"] <-  
        "Limburg" 
    
  }
  
  
  # Gemeente & NIS & postcode
  # Data source: http://portal.openbelgium.be/he/dataset/gemeentecodes
  gemeenteData <- loadGemeentes()
  
  
 if (type == "eco") {
    ## ECO data for grofwild
    
    newLevels <- loadMetaEco()
    
    ## Rename
    rawData$onderkaaklengte_mm <- rawData$lengte_mm
    rawData$lengte_mm <- NULL
    
    # Re-define "Adult" as "Volwassen" for leeftijd
    rawData$leeftijdscategorie_MF[rawData$leeftijdscategorie_MF == "Adult"] <- "Volwassen"
    rawData$leeftijd_comp[rawData$leeftijd_comp == "Adult"] <- "Volwassen"
    
    # Leeftijdscategorie_onderkaak 
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak == "Adult"] <- "Volwassen"
    rawData$Leeftijdscategorie_onderkaak[is.na(rawData$Leeftijdscategorie_onderkaak)] <- "Niet ingezameld"
    
    # Date format
    rawData$afschot_datum <- as.Date(rawData$afschot_datum)
    # Define season
    rawData$season <- getSeason(rawData$afschot_datum)
    
    # Redefine names and ordering of factor levels
    rawData$type_comp <- simpleCap(rawData$type_comp)
    rawData$jachtmethode_comp <- simpleCap(rawData$jachtmethode_comp)
    rawData$labeltype <- simpleCap(gsub("REE", "", rawData$labeltype))
    
    # New variable: leeftijd_comp_inbo
    rawData$leeftijd_comp_inbo <- rawData$leeftijd_comp
    rawData$leeftijd_comp_inbo[rawData$leeftijd_comp_inbo %in% "Frisling"] <- 
      ifelse(rawData$leeftijd_maanden[rawData$leeftijd_comp_inbo %in% "Frisling"] < 6,
        "Frisling (<6m)", "Frisling (>6m)")
    
    for (iVar in names(newLevels)) {
      
      oldValues <- unique(rawData[!is.na(rawData[, iVar]), iVar]) 
      if (any(!oldValues %in% c(newLevels[[iVar]], "Onbekend")))
        warning("Volgende waarden zullen worden overschreven als 'Onbekend' voor ", iVar, ": ",
          paste(oldValues[!oldValues %in% newLevels[[iVar]]], collapse = ", "))
      
      rawData[is.na(rawData[iVar]), iVar] <- "Onbekend"
      
    }  
    
    if (any(rawData$aantal_embryos_onbekend[!is.na(rawData$aantal_embryos)])) {
      warning(sum(rawData$aantal_embryos_onbekend[!is.na(rawData$aantal_embryos)], na.rm = TRUE), 
        " observaties met gekend aantal embryos wordt op onbekend gezet")
      rawData$aantal_embryos[rawData$aantal_embryos_onbekend] <- NA
    }
    
    # Drop unused columns
    rawData <- rawData[, colnames(rawData)[!colnames(rawData) %in% 
          c("aantal_embryos_onbekend", "doodsoorzaak", "leeftijd_maanden", "tijdstip_comp", 
            "wettelijk_kader", "periode", "periode_wettelijk")]]
  
    
  } else if (type == "geo") {
    ## GEO data for grofwild
    
    # Match on Postcode: otherwise mismatch with spatialData locatie
    rawData$gemeente_afschot_locatie <- as.character(gemeenteData$Gemeente)[
      match(rawData$postcode_afschot_locatie, gemeenteData$Postcode)] 
    
    # Drop unused columns
    rawData <- rawData[, colnames(rawData)[!colnames(rawData) %in% 
          c("verbatimCoordinateUncertainty", "WBE_Naam_Georef", "KboNummer_Georef")]]
    
    # For binding with waarnemingen data
    rawData$dataSource <- "afschot"
    rawData$aantal <- 1
    
  } else if (type == "wildschade") {
    ## Wildschade data
    
    # variables to keep
    rawData <- rawData[, c("UUID", "IndieningID", "IndieningType", "Jaartal", 
        "IndieningSchadeBasisCode", "IndieningSchadeCode",
        "SoortNaam", "DiersoortNaam", "DatumVeroorzaakt",
        "provincie", "fbz", "fbdz", "NisCode_Georef", "GemNaam_Georef", 
        "UTM5", "KboNummer", "WBE_Naam_Georef", "PartijNummer", 
        "PolyLocatieWKT", "x", "y",
        "geschat_schadebedrag", "type_melding")]
    
    # format date
    rawData$DatumVeroorzaakt <- as.Date(rawData$DatumVeroorzaakt, format = "%Y-%m-%d")
    
    # new column names
    colnames(rawData) <- c("ID", "caseID", "indieningType", "afschotjaar", 
      "schadeBasisCode", "schadeCode",
      "SoortNaam", "wildsoort", "afschot_datum",
      "provincie", "FaunabeheerZone", "fbdz", "NISCODE", "gemeente_afschot_locatie",
      "UTM5", "KboNummer", "WBE_Naam_Toek", "PartijNummer",
      "perceelPolygon", "x", "y", "schadeBedrag", "typeMelding")
    
    # Match on NISCODE: otherwise mismatch with spatialData locatie
    rawData$gemeente_afschot_locatie <- as.character(gemeenteData$Gemeente)[
      match(rawData$NISCODE, gemeenteData$NIS.code)] 
    
    # Remove Voeren as province
    rawData$provincie[rawData$provincie %in% "Voeren"] <- "Limburg"
    
    # Define season
    rawData$season <- getSeason(rawData$afschot_datum)
    # Fix for korrelmais
    rawData$SoortNaam[rawData$SoortNaam == "Korrelma\xefs"] <- "Korrelmais"
    
    # fix for ANDERE within GEWAS
    rawData$schadeCode[rawData$schadeBasisCode == "GEWAS" & rawData$schadeCode == "ANDERE"] <- "GEWASANDR"
    
    # format schade bedrag
    rawData$schadeBedrag <- suppressWarnings(as.numeric(gsub("BEDRAG", "", rawData$schadeBedrag)))
    
    # If x/y coordinates missing -> exclude
    toExclude <- is.na(rawData$x) | is.na(rawData$y)
    if (sum(toExclude) > 0)
      warning(sum(toExclude), " x/y locaties zijn onbekend en dus uitgesloten voor wildschade")
    rawData <- rawData[!toExclude, ]
    
    
  } else if (type == "kbo_wbe") {
    
    # drop unused
    rawData$WBE_Naam_Partij <- NULL
    
  } else if (type == "waarnemingen") {
    
    rawData <- rawData[, c("jaar", "NAAM", "TAG", "aantal")]
    colnames(rawData) <- c("afschotjaar", "gemeente_afschot_locatie", "UTM5", "aantal") 
    
    # Restrict to 2023 
    # https://github.com/inbo/reporting-rshiny-grofwildjacht/issues/395
    rawData <- rawData[rawData$afschotjaar < 2023, ]
  
    rawData <- cbind(rawData, data.frame(wildsoort = "Wild zwijn", dataSource = "waarnemingen.be"))
    
  }
  
  
  # Uniform FBZ
  if ("FaunabeheerZone" %in% colnames(rawData)) {
    
    rawData$FaunabeheerZone[is.na(rawData$FaunabeheerZone)] <- "Onbekend"
    
  }
  
  # Define fbz_gemeente
  if (type %in% c("geo", "wildschade")) {
    
    rawData$fbz_gemeente <- ifelse(
      rawData$FaunabeheerZone == "Onbekend" | is.na(rawData$gemeente_afschot_locatie),
      NA, paste0(rawData$FaunabeheerZone, "_", rawData$gemeente_afschot_locatie))
    
  }


  s3write_using(rawData, FUN = write.csv, row.names = FALSE, bucket = bucket,
    object = paste0(tools::file_path_sans_ext(dataFile), "_processed.", tools::file_ext(dataFile)),
    opts = list(multipart = TRUE))

  
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
            
    }, USE.NAMES = FALSE, simplify = FALSE))
  
  spreadData <- sapply(names(spatialFiles), function(iFile) {
      
      unitChoices <- if (grepl("pixels", iFile))
          c("Mdl_EP_", "Mdl_OH_", "Rsc_ExP", "Rsc_OpH") else
          c("M_EP_A_", "M_OH_A_", "M_EP__G_", "M_OH__G_")
      unitVariable <- unitChoices[match(unit, c("model_EP", "model_OH", "risk_EP", "risk_OH"))]
      
      baseMap <- rgdal::readOGR(file.path(jsonDir, spatialFiles[iFile])) %>%
        sp::spTransform(CRS("+proj=longlat +datum=WGS84"))
      
      # Modify data
#      ## Risico
#      riskLevels <- c("Hoog risico", "Gemiddeld risico", "Laag risico", "Verwaarloosbaar risico") 
#      if (grepl("pixels", iFile)) {
#        baseMap$Rsc_ExP <- factor(baseMap$Rsc_ExP, levels = riskLevels)
#        baseMap$Rsc_OpH <- factor(baseMap$Rsc_OpH, levels = riskLevels)
#      } else {
#        baseMap$M_EP__G_ <- factor(baseMap$M_EP__G_, levels = riskLevels)
#        baseMap$M_OH__G_ <- factor(baseMap$M_OH__G_, levels = riskLevels)
#      }
      
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
      
      if (!is.null(startVariable)) {
        modelShape$start <- modelShape[[startVariable]]
        startYear <- unique(modelShape$start)
        if (length(unique(startYear[!is.na(startYear)])) != 1)
          stop("Multiple start years detected in the spread data: ", spatialFiles[iFile])
      }
      
      # Compatibility with old data format
      if (iFile == "municipalities_2022")
        colnames(modelShape@data)[1] <- "NAAM"
      
      modelShape@data <- modelShape@data[, c(if (grepl("pixels", iFile)) "ID" else "NAAM", 
          "outcome", 
          if (!is.null(startVariable)) "start")]
      
      # Add Voeren
      if (grepl("municipalities", iFile)) {
        voerenShape <- spatialData$communes[spatialData$communes@data$NAAM == "Voeren", ]
        voerenShape@data <- data.frame(NAAM = "Voeren", outcome = "Al aanwezig")
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

