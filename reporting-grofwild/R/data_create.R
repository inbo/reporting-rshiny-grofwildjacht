# Create data
#
# Functions should only be called when background data changed
# 
# Author: mvarewyck
###############################################################################



#' Helper function - Define province based on commune NIS code
#' @param NISCODE character vector, NIS codes from communes, for which to define province
#' @inheritParams createSpaceData
#' @return character vector, same length and order as \code{NISCODE}
#' with corresponding province for each communeS
#' @author mvarewyck
#' @export
getProvince <- function(NISCODE, allSpatialData) {
  
  communeCode <- substr(NISCODE, start = 1, stop = 1)
  
  provinceData <- allSpatialData$provinces[, c("NISCODE", "NAAM")]
  provinceData$NISCODE <- substr(provinceData$NISCODE, start = 1, stop = 1)
  
  sapply(communeCode, function(iCode) {
      
      if (is.na(iCode))
        NA else
        as.character(provinceData[provinceData$NISCODE == iCode, ]$NAAM)
      
    })
  
}

#' Create all shape data from geojson files
#' 
#' @param jsonDir character, path to json shape files
#' @inheritParams loadRawData
#' @param tolerance numeric, defines the tolerance in the Douglas-Peuker algorithm;
#' larger values will impose stronger simplification; default value is 0.001
#' 
#' @return boolean, whether file is successfully saved;
#' save to \code{bucket} list with spatial list for WBE and non-WBE level.
#' Each contain a list with for each spatial level an object of class 'sf', 
#' with polygons and data as provided in the \code{jsonDir}; spatial levels are 
#' (1) flanders, provinces, communes, faunabeheerzones, fbz_gemeentes, utm5, utm1
#' and provincesVoeren (Voeren as separate province)
#' (2) WBE_buitengrenzen and WBE
#' @importFrom methods slot
#' @importFrom shiny incProgress
#' @importFrom utils write.csv read.csv
#' @importFrom aws.s3 s3save
#' @importFrom sf st_read st_transform st_crs st_area sf_use_s2
#' @importFrom units set_units
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
  
  sf::sf_use_s2(FALSE)
  
  ## New code for geojson files
  spatialData <- lapply(allLevels, function(iLevel) {
      
      file <- file.path(jsonDir, paste0(iLevel, ".geojson"))
      shapeData <- sf::st_read(file)
      if (sf::st_crs(shapeData)$input != "WGS 84") {
        # Transform CRS
        warning("Coordinate system is not WGS 84, shape data will be transformed for ", iLevel)
        shapeData <- sf::st_transform(shapeData, crs = 4326)
      }
      
      
      #####
      # We retrieve 'AREA' from habitatData see #439
      # This piece of code might be redundant
      areaVariables <- c("OPPERVL", "SHAPE_Area", "Shape_Area")
      if (any(areaVariables %in% colnames(shapeData))) {
        if ("AREA" %in% colnames(shapeData))
          shapeData$AREA <- NULL
        colnames(shapeData)[colnames(shapeData) %in% areaVariables] <- "AREA" 
      } else {
        # needed for preventing errors in area calculation
        shapeData <- sf::st_make_valid(shapeData) 
        shapeData$AREA <- units::set_units(sf::st_area(shapeData), "km^2", mode = "standard")
      }
      #####
      
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
        
        shapeData$NAAM <- factor(shapeData$WBE_NR)
        
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
  isLimburgProvince <- spatialData$provinces$NAAM == "Limburg"
  
  # Create new polygon for Limburg (Including voeren)
  ## sf_use_s2(FALSE)
  limburgPolygon <- spatialData$provinces[isLimburgProvince, ]
  limburgPolygon$geometry <- sf::st_union(spatialData$communes[provinceIds %in% 7, ])
  
  voerenPolygon <- spatialData$provinces[isLimburgProvince, ]
  voerenPolygon$NAAM <- "Voeren"
  voerenPolygon$NISCODE <- spatialData$communes$NISCODE[voerenId]
  voerenPolygon$OPPERVL <- spatialData$communes$OPPERVL[voerenId]
  voerenPolygon$geometry <- spatialData$communes$geometry[provinceIds %in% 100]
  
  # Bind all province polygons and data
  spatialData$provincesVoeren <- rbind(spatialData$provinces[!isLimburgProvince, ],
    limburgPolygon, voerenPolygon)
  
  
#  newNames <- names(spatialData)
#  
#  # Try to simplify polygons
#  spatialData <- lapply(names(spatialData), function(iName) {
#      
#      iData <- spatialData[[iName]]
#      
##      # Calculate area for each polygon -> needed? or rename existing
##      iData@data$AREA <- raster::area(iData)/1e06
#      
#      # No simplification
#      if (iName %in% c("fbz_gemeentes", "utm5") | grepl("WBE_[[:digit:]]", iName))
#        return(iData)
#      
#      simpleShapeData <- sf::st_simplify(iData)
#      
#      if (nrow(simpleShapeData) != nrow(iData))
#        stop("The number of polygons in original shapeData for ", 
#          iName, " is: ", length(iData),
#          "\nThe number of polygons in simplified shapeData is: ", length(simpleShapeData),
#          "\nPlease decrease value for tolerance")
#      
#      return(simpleShapeData)
#      
#    })
#  
#  names(spatialData) <- newNames
  
  
  # Update commune names for later matching: geo/wildschade data with shape data
  # Note: You can use gemeentecode.csv for matching NIS to NAAM
  gemeenteData <- loadGemeentes(bucket = bucket)
  
  communeData <- spatialData$communes
  gemeenteData$Gemeente <- communeData$NAAM[match(gemeenteData$NIS.code, communeData$NISCODE)]
  gemeenteFile <- file.path(tempdir(), "gemeentecodes.csv")
  write.csv(gemeenteData, file = gemeenteFile, row.names = FALSE)
  writeS3(dataFiles = gemeenteFile, bucket = bucket)
  
  # IF any NIS code not in gemeenteData -> throw error
  if (any(!spatialData$communes$NISCODE %in% gemeenteData$NIS.code))
    stop("Sommige NIS codes in shape data zijn niet gekend voor matching\n",
      "Gelieve het referentiebestand gemeentecodes.csv aan te vullen")
  
  # Add provincie/postcode for communes and fbz_gemeentes
  spatialData$communes$provincie <- getProvince(
    NISCODE = spatialData$communes$NISCODE,
    allSpatialData = spatialData)
  spatialData$communes$postcode <- gemeenteData$Postcode[
    match(spatialData$communes$NISCODE, gemeenteData$NIS.code)]
  spatialData$fbz_gemeentes$provincie <- getProvince(
    NISCODE = spatialData$fbz_gemeentes$NISCODE,
    allSpatialData = spatialData)
  
  # Save WBE data separately
  spatialDataWBEAll <- spatialData[grep("WBE", names(spatialData))]
  choicesWBE <- sort(unique(unlist(sapply(spatialDataWBEAll, function(x) as.numeric(x$WBE_NR)))))
  choicesWBE <- choicesWBE[!choicesWBE %in% c(0, 999, 601)]  #see #357
  for (iChoice in choicesWBE) {
    spatialDataWBE <- sapply(spatialDataWBEAll, function(iData) iData[iData$WBE_NR == iChoice, ])
    s3save(spatialDataWBE, 
      bucket = bucket, 
      object = paste0("spatialDataWBE/", iChoice,".RData"), 
      opts = list(multipart = TRUE))
  }
  
  # Save WBE for admin
  spatialDataWBE <- spatialDataWBEAll
  s3save(spatialDataWBE, bucket = bucket, object = "spatialDataWBE_sf.RData", opts = list(multipart = TRUE))

  # Save non-WBE
  spatialData <- spatialData[grep("WBE", names(spatialData), invert = TRUE)]
  s3save(spatialData, bucket = bucket, object = "spatialData_sf.RData", opts = list(multipart = TRUE))
  
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
#' @importFrom aws.s3 s3write_using
#' @importFrom arrow write_parquet
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
        " observaties met 'aantal_embryos_onbekend' TRUE terwijl gekend 'aantal_embryos'. Voor deze observaties wordt 'aantal_embryos' op NA (onbekend) gezet.")
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
    
    # Missing partijnummer for ANB regions
    toReplace <- is.na(rawData$PartijNummer) & 
      grepl("ANB_|Afwijking Jachtdecreet_|Bestrijding_|OJ_", rawData$WBE_Naam_Toek)
    rawData$PartijNummer[toReplace] <- rawData$KboNummer_Toek[toReplace]
    
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
    colnames(rawData) <- c("ID", "caseID", "dataSource", "afschotjaar", 
      "schadeBasisCode", "schadeCode",
      "SoortNaam", "wildsoort", "afschot_datum",
      "provincie", "FaunabeheerZone", "fbdz", "NISCODE", "gemeente_afschot_locatie",
      "UTM5", "KboNummer", "WBE_Naam_Toek", "PartijNummer",
      "perceelPolygon", "x", "y", "schadeBedrag", "typeMelding")
    
    # Match on NISCODE: otherwise mismatch with spatialData locatie
    rawData$gemeente_afschot_locatie <- as.character(gemeenteData$Gemeente)[
      match(rawData$NISCODE, gemeenteData$NIS.code)] 
    rawData$postcode <- gemeenteData$Postcode[match(rawData$NISCODE, gemeenteData$NIS.code)]
    
    # Remove Voeren as province
    rawData$provincie[rawData$provincie %in% "Voeren"] <- "Limburg"
    
    # Redefine dataSource
    sourcesSchade <- loadMetaSchade()$sources  
    isPresent <- grepl(paste(sourcesSchade, collapse = "|"), rawData$dataSource)
    if (!all(isPresent)) {
      warning("Nieuw indieningType gedetecteerd in schade data: ", 
        paste0(unique(rawData$dataSource[!isPresent]), collapse = ", "),
        "\nUpdate loadMetaSchade() functie.")
    }
    for (iVar in sourcesSchade)
      rawData$dataSource[grepl(iVar, rawData$dataSource)] <- iVar
    
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
    
    rawData <- rawData[, c("jaar", "NAAM", "provincie", "TAG", "aantal")]
    colnames(rawData) <- c("afschotjaar", "gemeente_afschot_locatie", "provincie", "UTM5", "aantal") 
    
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
  
  # Convert to factors
  newLevels <- loadMetaEco()
  toFactors <- names(newLevels)[names(newLevels) %in% colnames(rawData)]
  rawData[toFactors] <- lapply(toFactors, function(x) 
      droplevels(factor(rawData[[x]], levels = c(newLevels[[x]], "Onbekend"))))

  # Parquet file
  aws.s3::s3write_using(rawData, 
      FUN = arrow::write_parquet, 
      bucket = bucket, 
      object = paste0(tools::file_path_sans_ext(dataFile), "_processed.parquet"), 
      opts = list(multipart = TRUE))
  
  
  return(TRUE)   
  
}



#' Helper function - Convert old format (.RData) into new (.parquet)
#' @inheritParams createRawData
#' @return no return value
#' 
#' @author mvarewyck
convertRawData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  type = c("eco", "geo", "wildschade", "kbo_wbe", "waarnemingen")) {
  
  type <- match.arg(type)
  
  # For R CMD check
  rawData <- NULL  
  
  dataFile <- switch(type,
    "eco" = "rshiny_reporting_data_ecology_processed.RData",
    "geo" = "rshiny_reporting_data_geography_processed.RData",
    "wildschade" = "WildSchade_georef_processed.RData",
    "kbo_wbe" = "Data_Partij_Cleaned_processed.RData",
    "waarnemingen" = "waarnemingen_wild_zwijn_processed.RData"
  )
  
  readS3(file = dataFile, bucket = bucket, envir = environment())
  
  if (type == "wildschade") {
    
    rawData <- sf::st_transform(rawData, crs = 31370)     
    rawData$x <- sf::st_coordinates(rawData)[, 1]
    rawData$y <- sf::st_coordinates(rawData)[, 2]
    rawData <- sf::st_drop_geometry(rawData)
    
  }
  
  
  aws.s3::s3write_using(rawData, 
    FUN = arrow::write_parquet, 
    bucket = bucket, 
    object = paste0(tools::file_path_sans_ext(dataFile), ".parquet"), 
    opts = list(multipart = TRUE))
  
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
#' @param spatialData list, with object of class 'sf' for each spatial level;
#' as can be loaded using \code{readS3(file = "spatialData_sf.RData")}
#' @return boolean, whether file is successfully saved
#' save list of sf objects for each spatial level (pixels and municipalities)
#' as used in \code{\link{mapSpread}}
#' 
#' @author mvarewyck
#' 
#' @examples 
#' \dontrun{createSpreadData()}
#' @importFrom sf st_read st_transform
#' @export
createSpreadData <- function(
  jsonDir = "~/git/reporting-rshiny-grofwildjacht/data",
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  spatialData) {
  
  
  # currently only unit of interest
  unit <- "model_EP"
  
  # File pattern per resolution
  tmpFiles <- list(
    # pixels
    pixels = "Pixels_ModelOutput_toekomst_verspr",
#    pixels = "Model_output_Pixels",
    # gemeente
    municipalities = "Municipalities_ModelOutput_toekomst_verspr"
#    municipalities = "Model_output_Municipalities"
  )
  
  # Specify relevant (2 recent) files
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
      
      baseMap <- sf::st_read(file.path(jsonDir, spatialFiles[iFile]))
      baseMap <- sf::st_transform(baseMap, crs = "+proj=longlat +datum=WGS84")
      
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
      modelShape <- subset(baseMap, !is.na(baseMap[, unitVariable]))
      modelShape$outcome <- as.factor(modelShape[[unitVariable]])
        
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
        colnames(modelShape)[1] <- "NAAM"
      
      modelShape <- modelShape[, c(if (grepl("pixels", iFile)) "ID" else "NAAM", 
          "outcome", "geometry",
          if (!is.null(startVariable)) "start")]
      
      # Add Voeren
      if (grepl("municipalities", iFile)) {
        voerenShape <- spatialData$communes[spatialData$communes$NAAM == "Voeren", c("NAAM", "geometry")]
        voerenShape$outcome <- "Al aanwezig"
        modelShape <- rbind(modelShape, voerenShape)
      }
      
      attr(modelShape, "unit") <- unit
      attr(modelShape, "spatialLevel") <- strsplit(iFile, split = "_")[[1]][1]
      attr(modelShape, "year") <- strsplit(iFile, split = "_")[[1]][2]
      
      modelShape
      
    })
    
#    s3save(spreadData, bucket = bucket, object = "spreadData_sf.RData")
    
    return(TRUE)

} 




#' Create Habitats (Background) data
#' 
#' named list with data.frame for each region level
#' @inheritParams createRawData
#' @return boolean, whether file is successfully saved
#' 
#' @author mvarewyck
#' @export
createHabitatData <- function(
  dataDir = "~/git/reporting-rshiny-grofwildjacht/dataS3",
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))) {

  # spatialData - non WBE
  spatialData <- NULL
  readS3(file = "spatialData_sf.RData")
  
  allLevels <- list(
    "flanders" = "flanders_habitats", 
    "provinces" = "Provincies_habitats", 
    "communes" = "Gemeentes_habitats", 
    "faunabeheerzones" = "Faunabeheerzones_habitats", 
    "fbz_gemeentes" = "fbz_gemeentes_habitats",
    "utm5" = "utm5_vlgrens_habitats", 
    "wbe" = "WBE_habitats"
  ) 
  
  habitatData <- sapply(names(allLevels), function(iRegion) {
      
      iLevel <- allLevels[[match(iRegion, names(allLevels))]]
      
      allFiles <- grep(pattern = iLevel, x = list.files(dataDir), value = TRUE)
      
      if (iRegion == "wbe") {
        
        tmpData <- do.call(rbind, lapply(allFiles, function(iFile) {
              
              iData <- read.csv(file.path(dataDir, iFile)) 
              iData$year <- as.numeric(gsub("WBE_|habitats_|\\.csv", "", basename(iFile)))
              
              iData
              
            }))
        
        colnames(tmpData)[1] <- "regio"
        
      } else { 
        
        # Special case
        if (iRegion == "provinces")
          iRegion <- "provincesVoeren"
        
        tmpData <- read.csv(file.path(dataDir, allFiles)) 
        
        if ("NISCODE" %in% colnames(tmpData)) {
          colnames(tmpData)[colnames(tmpData) == "NISCODE"] <- "regio"
        } else {
          colnames(tmpData)[1] <- "regio"
        }
        
        # Match region names
        if (iRegion != "fbz_gemeentes" & "NISCODE" %in% colnames(spatialData[[iRegion]]))
          tmpData$regio <- spatialData[[iRegion]]$NAAM[
            match(as.numeric(tmpData$regio), as.numeric(spatialData[[iRegion]]$NISCODE))]
        
        # Check matching
        if (!all(spatialData[[iRegion]]$NAAM %in% tmpData$regio))
          stop("Matching for habitat data names failed ", iRegion)
        
      }
      
      return(tmpData)
      
    }, simplify = FALSE)
  
  # Bind wegdensiteit
  densiteitData <- data.table::fread(file = file.path(dataDir, "wegdensiteit.csv"), 
    dec = ",")
  habitatData <- sapply(names(habitatData), function(iLevel) {
      
      iData <- switch(iLevel, 
        flanders = densiteitData[densiteitData$Niveau == "Vlaanderen", ],
        provinces = densiteitData[densiteitData$Niveau == "Provincie", ],
        communes = densiteitData[densiteitData$Niveau == "Gemeente", ],
        faunabeheerzones = densiteitData[densiteitData$Niveau == "Faunabeheerzone", ],
        NULL
      )
      if (is.null(iData))
        return(habitatData[[iLevel]])
      # Special case
      if (iLevel == "provinces")
        iData <- rbind(iData, 
          densiteitData[densiteitData$Niveau == "Gemeente" & densiteitData$NAAM == "Voeren", ])
      
      iData$Niveau <- NULL
      colnames(iData) <- paste0("weg_", colnames(iData))
      
      merge(habitatData[[iLevel]], iData, by.x = "regio", by.y = "weg_NAAM", all.x = TRUE)
      
    }, simplify = FALSE)
  
  
  s3save(habitatData, bucket = bucket, object = "habitatData.RData", 
    opts = list(multipart = TRUE))
  
  
  return(TRUE)   
  
}



