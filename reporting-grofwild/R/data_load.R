#' Read all shape data from geojson files
#' 
#' @param jsonDir character, path to json shape files
#' @param dataDir character, path to data files
#' @param tolerance numeric, defines the tolerance in the Douglas-Peuker algorithm;
#' larger values will impose stronger simplification; default value is 0.001
#' @return save to dataDir object spatialData, i.e. a list with for each 
#' spatial level a SpatialPolygonsDataFrame object, 
#' with polygons and data as provided in the dataDir; spatial levels are 
#' flanders, provinces, communes, faunabeheerzones, fbz_gemeentes, utm5 
#' and provincesVoeren (Voeren as separate province)
#' @importFrom sp CRS spTransform SpatialPolygonsDataFrame
#' @importFrom methods slot
#' @importFrom maptools unionSpatialPolygons spRbind
#' @importFrom rgdal readOGR
#' @importFrom rgeos gSimplify
#' @importFrom shiny incProgress
#' @importFrom raster area
#' @importFrom utils write.csv read.csv
#' @export
readShapeData <- function(jsonDir, dataDir = system.file("extdata", package = "reportingGrofwild"),
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
          
          shapeData$NAAM <- factor(shapeData$WBENR)
          
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
        if (iName %in% c("fbz_gemeentes", "utm5"))
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
  gemeenteData <- read.csv(file.path(dataDir, "gemeentecodes.csv"), 
      header = TRUE, sep = ",",stringsAsFactors = FALSE)
  
  communeData <- spatialData$communes@data
  gemeenteData$Gemeente <- communeData$NAAM[match(gemeenteData$NIS.code, communeData$NISCODE)]
  write.csv(gemeenteData, file = file.path(dataDir, "gemeentecodes.csv"), row.names = FALSE)
  
  # IF any NIS code not in gemeenteData -> throw error
  if (any(!spatialData$communes@data$NISCODE %in% gemeenteData$NIS.code))
    stop("Sommige NIS codes in shape data zijn niet gekend voor matching\n",
        "Gelieve het referentiebestand gemeentecodes.csv aan te vullen")
  
    
  # Save WBE data separately
  spatialDataWBE <- spatialData[grep("WBE", names(spatialData))]
  save(spatialDataWBE, file = file.path(dataDir, "spatialDataWBE.RData"))
  
  spatialData <- spatialData[grep("WBE", names(spatialData), invert = TRUE)]
  save(spatialData, file = file.path(dataDir, "spatialData.RData"))
    
}




#' read openingstijden data
#' @inheritParams readShapeData
#' @return data.frame with columns:
#' \itemize{
#' \item{'Soort': }{specie}
#' \item{'Type': }{specie type}
#' \item{'Jaar': }{year}
#' \item{'Startdatum': }{start datum, in the format '\%d/\%m/\%Y'}
#' \item{'Stopdatum': }{end datum, in the format '\%d/\%m/\%Y'}
#' }
#' and attribute 'Date', the date that this data file was created
#' @importFrom utils read.csv
#' @export
loadOpeningstijdenData <- function(dataDir = system.file("extdata", package = "reportingGrofwild")){
  
  pathFile <- file.path(dataDir, "Openingstijden_grofwild.csv")
  
  rawData <- read.csv(pathFile, sep = ";", stringsAsFactors = FALSE)
  rawData$Type <- simpleCap(rawData$Type)
  
  attr(rawData, "Date") <- file.mtime(pathFile)
  
  return(rawData)
  
}


#' Read toekenningen (Ree) data
#' @inheritParams readShapeData
#' @return data.frame with columns:
#' \itemize{
#' \item{'labeltype': }{character, type of Ree, one of \code{c("geit", "bok", "kits")}}
#' \item{'WBE_Naam': }{character, WBE name}
#' \item{'labeljaar': }{integer, year}
#' \item{'provincie_toek': }{character, province}
#' \item{'toegekend': }{integer, no. of assigned animals}
#' \item{'verwezenlijkt': }{integer, no. of shot animals} 
#' \item{'percentage_verwezenlijkt': }{numeric, percentage shot animals}
#' \item{'KboNummer_Toek': }{character, WBE KBO number}
#' }
#' and attribute 'Date', the date that this data file was created
#' @importFrom utils read.csv
#' @export
loadToekenningen <- function(dataDir = system.file("extdata", package = "reportingGrofwild")) {
  
  pathFile <- file.path(dataDir, "Verwezenlijkt_categorie_per_afschotplan.csv")
  
  rawData <- read.csv(pathFile, sep = ";", stringsAsFactors = FALSE)
  
  # Rename LabelType to non-plural
  rawData$labeltype[rawData$labeltype == "Geiten"] <- "Geit"
  rawData$labeltype[rawData$labeltype == "Bokken"] <- "Bok"
  rawData$labeltype[rawData$labeltype == "Kitsen"] <- "Kits"
  
  rawData$wildsoort <- "Ree"
  
  # Change variable class
  rawData$KboNummer_Toek <- as.character(rawData$KboNummer_Toek)
  
  # Rename provinces
  rawData$provincie_toek[rawData$provincie_toek == "Vlaams-Brabant"] <- "Vlaams Brabant"
  rawData$provincie_toek <- factor(rawData$provincie_toek,
    levels = c("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams Brabant",
      "Antwerpen", "Limburg"))
  
  attr(rawData, "Date") <- file.mtime(pathFile)
  
  
  return(rawData)
  
}



#' Read ecology or geography data
#' @inheritParams readShapeData
#' @param type data type, "eco" for ecology data and "geo" for geography data
#' @return data.frame, loaded ecology or geography data; 
#' and attribute 'Date', the date that this data file was created
#' @author mvarewyck
#' @importFrom utils read.csv
#' @importFrom sp CRS proj4string
#' @importFrom raster coordinates
#' @export
loadRawData <- function(
    dataDir = system.file("extdata", package = "reportingGrofwild"),
    type = c("eco", "geo", "wildschade")) {
  
  type <- match.arg(type)
  
  
  dataFile <- file.path(dataDir, switch(type,
          "eco" = "rshiny_reporting_data_ecology.csv",
          "geo" = "rshiny_reporting_data_geography.csv",
          "wildschade" = "WildSchade_georef.csv"))
  
  rawData <- read.csv(dataFile, sep = ";", stringsAsFactors = FALSE)
#  xtabs( ~ provincie + wildsoort, data = rawData)
  
  ## Replace decimal comma by dot
  if ("ontweid_gewicht" %in% names(rawData))
    rawData$ontweid_gewicht <- as.numeric(sub("\\,", ".", rawData$ontweid_gewicht))
  
  ## Replace decimal comma by dot
  if ("lengte_mm" %in% names(rawData))
    rawData$lengte_mm <- as.numeric(sub("\\,", ".", rawData$lengte_mm))
  
  ## Replace decimal comma by dot
  if ("onderkaaklengte_comp" %in% names(rawData))
    rawData$onderkaaklengte_comp <- as.numeric(sub("\\,", ".", rawData$onderkaaklengte_comp))
  
  ## Mismatch names with spatial (shape) data for "Vlaams Brabant"
  rawData$provincie <- factor(ifelse(rawData$provincie == "Vlaams-Brabant",
          "Vlaams Brabant", as.character(rawData$provincie)))
#	xtabs( ~ provincie + wildsoort, data = rawData)
  
  # Gemeente & NIS & postcode
  # Data source: http://portal.openbelgium.be/he/dataset/gemeentecodes
  gemeenteData <- read.csv(file.path(dataDir, "gemeentecodes.csv"), 
      header = TRUE, sep = ",")
  
  
  ## Only for "Wild zwijn" separate province "Voeren" is considered, otherwise part of "Limburg"
  ## Re-order factor levels for plots
  if ("wildsoort" %in% names(rawData)) {
    
    rawData$provincie <- factor(ifelse(rawData$wildsoort == "Wild zwijn", 
            as.character(rawData$provincie), 
            ifelse(rawData$provincie == "Voeren", 
                "Limburg", 
                as.character(rawData$provincie))),
        levels = c("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren"))
#   xtabs( ~ provincie + wildsoort, data = rawData)
    
  }
  
  
  if (type == "geo") {
    ## GEO data for grofwild
    
    # Replace "N/B"
    rawData$provincie[rawData$provincie %in% "#N/B"] <- NA
    
    # Match on Postcode: otherwise mismatch with spatialData locatie
    rawData$gemeente_afschot_locatie <- as.character(gemeenteData$Gemeente)[match(rawData$postcode_afschot_locatie, gemeenteData$Postcode)] 
    
    # Create fbz_gemeente
    rawData$fbz_gemeente <- ifelse(is.na(rawData$FaunabeheerZone) | is.na(rawData$gemeente_afschot_locatie),
        NA, paste0(rawData$FaunabeheerZone, "_", rawData$gemeente_afschot_locatie))
    
    
  } else if (type == "eco") {
    ## ECO data for grofwild
    
    newLevels <- loadMetaEco()
    
    # Re-define "Adult" as "Volwassen" for leeftijd
    rawData$leeftijdscategorie_MF[rawData$leeftijdscategorie_MF == "Adult"] <- "Volwassen"
    rawData$leeftijd_comp[rawData$leeftijd_comp == "Adult"] <- "Volwassen"
    
    # Leeftijdscategorie_onderkaak 
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak == "Adult"] <- "Volwassen"
    rawData$Leeftijdscategorie_onderkaak <- factor(rawData$Leeftijdscategorie_onderkaak,
      levels = c(newLevels[["leeftijd_comp"]], "Niet ingezameld"))
    rawData$Leeftijdscategorie_onderkaak[is.na(rawData$Leeftijdscategorie_onderkaak)] <- "Niet ingezameld"
    
    
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
      if (any(!oldValues %in% newLevels[[iVar]]))
        warning("Volgende waarden zullen worden overschreven als 'Onbekend' voor ", iVar, ": ",
          paste(oldValues[!oldValues %in% newLevels[[iVar]]], collapse = ", "))
      
      rawData[, iVar] <- factor(rawData[, iVar], levels = c(newLevels[[iVar]], "Onbekend"))
      rawData[is.na(rawData[iVar]), iVar] <- "Onbekend"
      
    }  
    
  } else if (type == "wildschade") {
    ## Wildschade data
    
        # variables to keep
        rawData <- rawData[, c("UUID", "IndieningID", "IndieningType", "Jaartal", 
                        "IndieningSchadeBasisCode", "IndieningSchadeCode",
                        "SoortNaam", "DiersoortNaam", "DatumVeroorzaakt",
                        "provincie", "fbz", "fbdz", "NisCode_Georef", "GemNaam_Georef", 
                        "UTM5", "KboNummer", "PolyLocatieWKT", "x", "y")]
        
        # format date
        rawData$DatumVeroorzaakt <- format(as.Date(substr(x = rawData$DatumVeroorzaakt, start = 1, stop = 10), 
                        format = "%Y-%m-%d"), "%d/%m/%Y")
        
        # new column names
        colnames(rawData) <- c("ID", "caseID", "indieningType", "afschotjaar", 
                "schadeBasisCode", "schadeCode",
                "SoortNaam", "wildsoort", "afschot_datum",
                "provincie", "FaunabeheerZone", "fbdz", "NISCODE", "gemeente_afschot_locatie",
                "UTM5", "KboNummer", "perceelPolygon", "x", "y")
        
        # Match on NISCODE: otherwise mismatch with spatialData locatie
        rawData$nieuwe_locatie <- as.character(gemeenteData$Gemeente)[match(rawData$NISCODE, gemeenteData$NIS.code)] 

        rawData$gemeente_afschot_locatie <- rawData$nieuwe_locatie
        rawData$nieuwe_locatie <- NULL
        # Remove Voeren as province
        rawData$provincie[rawData$provincie %in% "Voeren"] <- "Limburg"
        rawData$provincie <- droplevels(rawData$provincie)
        
        
        # Define fbz_gemeente
        rawData$fbz_gemeente <- ifelse(is.na(rawData$FaunabeheerZone) | is.na(rawData$gemeente_afschot_locatie),
                NA, paste0(rawData$FaunabeheerZone, "_", rawData$gemeente_afschot_locatie))
        # Define season
        rawData$season <- getSeason(rawData$afschot_datum)
        # Fix for korrelmais
        rawData$SoortNaam[rawData$SoortNaam == "Korrelma\xefs"] <- "Korrelmais"
        
        # fix for ANDERE within GEWAS
        rawData$schadeCode[rawData$schadeBasisCode == "GEWAS" & rawData$schadeCode == "ANDERE"] <- "GEWASANDR"
        
        # TODO what if x/y coordinates missing -> exclude
        toExclude <- is.na(rawData$x) | is.na(rawData$y)
        warning(sum(toExclude), " x/y locaties zijn onbekend en dus uitgesloten voor wildschade")
        rawData <- rawData[!toExclude, ]
        
        
        # create shape data
        coordinates(rawData) <- ~x + y
        proj4string(rawData) <- CRS("+init=epsg:31370")
        
        rawData <- spTransform(rawData, CRS("+proj=longlat +datum=WGS84"))
    
  }
  
  
  attr(rawData, "Date") <- file.mtime(dataFile)
  
  
  return(rawData)
  
}


#' Load Habitats (Background) data
#' @inheritParams readShapeData
#' @param spatialData list with each element a SpatialPolygonsDataFrame as created 
#' by \code{\link{readShapeData}}
#' @param regionLevels character vector, for which regions load the habitat data;
#' if NULL loaded for all levels; default value is NULL
#' @return named list with data.frame for each region level
#' 
#' @author mvarewyck
#' @export
loadHabitats <- function(dataDir = system.file("extdata", package = "reportingGrofwild"),
  spatialData, regionLevels = NULL) {
  
  allLevels <- list(
    "flanders" = "flanders_habitats", 
    "provinces" = "Provincies_habitats", 
    "communes" = "Gemeentes_habitats", 
    "faunabeheerzones" = "Faunabeheerzones_habitats", 
    # "fbz_gemeentes" = "FaunabeheerDeelzones",  # currently missing see #295 
    "utm5" = "utm5_vlgrens_habitats", 
    "wbe" = "WBE_habitats"
  ) 
  
  if (!is.null(regionLevels))
    allLevels <- allLevels[names(allLevels) %in% regionLevels]
  
  
  habitatData <- sapply(names(allLevels), function(iRegion) {
      
      iLevel <- allLevels[[match(iRegion, names(allLevels))]]
      
      allFiles <- list.files(dataDir, pattern = iLevel, full.names = TRUE)
      
      if (iRegion == "wbe") {
          
          tmpData <- do.call(rbind, lapply(allFiles, function(iFile) {
                
                iData <- read.csv(file = iFile)
                iData$year <- as.numeric(gsub("WBE_|habitats_|\\.csv", "", basename(iFile)))
             
                iData
                
              }))
          
          colnames(tmpData)[1] <- "regio"
          
        } else { 
          
          tmpData <- read.csv(file = allFiles)
          colnames(tmpData)[1] <- "regio"
          
          # Match region names
          if ("NISCODE" %in% colnames(spatialData[[iRegion]]@data))
            tmpData$regio <- spatialData[[iRegion]]$NAAM[
              match(as.numeric(tmpData$regio), as.numeric(spatialData[[iRegion]]$NISCODE))]
          
          # Check matching
          if (!all(spatialData[[iRegion]]$NAAM %in% tmpData$regio))
            stop("Matching for habitat data names failed ", iRegion)
          
        }
        
      return(tmpData)
    
    }, simplify = FALSE)
  
  
  return(habitatData)
  
}



#' Specify currently used levels in eco data
#' 
#' @param species character, whether to extract levels for specific species;
#' default is NA
#' @return list with meta data for eco data
#' 
#' @author mvarewyck
#' @export
loadMetaEco <- function(species = NA) {
  
  # Defines the order of the species
  allSpecies <- c("Wild zwijn", "Ree", "Damhert", "Edelhert")
  
  toReturn <- list(
    geslacht_comp = c("Vrouwelijk", "Mannelijk"),
    leeftijd_comp_inbo = list(
      # Young to old
      c("Frisling (<6m)", NA, NA),
      c("Frisling (>6m)", "Kits", rep("Kalf", 2)),
      c(NA, NA, "Jaarling", "Jaarling"),  
      c("Overloper", rep("Jongvolwassen", 3)),
      rep("Volwassen", 4) 
    ),
    leeftijd_comp = list(
      # Young to old
      c("Frisling", "Kits", rep("Kalf", 2)),
      c(NA, NA, "Jaarling", "Jaarling"),  
      c("Overloper", rep("Jongvolwassen", 3)),
      rep("Volwassen", 4) 
    ),
    type_comp = list(
      # Young to old
      c("Frisling (v)", "Frisling (m)", "Geitkits", "Bokkits", rep(c("Kalf (v)", "Kalf (m)"), 2)),
      c("Overloper (v)", "Overloper (m)", "Smalree", "Jaarlingbok", rep(c("Smaldier", "Spitser"), 2)),
      c("Zeug", "Keiler", "Reegeit", "Reebok", rep(c("Hinde", "Hert"), 2))
    ),
    jachtmethode_comp = 
      c("Aanzitjacht", "Bersjacht", "Drijfjacht", "Drukjacht", "Kooijacht"),
    labeltype = list(
      # Wild Zwijn - Ree - Damhert - Edelhert
      "Wild zwijn",
      c("Kits", "Geit", "Bok"),
      "Damhert",
      "Edelhert"        
    )
  )
  
  if (!is.na(species)) {
    
    # Filter species
    matchId <- match(species, allSpecies)
    toReturn$leeftijd_comp_inbo <- sapply(toReturn$leeftijd_comp_inbo, function(x) x[matchId])
    toReturn$leeftijd_comp <- sapply(toReturn$leeftijd_comp, function(x) x[matchId])
    toReturn$type_comp <- sapply(toReturn$type_comp, function(x) x[c(-1, 0) + matchId*2])
    toReturn$labeltype <- toReturn$labeltype[[matchId]] 
    
    # Remove NA
    sapply(toReturn, function(x) x[!is.na(x)])
    
  } else {
    
    # Remove NA and duplicates
    sapply(toReturn, function(x) unique(unlist(x)[!is.na(unlist(x))]))
    
  }
  
}

#' Specify currently used type schades
#' @return list with meta data for wildschade
#' 
#' @author mvarewyck
#' @export
loadMetaSchade <- function() {
  
  # Specify currently used wildsoorten
  schadeWildsoorten <- list("Grof wild" = c("wild zwijn", "edelhert", "ree", "damhert"),
    "Klein wild" = c("haas", "fazant", "konijn", "patrijs"),
    "Waterwild" = c("wilde eend", "smient", "grauwe gans", "Canadese gans", "kievit"),
    "Overig" = c("houtduif", "vos", "verwilderde kat"))
  
  # Specify currently used type schades
  schadeTypes <- c("GEWAS", "VRTG", "ANDERE")
  
  # Specify currently used types schade subcodes
  schadeCodes <- c(c("GNPERSLTSL", "PERSLTSL", "ONBEKEND"),           #VRTG
    c("WLSCHD", "VRTSCHD", "GEWASANDR", "VGSCHD", "GRFSCHD"),     #GEWAS
    c("ANDERE", "VALWILD"))                            #ANDERE
  
  # List with all patterns to search for in indieningType per schadeChoice
  sourcesSchade <-  list(
    "Dieren onder de wielen (NP)" = "Natuurpunt",
    "Wilder (LJV)" = "HVV_Wilder",
    "E-loket (ANB)" = "E_Loket"
  )
  
  
  list(
    wildsoorten = schadeWildsoorten,
    types = schadeTypes,
    codes = schadeCodes,
    sources = sourcesSchade
  )
  
}
