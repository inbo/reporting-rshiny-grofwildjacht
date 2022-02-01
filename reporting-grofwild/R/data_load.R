#' Read all shape data from geojson files
#' @param dataDir character vector, defines the path to the data files
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
readShapeData <- function(dataDir = system.file("extdata", package = "reportingGrofwild"),
    tolerance = 0.0001) {
  
  
  allLevels <- c("Vlaanderen" = "flanders", "Provincies" = "provinces", 
      "Gemeenten" = "communes", "FBZ" = "faunabeheerzones", "FBDZ" = "fbz_gemeentes",
      "UTM5" = "utm5")
   # WBE per year
  wbeLevels <- gsub(".geojson", "", list.files(path = system.file("extdata", package = "reportingGrofwild"), pattern = "WBE_binnengrenzen_"))
  allLevels <- c(allLevels, wbeLevels)  
  
  
  ## New code for geojson files
  spatialData <- lapply(allLevels, function(iLevel) {
        
        file <- file.path(dataDir, paste0(iLevel, ".geojson"))
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
          
        }
        return(shapeData)
        
      })
  
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
  attr(rawData, "Date") <- file.mtime(pathFile)
  
  return(rawData)
  
}


#' Read toekenningen (Ree) data
#' @inheritParams readShapeData
#' @return data.frame with columns:
#' \itemize{
#' \item{'Provincie': }{character, province}
#' \item{'Jaar': }{integer, year}
#' \item{'Labeltype': }{character, type of Ree, one of \code{c("Geiten", "Bokken", "Kitsen")}}
#' \item{'Aantal': }{integer, frequencie per categorie}
#' }
#' and attribute 'Date', the date that this data file was created
#' @importFrom utils read.csv
#' @export
loadToekenningen <- function(dataDir = system.file("extdata", package = "reportingGrofwild")) {
  
  pathFile <- file.path(dataDir, "Toekenningen_ree.csv")
  
  rawData <- read.csv(pathFile, sep = ";", stringsAsFactors = FALSE)
  
  # Rename LabelType to non-plural
  rawData$Labeltype[rawData$Labeltype == "Geiten"] <- "geit"
  rawData$Labeltype[rawData$Labeltype == "Bokken"] <- "bok"
  rawData$Labeltype[rawData$Labeltype == "Kitsen"] <- "kits"
  
  # Rename provinces
  rawData$Provincie[rawData$Provincie == "Vlaams-Brabant"] <- "Vlaams Brabant"
  rawData$Provincie <- factor(rawData$Provincie,
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
    
    # Re-define "Adult" as "Volwassen" for leeftijd + ordering levels
    rawData$leeftijdscategorie_MF[rawData$leeftijdscategorie_MF == "Adult"] <- "Volwassen"
    rawData$leeftijd_comp[rawData$leeftijd_comp == "Adult"] <- "Volwassen"
    
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak == "Adult"] <- "Volwassen"
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak %in% c("", "Onbekend")] <- "Niet ingezameld"
    
    
    # Define type_comp (ageGender)
    rawData$type_comp <- as.factor(simpleCap(rawData$type_comp))
    
#        rawData$type <- ifelse(rawData$wildsoort != "Ree",
#                "", ifelse(grepl("kits", rawData$type_comp), "kits",
#                        ifelse(rawData$geslacht_comp == "Mannelijk", "bok", "geit")))
#        
#        
#        # for Figure 13: combine age and gender: 'type' column 
#        # (to do the matching with the openingstijden table)
#        idx <- which(rawData$wildsoort == "Ree")
#        typeRee <- ifelse(
#                rawData[idx, "leeftijd_comp"]  == "Kits", "kits",
#                ifelse(rawData[idx, "leeftijd_comp"] %in% c("Jongvolwassen", "Volwassen"),
#                        ifelse(rawData[idx, "geslacht_comp"] == 'Mannelijk', "bok", 
#                                ifelse(rawData[idx, "geslacht_comp"] == 'Vrouwelijk', "geit", "")
#                        ),
#                        ""))
#        rawData$type2 <- ""
#        rawData$type2[idx] <- typeRee
#        rawData$type2[is.na(rawData$type2)] <- ""
#        
#        
#       
#        # for Figure 28: combine age and gender, with subcategory for young adult
#        male <- rawData$geslacht_comp == "Mannelijk"
#        female <- rawData$geslacht_comp == "Vrouwelijk"
#        ageGender <- with(rawData,
#                ifelse(leeftijd_comp == "Kits", 
#                        ifelse(male, "Bokkits", ifelse(female, "Geitkits", "")),
#                        ifelse(leeftijd_comp == "Jongvolwassen", 
#                                ifelse(male, "Jaarlingbok", ifelse(female, "Smalree", "")),
#                                ifelse(leeftijd_comp == "Volwassen", 
#                                        ifelse(male, "Bok", ifelse(female, "Geit", "")), "")
#                        )))
#        ageGender[is.na(ageGender)] <- ""
#        
#        rawData$ageGender <- factor(ageGender, 
#                levels = c("", "Geitkits", "Bokkits", "Smalree", "Jaarlingbok", "Geit", "Bok"))
#        
#		# for Figure p. 27, 28: compute cheek length
# 		# redundant: now available in onderkaaklengte_comp_bron
#		rawData$bron <- with(rawData, ifelse(is.na(onderkaaklengte_comp), NA,
#						ifelse(!is.na(lengte_mm), "inbo", "meldingsformulier")))
    
    
    
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
        

#        # Temporary fix - this should be done by Sander (data cleaning) in the future
#        # see also global.R
#        toExclude <- (rawData$ageGender == "Geit" & rawData$onderkaaklengte_comp > 200)
#        toExclude[is.na(toExclude)] <- FALSE
#        
#        if (any(toExclude)) {
#            
#            ids <- rawData$ID[toExclude]
#            
#            
#            rawData <- rawData[!toExclude, ]
#            
#            warning(sum(toExclude), 
#                    " Geit(en) with onderkaaklengte_comp > 200 were excluded.")
#            attr(rawData, "excluded") <- ids
#            
#            
#        }
    
  }
  
  
  attr(rawData, "Date") <- file.mtime(dataFile)
  
  
  return(rawData)
  
}


#' Load WBE Habitats (Background) data
#' @inheritParams loadToekenningen 
#' @return data.frame
#' 
#' @author mvarewyck
#' @export
loadWbeHabitats <- function(dataDir = system.file("extdata", package = "reportingGrofwild")) {
  
  allFiles <- list.files(dataDir, pattern = "WBE_habitats", full.names = TRUE)
  wbeHabitats <- do.call(rbind, lapply(allFiles, function(iFile) {
        
        iYear <- as.numeric(strsplit(gsub("WBE_habitats_", "", basename(iFile)), "\\.")[[1]][1])
        iData <- read.csv(file = iFile)
        iData$year <- iYear
        
        iData
        
      }))
  
  return(wbeHabitats)
  
}



#' Specify currently used type schades
#' @return character vector 
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
  
  # List with all possible choices for indieningType per schadeChoice
  sourcesSchade <-  list(
    "E-loket" = c("E_Loket_Meldpunt Jacht", "E_Loket_Meldpunt Schade_Punt", "E_Loket_Meldpunt Schade_Poly"),
    "Natuurpunt" = c("Natuurpunt_copied_observation", "Natuurpunt_ifbl", "Natuurpunt_losse_waarneming", "Natuurpunt_ObsMapp", "Natuurpunt_via_wnpda", 
      "Natuurpunt_WinObs", "Natuurpunt_Dieren_onder_de_wielen_2.0", "Natuurpunt_iObs", "Natuurpunt_NA", "Natuurpunt_Site", 
      "Natuurpunt_Webobs_html5", "Natuurpunt_zoogdiertelling_be"),
    "HVV" = c("HVV_Wilder")
  )
  
  
  list(
    wildsoorten = schadeWildsoorten,
    types = schadeTypes,
    codes = schadeCodes,
    sources = sourcesSchade
  )
  
}
