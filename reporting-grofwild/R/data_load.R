#' Read ecology or geography data
#' 
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-wbe-uat-data"
#' @param type data type, "eco" for ecology data and "geo" for geography data
#' 
#' @return data.frame, loaded ecology or geography data; 
#' and attribute 'Date', the date that this data file was created
#' @author mvarewyck
#' @importFrom utils read.csv
#' @importFrom sp CRS proj4string
#' @importFrom raster coordinates
#' @export
loadRawData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  type = c("eco", "geo", "wildschade", "kbo_wbe")) {
  
  type <- match.arg(type)
  
  
  dataFile <- switch(type,
          "eco" = "rshiny_reporting_data_ecology.csv",
          "geo" = "rshiny_reporting_data_geography.csv",
          "wildschade" = "WildSchade_georef.csv",
          "kbo_wbe" = "Data_Partij_Cleaned.csv")
  
  rawData <- readS3(FUN = read.csv, sep = ";", stringsAsFactors = FALSE, 
    file = dataFile, bucket = bucket)
#  xtabs( ~ provincie + wildsoort, data = rawData)
  
  ## Replace decimal comma by dot
  for (iVar in c("ontweid_gewicht", "lengte_mm", "onderkaaklengte_comp", "verbatimLatitude", "verbatimLongitude"))
    if (iVar %in% names(rawData))
      rawData[, iVar] <- as.numeric(sub("\\,", ".", rawData[, iVar]))
  
  
  ## Replace decimal comma by dot & rename
  if ("lengte_mm" %in% names(rawData)) {
    rawData$onderkaaklengte_mm <- rawData$lengte_mm
    rawData$lengte_mm <- NULL
  }
  
  ## Mismatch names with spatial (shape) data for "Vlaams Brabant"
  if ("provincie" %in% names(rawData))
    rawData$provincie <- factor(ifelse(rawData$provincie == "Vlaams-Brabant",
        "Vlaams Brabant", as.character(rawData$provincie)))
#	xtabs( ~ provincie + wildsoort, data = rawData)
  
  # Gemeente & NIS & postcode
  # Data source: http://portal.openbelgium.be/he/dataset/gemeentecodes
  gemeenteData <- loadGemeentes()
  
  
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
    
    # Drop unused columns
    rawData$verbatimCoordinateUncertainty <- NULL
    
    # For binding with waarnemingen data
    rawData$dataSource <- "afschot"
    rawData$aantal <- 1
    
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
        rawData$nieuwe_locatie <- as.character(gemeenteData$Gemeente)[match(rawData$NISCODE, gemeenteData$NIS.code)] 

        rawData$gemeente_afschot_locatie <- rawData$nieuwe_locatie
        rawData$nieuwe_locatie <- NULL
        # Remove Voeren as province
        rawData$provincie[rawData$provincie %in% "Voeren"] <- "Limburg"
        rawData$provincie <- droplevels(rawData$provincie)

        
        # Define fbz_gemeente
        rawData$fbz_gemeente <- ifelse(is.na(rawData$FaunabeheerZone) | is.na(rawData$gemeente_afschot_locatie),
          NA, paste0(rawData$FaunabeheerZone, "_", rawData$gemeente_afschot_locatie))
        # Onbekende locaties
        rawData$provincie <- factor(ifelse(is.na(rawData$provincie), "Onbekend", as.character(rawData$provincie)), levels = c(levels(rawData$provincie), "Onbekend"))
        rawData$FaunabeheerZone[is.na(rawData$FaunabeheerZone)] <- "Onbekend"
        
        # Define season
        rawData$season <- getSeason(rawData$afschot_datum)
        # Fix for korrelmais
        rawData$SoortNaam[rawData$SoortNaam == "Korrelma\xefs"] <- "Korrelmais"
        
        # fix for ANDERE within GEWAS
        rawData$schadeCode[rawData$schadeBasisCode == "GEWAS" & rawData$schadeCode == "ANDERE"] <- "GEWASANDR"
        
        # format schade bedrag
        rawData$schadeBedrag <- suppressWarnings(as.numeric(gsub("BEDRAG", "", rawData$schadeBedrag)))
        
        # TODO what if x/y coordinates missing -> exclude
        toExclude <- is.na(rawData$x) | is.na(rawData$y)
        warning(sum(toExclude), " x/y locaties zijn onbekend en dus uitgesloten voor wildschade")
        rawData <- rawData[!toExclude, ]
        
        
        # create shape data
        coordinates(rawData) <- ~x + y
        proj4string(rawData) <- CRS("+init=epsg:31370")
        
        rawData <- spTransform(rawData, CRS("+proj=longlat +datum=WGS84"))
    
  } else if (type == "kbo_wbe") {
    
    # drop unused
    rawData$WBE_Naam_Partij <- NULL
    
  }
  
  
  attr(rawData, "Date") <- file.mtime(dataFile)
  
  
  return(rawData)
  
}


#' Read gemeentes data
#' @inheritParams loadRawData
#' @return data.frame with NIS.code, Postcode and Gemeente
#' 
#' @author mvarewyck
#' @export
loadGemeentes <- function(bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))) {
  
  readS3(FUN = read.csv, header = TRUE, file = "gemeentecodes.csv", bucket = bucket)
  
}


#' read openingstijden data
#' @inheritParams loadRawData
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
#' @importFrom data.table rbindlist
#' @importFrom aws.s3 get_bucket
#' @export
loadOpeningstijdenData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))){
  
  pathFile <- "Openingstijden_grofwild.csv"
  rawData <- readS3(FUN = read.csv, sep = ";", stringsAsFactors = FALSE,
    file = pathFile, bucket = bucket)
  
  rawData$Type <- simpleCap(rawData$Type)
  
  tmpInfo <- data.table::rbindlist(aws.s3::get_bucket(bucket = bucket))
  attr(rawData, "Date") <- as.Date(tmpInfo[tmpInfo$Key == pathFile, ]$LastModified[1])
  
  return(rawData)
  
}


#' Read toekenningen (Ree) data
#' @inheritParams loadRawData
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
loadToekenningen <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))) {
  
  pathFile <- "Verwezenlijkt_categorie_per_afschotplan.csv"
  rawData <- readS3(FUN = read.csv, sep = ";", stringsAsFactors = FALSE,
    file = pathFile, bucket = bucket)
  
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
  
  rawData$provincie <- rawData$provincie_toek
  rawData$provincie_toek <- NULL
  
  attr(rawData, "Date") <- file.mtime(pathFile)
  
  
  return(rawData)
  
}




#' Load Habitats (Background) data
#' 
#' @inheritParams loadRawData
#' @param spatialData list with each element a SpatialPolygonsDataFrame as created 
#' by \code{\link{createShapeData}}
#' @param regionLevels character vector, for which regions load the habitat data;
#' if NULL loaded for all levels; default value is NULL
#' @return named list with data.frame for each region level
#' 
#' @author mvarewyck
#' @export
loadHabitats <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
  spatialData, 
  regionLevels = NULL) {
  
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
      
      allFiles <- grep(pattern = iLevel,
        x = sapply(aws.s3::get_bucket(bucket = bucket), function(x) as.list(x)$Key),
        value = TRUE)
      
      if (iRegion == "wbe") {
          
          tmpData <- do.call(rbind, lapply(allFiles, function(iFile) {
                
                iData <- readS3(FUN = read.csv, file = iFile)
                iData$year <- as.numeric(gsub("WBE_|habitats_|\\.csv", "", basename(iFile)))
             
                iData
                
              }))
          
          colnames(tmpData)[1] <- "regio"
          
        } else { 
          
          # Special case
          if (iRegion == "provinces")
            iRegion <- "provincesVoeren"
          
          tmpData <- readS3(FUN = read.csv, file = allFiles)

          if ("NISCODE" %in% colnames(tmpData)) {
            colnames(tmpData)[colnames(tmpData) == "NISCODE"] <- "regio"
          } else {
            colnames(tmpData)[1] <- "regio"
          }
          
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
  
  # Bind wegdensiteit
  densiteitData <- readS3(FUN = data.table::fread, dec = ",", file = "wegdensiteit.csv", bucket = bucket)
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
#' 
#' @param dataDir character, path to data files
#' @return list with meta data for wildschade
#' 
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export
loadMetaSchade <- function(dataDir = system.file("extdata", package = "reportingGrofwild")) {
  
  rawData <- read.csv(file = file.path(dataDir, "meta_schade.csv"), sep = ";")
  
  # Specify currently used wildsoorten
  wildsoorten <- rawData[rawData$variable == "wildsoort", c("group", "name")]
  schadeWildsoorten <- sapply(unique(wildsoorten$group), function(x)
      wildsoorten$name[wildsoorten$group == x], simplify = FALSE)
  
  # Specify currently used SoortNaam (gewas)
  gewassen <- rawData[rawData$variable == "SoortNaam", c("group", "name")]
  schadeGewassen <- sapply(unique(gewassen$group), function(x)
      gewassen$name[gewassen$group == x], simplify = FALSE)
  
  # Specify currently used types schade
  types <- rawData[rawData$variable == "type", c("group", "name")]
  schadeTypes <- unique(types$group)
  
  # Specify currently used subcodes
  schadeCodes <- sapply(schadeTypes, function(x) {
      toReturn <- types$name[types$group == x]
      names(toReturn) <- rawData$name_display[match(toReturn, rawData$name)]
      toReturn
    }, simplify = FALSE)
  
  # Keep after schadeCodes to give them raw list names
  names(schadeTypes) <- rawData$group_display[match(schadeTypes, rawData$group)]
  
  # List with all patterns to search for in indieningType per schadeChoice
  sources <- rawData[rawData$variable == "source", c("name", "name_display")]
  sourcesSchade <- sapply(unique(sources$name_display), function(x)
    sources$name[sources$name_display == x], simplify = FALSE)
  
  
  list(
    wildsoorten = schadeWildsoorten,
    types = schadeTypes,
    codes = schadeCodes,
    sources = sourcesSchade,
    gewassen = schadeGewassen
  )
  
}


