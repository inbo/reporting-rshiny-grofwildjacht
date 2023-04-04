#' Read ecology, geography, wildschade, kbo_wbe or waarnemingen data
#' 
#' Data is preprocessed by createRawData() at INBO
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-wbe-uat-data"
#' @param type data type, "eco" for ecology data and "geo" for geography data
#' 
#' @return data.frame, loaded data
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export
loadRawData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  type = c("eco", "geo", "wildschade", "kbo_wbe", "waarnemingen")) {
  
  type <- match.arg(type)
  
  
  dataFile <- switch(type,
          "eco" = "rshiny_reporting_data_ecology_processed.csv",
          "geo" = "rshiny_reporting_data_geography_processed.csv",
          "wildschade" = "WildSchade_georef_processed.csv",
          "kbo_wbe" = "Data_Partij_Cleaned_processed.csv",
          "waarnemingen" = "waarnemingen_wild_zwijn_processed.csv"
        )
  
  rawData <- as.data.frame(readS3(FUN = data.table::fread, file = dataFile, bucket = bucket))
  
  # Convert to factors
  newLevels <- loadMetaEco()
  toFactors <- names(newLevels)[names(newLevels) %in% colnames(rawData)]
  rawData[toFactors] <- lapply(toFactors, function(x) 
      droplevels(factor(rawData[[x]], levels = c(newLevels[[x]], "Onbekend"))))
  
  if (type == "wildschade") {
    
    # create shape data
    rawData <- st_as_sf(rawData, coords = c("x", "y"), crs = "+init=epsg:31370")
    rawData <- st_transform(rawData, crs = "+proj=longlat +datum=WGS84")
    
  }
  
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
#' @param spatialData list with each element an object of class sf as created 
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
          if ("NISCODE" %in% colnames(spatialData[[iRegion]]))
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
    ),
    provincie = list("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren")
  )
  
  toReturn$Leeftijdscategorie_onderkaak <- c(toReturn$leeftijd_comp, "Niet ingezameld")
  
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


