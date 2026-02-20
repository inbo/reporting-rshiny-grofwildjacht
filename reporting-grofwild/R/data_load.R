#' Common arguments for the functions that load data
#' @param path (optional) string, path to local folder 
#' containing the data, by default value of the environment
#' variable: '\emph{reportingGrofwild-data-path} '
#' - for development purpose only
#' @name reportingGrofwild-data-load
NULL

#' Load spatial data
#' @param WBE_NR integer, if not NULL select only relevant data for given WBE;
#' default value is NULL 
#' @inheritParams readS3
#' @inheritParams reportingGrofwild-data-load
#' @return list of sf objects
#' 
#' @author mvarewyck
#' @importFrom sf st_read st_layers
#' @export
loadShapeData <- function(
  WBE_NR = NULL,
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  path = Sys.getenv("reportingGrofwild-data-path")
) {
	
  if (all(is.na(WBE_NR)))
    return(NULL)
  
  if (is.null(WBE_NR) | length(WBE_NR) > 100) {
  	if(!identical(path, "")){
  		load(
        file = file.path(path, "spatialDataWBE_sf.RData"), 
        envir = environment()
      )
  	}else{
      # From 100 it is faster to load single object, but requires more memory (+-80 MB)
      readS3(file = "spatialDataWBE_sf.RData", envir = environment())
  	}
  	return(spatialDataWBE)
  }
  
  # 1st layer (WBE)
	file <- paste0("spatialDataWBE/", WBE_NR[1], ".RData")
 	if(!identical(path, "")){
		load(file.path(path, file), envir = environment())
	}else{
    readS3(file = file, bucket = bucket, envir = environment())
	}
  
  if (length(WBE_NR) > 1)
    for (wbe in WBE_NR[-1]) {
      envTmp <- new.env()
      file <- paste0("spatialDataWBE/", wbe, ".RData")
      if(!identical(path, "")){
        load(file.path(path, file), envir = envTmp)
      }else{
        readS3(file = file, bucket = bucket, envir = envTmp)
      }
      spatialDataWBE <- sapply(names(spatialDataWBE), function(iLayer)
          rbind(spatialDataWBE[[iLayer]], envTmp$spatialDataWBE[[iLayer]]))
    }
  
  spatialDataWBE

}

#' Load spatial data for wolves
#' @param type character data type to load
#' @inheritParams readS3
#' @inheritParams reportingGrofwild-data-load
#' @return list of sf objects
#' 
#' @author mvarewyck
#' @importFrom sf st_transform read_sf
#' @export
loadWolfShapeData <- function(
  type = c("territory", "utm", "gemeenten"),
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  path = Sys.getenv("reportingGrofwild-data-path")
) {
  
  type <- match.arg(type)
  
  dataFile <- switch(type,
    "utm" = "utm10_vl.geojson",
    "territory" = "wolf_territories_Flanders.geojson",
    "gemeenten" = "wolf_gemeenten.geojson"
  )
  
  if (!identical(path, "")) {
    data <- sf::read_sf(file.path(path, dataFile))
  } else {
    obj <- aws.s3::get_object(dataFile, bucket = bucket)
    geojson_text <- rawToChar(obj)
    
    data <- geojsonsf::geojson_sf(geojson_text)
    
    if (st_crs(data)$epsg != 4326) {
      data <- st_transform(data, crs = 4326)
    }
    
    if (type == "gemeenten") {
      colnames(data)[colnames(data) == "OPPERVL"] <- "AREA"
    }
  }
  
  data
  
}

#' Read ecology, geography, wildschade, kbo_wbe or waarnemingen data
#' 
#' Data is preprocessed by createRawData() at INBO
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-wbe-uat-data"
#' @param type data type, "eco" for ecology data and "geo" for geography data
#' @inheritParams reportingGrofwild-data-load
#' @return data.frame, loaded data
#' @importFrom arrow read_parquet
#' @importFrom sf st_as_sf st_transform
#' @author mvarewyck
#' @export
loadRawData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  path = Sys.getenv("reportingGrofwild-data-path"),
  type = c("eco", "geo", "wildschade", "kbo_wbe", "waarnemingen")
) {
  
  type <- match.arg(type)
  
  dataFile <- switch(type,
      "eco" = "rshiny_reporting_data_ecology_processed.parquet",
      "geo" = "rshiny_reporting_data_geography_processed.parquet",
      "wildschade" = "WildSchade_georef_processed.parquet",
      "kbo_wbe" = "Data_Partij_Cleaned_processed.parquet",
      "waarnemingen" = "waarnemingen_wild_zwijn_processed.parquet"
    )

  dataFile <- if(!identical(path, "")){
    file.path(path, dataFile)
  }else{
    file.path("s3:/", bucket, dataFile)
  }

  rawData <- read_parquet(file = dataFile)
  
  return(rawData)
  
}


#' Read all data related to Wolf species
#' 
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-wbe-uat-data"
#' @param type character data type to load
#' @inheritParams reportingGrofwild-data-load
#' @return data.frame, loaded data
#' @importFrom arrow read_parquet
#' @importFrom sf st_as_sf st_transform st_point st_sfc
#' @importFrom dplyr n
#' @author mvarewyck
#' @export
loadWolfData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  path = Sys.getenv("reportingGrofwild-data-path"),
  type = c("locaties", "utm", "overzicht", "terr", "schade")
) {
  
  # For R CMD check
  X_coord <- Y_coord <- NULL
  
  type <- match.arg(type)
  
  dataFile <- switch(type,
    "locaties" = "wolf_monitoring_puntlocaties.csv",
    "utm" = "wolf_monitoring_UTMhokken.csv",
    "overzicht" = "wolf_monitoring_jaarlijksoverzicht.csv",
    "terr" = "wolf_monitoring_territoria.csv",
    "schade" = "wolf_schade_overzicht.csv"
  )
  
  data <- if (!identical(path, "")){
      read.csv(file.path(path, dataFile), header = TRUE)
    } else {
      readS3(FUN = read.csv, header = TRUE, sep = ";", row.names = NULL, file = dataFile, bucket = bucket)
    }
  
  if (type == "locaties") {
    data <- data %>%
      st_as_sf(coords = c("x", "y"), crs = 31370 )
    
    data <- sf::st_transform(data, crs = 4326)
    
    colnames(data)[colnames(data) == "monitoringsjaar"] <- "year"
  } else if (type == "overzicht") {
    colnames(data)[colnames(data) == "Year"] <- "year"
    data$X_Coord <- as.numeric(as.character(data$X_Coord))
    data$Y_Coord <- as.numeric(as.character(data$Y_Coord))

    sf_with_geom <- data %>% filter(!is.na(X_Coord), !is.na(Y_Coord)) %>%
      st_as_sf(coords = c("X_Coord", "Y_Coord"), crs = 4326, remove = FALSE)
    sf_no_geom <- data %>% filter(is.na(X_Coord) | is.na(Y_Coord)) %>%
      mutate(geometry = st_sfc(
          lapply(seq_len(n()), function(i) st_point()),
          crs = 4326
        )) %>% st_as_sf()
    
    data <- rbind(sf_with_geom, sf_no_geom)
  } else if (type == "utm") {
    colnames(data)[colnames(data) == "monitoringsjaar"] <- "year"
  } else if (type == "schade") {
    data$Datum <- as.Date(data$Datum)
    colnames(data)[colnames(data) == "Jaar"] <- "year"
    colnames(data)[colnames(data) == "Gemeente"] <- "gemeente_afschot_locatie"
    colnames(data)[colnames(data) == "Provincie"] <- "provincie"
    colnames(data)[colnames(data) == "Soort"] <- "wildsoort"
  }
  
  data
  
}



#' Read gemeentes data
#' @inheritParams loadRawData
#' @return data.frame with NIS.code, Postcode and Gemeente
#' 
#' @author mvarewyck
#' @export
loadGemeentes <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  pathFile <- "gemeentecodes.csv"

  if(!identical(path, "")){
	  read.csv(file.path(path, pathFile), header = TRUE)
  }else{
    readS3(FUN = read.csv, header = TRUE, file = pathFile, bucket = bucket)
  }
  
}


#' read openingstijden data
#' @inheritParams loadRawData
#' @return data.frame with columns:
#' \itemize{
#' \item 'Soort':  specie 
#' \item 'Type':  specie type 
#' \item 'Jaar':  year 
#' \item 'Startdatum':  start datum, in the format '\%d/\%m/\%Y' 
#' \item 'Stopdatum':  end datum, in the format '\%d/\%m/\%Y' 
#' }
#' and attribute 'Date', the date that this data file was created
#' @importFrom utils read.csv
#' @importFrom aws.s3 get_bucket
#' @export
loadOpeningstijdenData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  path = Sys.getenv("reportingGrofwild-data-path")){
  
  pathFile <- "Openingstijden_grofwild.csv"
  
  if(!identical(path, "")){
    pathFile <- file.path(path, pathFile)
    rawData <- read.csv(
      file = pathFile, 
      sep = ";", stringsAsFactors = FALSE
    )
    modifTime <- file.mtime(pathFile)
  }else{
    rawData <- readS3(
      FUN = read.csv, sep = ";", stringsAsFactors = FALSE,
      file = pathFile, bucket = bucket
    )
    tmpInfo <- aws.s3::get_bucket(bucket = bucket)
    modifTime <- tmpInfo[[which(sapply(tmpInfo, function(x) x$Key == pathFile))]]$LastModified[1]
        
  }
  
  rawData$Type <- simpleCap(rawData$Type)

  attr(rawData, "Date") <- as.Date(modifTime)
  
  return(rawData)
  
}


#' Read toekenningen (Ree) data
#' @inheritParams loadRawData
#' @return data.frame with columns:
#' \itemize{
#' \item 'labeltype':  character, type of Ree, one of \code{c("geit", "bok", "kits")} 
#' \item 'WBE_Naam':  character, WBE name 
#' \item 'labeljaar':  integer, year 
#' \item 'provincie_toek': character, province 
#' \item 'toegekend':  integer, no. of assigned animals 
#' \item 'verwezenlijkt':  integer, no. of shot animals 
#' \item 'percentage_verwezenlijkt':  numeric, percentage shot animals 
#' \item 'KboNummer_Toek':  character, WBE KBO number 
#' }
#' and attribute 'Date', the date that this data file was created
#' @importFrom utils read.csv
#' @export
loadToekenningen <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")),
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  pathFile <- "Verwezenlijkt_categorie_per_afschotplan.csv"
  
  rawData <- if(!identical(path, "")){
    pathFile <- file.path(path, pathFile)
    read.csv(
      file = pathFile,  
      sep = ";", stringsAsFactors = FALSE
    )
  }else{
    readS3(FUN = read.csv, sep = ";", stringsAsFactors = FALSE,
      file = pathFile, bucket = bucket
    )
  }
  
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
#' @param regionLevels character vector, for which regions load the habitat data;
#' if NULL loaded for all levels; default value is NULL
#' @return named list with data.frame for each region level
#' 
#' @author mvarewyck
#' @export
loadHabitats <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
  regionLevels = NULL,
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  # For R CMD check
  habitatData <- NULL
  
  pathFile <- "habitatData.RData"
  
  rawData <- if(!identical(path, "")){
    load(file = file.path(path, pathFile), envir = environment())
  }else{
    readS3(file = pathFile, bucket = bucket, envir = environment())
  }
  
  allLevels <- names(habitatData)
  
  if (!is.null(regionLevels))
    allLevels <- allLevels[allLevels %in% regionLevels]
  
  return(habitatData[allLevels])
  
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
      c("Frisling (<6m)", NA, NA, NA),
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
    provincie = list("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg"),
    wettelijk_kader = c("reguliere jacht", "bijzondere jacht", "bestrijding", "bestrijding veldwachter"),
    periode = c("dag", "nacht", "schemer"),
    periode_wettelijk = c("dag", "nacht", "schemer")
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


#' List all wildsoorten and corresponding group
#' @inheritParams loadMetaSchade 
#' @return data.frame listing for all defined species
#' \code{group} species group and \code{name} species name
#' 
#' @author mvarewyck
#' @export
loadWildsoorten <- function(dataDir = system.file("extdata", package = "reportingGrofwild")) {
  
  rawData <- read.csv(file = file.path(dataDir, "meta_schade.csv"), sep = ";")
  
  # Specify currently used wildsoorten
  rawData[rawData$variable == "wildsoort", c("group", "name")]
    
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
  schadeWildsoorten <- groupSpecies(allSpecies = wildsoorten)
  
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
  
  # List with all data sources
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

#' Load Spread data
#' @inheritParams loadRawData
#' @export
loadSpreadData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  # For R CMD check
  spreadData <- NULL
  
  pathFile <- "spreadData_sf.RData"
  
  if(!identical(path, "")){
    load(file = file.path(path, pathFile), envir = environment())
  }else{
    readS3(file = pathFile, bucket = bucket, envir = environment())
  }
  
  return(spreadData)
  
}

#' Load Traffic data
#' @inheritParams loadRawData
#' @export
loadTrafficData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  # For R CMD check
  trafficData <- NULL
  
  pathFile <- "trafficData.RData"
  
  if(!identical(path, "")){
    load(file = file.path(path, pathFile), envir = environment())
  }else{
    readS3(file = pathFile, bucket = bucket, envir = environment())
  }
  
  return(trafficData)
  
}


#' Read Draagvlak data
#' @inheritParams loadRawData 
#' @return list with for each draagvlak category (impacts, beleid, maatregelen, aanwezigheid)
#' a data.frame
#' 
#' @author mvarewyck
#' @export
loadDraagvlakData <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  dataFiles <- c(
    "PCI_impacts_stakeholders.csv",
    "PCI_beleid_stakeholders.csv",
    "PCI_maatregelen_stakeholders.csv",
    "PCI_impacts_breed_publiek.csv",
    "PCI_maatregelen_breed_publiek.csv",
    "PCI_aanwezigheid_breed_publiek.csv",
    "Data_aantrekkingskracht_breed_publiek.csv"
  )
  
  dataSets <- sapply(dataFiles, function(iFile) {
      tmpData <- if (!identical(path, ""))
          read.csv(file = file.path(path, iFile)) else
          readS3(file = iFile, bucket = bucket)
      tmpData$group <- paste(strsplit(gsub(".csv", "", iFile), split = "_", )[[1]][-(1:2)], collapse = "_")
      tmpData
    }, simplify = FALSE, USE.NAMES = TRUE)
  
  # Combine multiple files
  categories <- unique(sapply(dataFiles, function(x) strsplit(x, "_")[[1]][2]))
  toReturn <- sapply(categories, function(iCategory) {
      tmp <- do.call(rbind, dataSets[grep(iCategory, names(dataSets))])
      rownames(tmp) <- NULL
      # Summarize 'Antwoord_binair'
      if ("Antwoord_binair" %in% colnames(tmp)) {
        tmp <- array2DF(tapply(tmp, ~ Year + Sector + Soort + Antwoord + group, function(x)
              list(Aantal_tot = sum(x == "Ja"), totaal = nrow(x))))
        tmp$Aantal_tot <- as.numeric(tmp$Aantal_tot)
        tmp$totaal <- as.numeric(tmp$totaal)
      }
      tmp$X <- NULL
      tmp$Year <- as.factor(tmp$Year)
      if ("vraag_label" %in% colnames(tmp))
        tmp$vraag_label <- droplevels(as.factor(tmp$vraag_label))
      
      tmp
    }, simplify = FALSE, USE.NAMES = TRUE)
  
  return(toReturn)
  
}

#' Fetch available years for bever data
#' @inheritParams loadRawData
#' @export
loadBeverAvailableYears <- function(
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  objs <- get_bucket(bucket, max = Inf)
  keys <- vapply(objs, function(x) x$Key, character(1))
  
  predFiles <- grep("^bever_predictie_([0-9]{4})\\.geojson$", basename(keys), value = TRUE)
  predYears <- sort( sub("^bever_predictie_([0-9]{4})\\.geojson$", "\\1", predFiles) )
  predYears <- as.numeric(predYears)
  predYears <- predYears[predYears >= as.numeric(format(Sys.Date(), "%Y"))]
  
  versprFiles <- grep("^bever_verspreiding_([0-9]{4})\\.geojson$", basename(keys), value = TRUE)
  versprYears <- sort( sub("^bever_verspreiding_([0-9]{4})\\.geojson$", "\\1", versprFiles) )
  
  return(list(
      verspreiding = versprYears, 
      prediction = predYears)
  )
  
}


#' Load prediction Bever data
#' @param year character, year of interest
#' @inheritParams loadRawData
#' @export
loadBeverData <- function(
  year, type = "predictie",
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
  path = Sys.getenv("reportingGrofwild-data-path")) {
  
  # For R CMD check
  beverData <- NULL
  
  pathFile <- paste0("bever_", type, "_", year, ".geojson")

  if (!identical(path, "")) {
    beverData <- sf::read_sf(file.path(path, pathFile))
  } else {
    obj <- aws.s3::get_object(pathFile, bucket = bucket)
    geojson_text <- rawToChar(obj)
    
    beverData <- geojsonsf::geojson_sf(geojson_text)
  }
  
  if (type == "predictie") {
    beverData$Vstgngs <- factor(beverData$Vstgngs, levels = c("Laag", "Gemiddeld", "Hoog"))
    beverData$label <- with(beverData, paste(
        "<p>", "% overstroming:", round(beverData$Ovrstrm,0), "</br>",
        "% rust:", round(beverData$Rustzns,0),"</br>",
        "# HRL-soorten:", round(beverData$HRLsrtn,0),"</br>",
        "# vissen:", round(beverData$HRLvssn,0),"</br>",
        "% habitat:", round(beverData$Habitts,0),"</br>",
        "</p>"))
  }
  
  return(beverData)
  
}
