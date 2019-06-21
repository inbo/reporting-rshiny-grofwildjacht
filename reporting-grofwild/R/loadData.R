#' Read all shape data from geojson files
#' @param dataDir character vector, defines the path to the data files
#' @param showProgress boolean, whether to show progress window in the shiny app;
#' Note, if used outside shiny app, this will cause an error; if FALSE progress
#' is printed in the console
#' @param tolerance numeric, defines the tolerance in the Douglas-Peuker algorithm;
#' larger values will impose stronger simplification; default value is 0.01
#' @return save to dataDir object spatialData, i.e. a list with for each 
#' spatial level a SpatialPolygonsDataFrame object, 
#' with polygons and data as provided in the dataDir; spatial levels are 
#' flanders, provinces, communes and provincesVoeren (Voeren as separate province)
#' @importFrom sp CRS spTransform SpatialPolygonsDataFrame
#' @importFrom methods slot
#' @importFrom maptools unionSpatialPolygons spRbind
#' @importFrom rgdal readOGR
#' @importFrom rgeos gSimplify
#' @importFrom shiny incProgress
#' @export
readShapeData <- function(dataDir = system.file("extdata", package = "reportingGrofwild"),
    showProgress = FALSE, tolerance = 0.001) {
  
  
  allLevels <- c("Vlaanderen" = "flanders", "Provincies" = "provinces", 
      "Gemeenten" = "communes")
  
  ## New code for geojson files
  spatialData <- lapply(allLevels, function(iLevel) {
        
        if (showProgress)
          incProgress(1/3, 
              detail = paste0("Geo Data: ", 
                  names(allLevels)[allLevels == iLevel], 
                  " (", which(allLevels == iLevel), "/3)"))
        
        file <- file.path(dataDir, paste0(iLevel, ".geojson"))
#        # Check whether we can use readOGR()
#        "GeoJSON" %in% ogrDrivers()$name
        shapeData <- readOGR(dsn = file, layer = "OGRGeoJSON", 
            verbose = !showProgress)
        shapeData <- sp::spTransform(shapeData, CRS("+proj=longlat +datum=WGS84"))
        
        # Create factor for region names
        if (iLevel == "provinces")
          shapeData$NAAM <- factor(shapeData$NAAM, levels = c("West-Vlaanderen",
                  "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg"))
        
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
  
  # Try to simplify polygons
  spatialData <- lapply(spatialData, function(iData) {
        
        simpleShapeData <- gSimplify(spgeom = iData, tol = tolerance)
        
        if (length(simpleShapeData) != length(iData))
          stop("The number of polygons in original shapeData is: ", length(iData),
              "\nThe number of polygons in simplified shapeData is: ", length(simpleShapeData),
              "\nPlease decrease value for tolerance")
        
        iData <- SpatialPolygonsDataFrame(Sr = simpleShapeData, 
            data = data.frame(iData@data, stringsAsFactors = FALSE))
        
        
        return(iData)
        
      })
  
  
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
#' @param shapeData list with objects of class SpatialPolygonsDataFrame as 
#' returned by \code{\link{readShapeData}}; if not NULL, commune names are 
#' matched between geography (raw) data and spatial (shape) data 
#' @return data.frame, loaded ecology or geography data; 
#' and attribute 'Date', the date that this data file was created
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export
loadRawData <- function(dataDir = system.file("extdata", package = "reportingGrofwild"),
    type = c("eco", "geo"), shapeData = NULL) {
  
  type <- match.arg(type)
  
  
  dataFile <- file.path(dataDir, switch(type,
          "eco" = "rshiny_reporting_data_ecology.csv",
          "geo" = "rshiny_reporting_data_geography.csv"))
  
  rawData <- read.csv(dataFile, sep = ";", stringsAsFactors = FALSE)
#  xtabs( ~ provincie + wildsoort, data = rawData)
  
  ## Replace decimal comma by dot
  if ("ontweid_gewicht" %in% names(rawData))
    rawData$ontweid_gewicht <- as.numeric(sub("\\,", ".", rawData$ontweid_gewicht))
  
  ## Replace decimal comma by dot
  if ("lengte_mm" %in% names(rawData))
    rawData$lengte_mm <- as.numeric(sub("\\,", ".", rawData$lengte_mm))
  
  ## Mismatch names with spatial (shape) data for "Vlaams Brabant"
  rawData$provincie <- factor(ifelse(rawData$provincie == "Vlaams-Brabant",
          "Vlaams Brabant", as.character(rawData$provincie)))
#  xtabs( ~ provincie + wildsoort, data = rawData)
  
  ## Mismatch names with spatial (shape) data for multiple communes
  if (type == "geo" & !is.null(shapeData)) {
    
    communeData <- shapeData$communes@data
    # Data source: http://portal.openbelgium.be/he/dataset/gemeentecodes
    gemeenteData <- read.csv(file.path(dataDir, "gemeentecodes.csv"), 
        header = TRUE, sep = ",")
    
    geoNis <- gemeenteData$NIS.code[match(rawData$postcode_afschot_locatie, gemeenteData$Postcode)]
    geoName <- as.character(communeData$NAAM)[match(geoNis, communeData$NISCODE)] 
    
#    tmpData <- data.frame(old = as.character(rawData$gemeente_afschot_locatie), 
#        new = geoName, stringsAsFactors = FALSE)
#    tmpData[which(tmpData$old != tmpData$new), ]
    
    rawData$gemeente_afschot_locatie <- geoName
    
  }
  
  ## Only for "Wild zwijn" separate province "Voeren" is considered, otherwise part of "Limburg"
  ## Re-order factor levels for plots
  rawData$provincie <- factor(ifelse(rawData$wildsoort == "Wild zwijn", 
          as.character(rawData$provincie), 
          ifelse(rawData$provincie == "Voeren", 
              "Limburg", 
              as.character(rawData$provincie))),
      levels = c("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren"))
#  xtabs( ~ provincie + wildsoort, data = rawData)
  
  
  
  if (type == "eco") {
    
    # Re-define doodsoorzaak "verdelging ANB" into afschot
    rawData$doodsoorzaak[rawData$doodsoorzaak == "verdelging ANB"] <- "afschot" 
    
    # Re-define "Adult" as "Volwassen" for leeftijd + ordering levels
    rawData$leeftijdscategorie_MF[rawData$leeftijdscategorie_MF == "Adult"] <- "Volwassen"
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak == "Adult"] <- "Volwassen"
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak %in% c("", "Onbekend")] <- "Niet ingezameld"
    
    # for Figure 13: combine age and gender: 'type' column 
    # (to do the matching with the openingstijden table)
    idx <- which(rawData$wildsoort == "Ree")
    typeRee <- ifelse(
        rawData[idx, "leeftijdscategorie_MF"]  == "Kits", "kits",
        ifelse(rawData[idx, "leeftijdscategorie_MF"] %in% c("Jongvolwassen", "Volwassen"),
            ifelse(rawData[idx, "geslacht.MF"] == 'Mannelijk', "bok", 
                ifelse(rawData[idx, "geslacht.MF"] == 'Vrouwelijk', "geit", "")
            ),
            ""))
    rawData$type <- ""
    rawData$type[idx] <- typeRee
    rawData$type[is.na(rawData$type)] <- ""
    
    # for Figure 28: combine age and gender, with subcategory for young adult
    male <- rawData$geslacht.MF == "Mannelijk"
    female <- rawData$geslacht.MF == "Vrouwelijk"
    ageGender <- with(rawData,
        ifelse(leeftijdscategorie_MF	== "Kits", ifelse(male, "Bokkits", ifelse(female, "Geitkits", "")),
            ifelse(leeftijdscategorie_MF	== "Jongvolwassen", ifelse(male, "Jaarlingbok", ifelse(female, "Smalree", "")),
                ifelse(leeftijdscategorie_MF	== "Volwassen", ifelse(male, "Bok", ifelse(female, "Geit", "")), "")
            )))
    ageGender[is.na(ageGender)] <- ""
    
    rawData$ageGender <- factor(ageGender, 
        levels = c("", "Geitkits", "Bokkits", "Smalree", "Jaarlingbok", "Geit", "Bok"))
    
    # for Figure p. 27, 28: compute cheek length
    rawData$onderkaaklengte <- rowMeans(
        rawData[, c("onderkaaklengte_links", "onderkaaklengte_rechts")],
        na.rm = TRUE)
    
  }
  
  attr(rawData, "Date") <- file.mtime(dataFile)
  
  
  return(rawData)
  
}



#' Name file given content information
#' @param species character, species of the file content
#' @param year numeric vector, year span of the file content 
#' @param content character, more information on the file
#' @param fileExt character, extension of the file
#' @return character, suggested file name pasting together \code{species},
#' \code{year}, \code{content}, \code{fileExt}
#' @author mvarewyck
#' @export
nameFile <- function(species, year, content, fileExt) {
  
  paste0(
      gsub(pattern = " ", replacement = "_", x = species), "_",
      if (length(year) > 1) paste(year, collapse = "-") else year, "_",
      content, 
      ".", fileExt
  )
  
}



#' Print for debugging
#' @param x R object that will be printed
#' @return NULL, print output in the console
#' @export
printer <- function(x){
  
  cat("MV", deparse(substitute(x)), "\n")
  print(x)
  
}



#' Paste elements of vector into string vector (for testing)
#' @param x vector
#' @return string of the form "c(<elements of x>)"
#' @export
pasteToVector <- function(x) {
  
  if (is.character(x))
    elements <- paste(paste0("'", x, "'"), collapse = ", ")
  else elements <- paste(x, collapse = ", ")
  
  paste0("c(", elements, ")")
  
}


