#' Load all shape data from zipped files
#' @return list with for each spatial level a SpatialPolygonsDataFrame object, 
#' with polygons and data as provided in the zipFile; spatial levels are 
#' flanders, provinces, communes and provincesVoeren (Voeren as separate province)
#' @importFrom sp CRS spTransform SpatialPolygonsDataFrame
#' @importFrom utils unzip
#' @importFrom methods slot
#' @importFrom maptools readShapePoly unionSpatialPolygons spRbind
#' @export
loadShapeData <- function() {
  
  dataDir <- system.file("extdata", package = "reportingGrofwild")
  
  allLevels <- c("flanders", "provinces", "communes")
  
  
  ## Read all spatial data
  spatialData <- lapply(allLevels, function(iLevel) {
        
        tmpDir <- tempdir()
        unlink(file.path(tmpDir, "*.shp"))
        unlink(file.path(tmpDir, "*.shp.*"))
        unzip(file.path(dataDir, paste0(iLevel, ".zip")), exdir = tmpDir)
        allShapeFiles <- list.files(path = tmpDir, pattern = ".shp", 
            full.names = TRUE)
        if (length(allShapeFiles) == 0)
          stop("Unzipped folder contains no files. Please make sure that the files are not in a subfolder.")
        shapeData <- readShapePoly(allShapeFiles, IDvar = NULL, proj4string = CRS("+init=epsg:31370"))
        shapeData <- sp::spTransform(shapeData, CRS("+proj=longlat +datum=WGS84"))
        
        
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
  
  
  return(spatialData)
  
}

#' read openingstijden data
#' @return data.frame with columns:
#' \itemize{
#' \item{'Soort': }{specie}
#' \item{'Type': }{specie type}
#' \item{'Jaar': }{year}
#' \item{'Startdatum': }{start datum, in the format '\%d/\%m/\%Y'}
#' \item{'Stopdatum': }{end datum, in the format '\%d/\%m/\%Y'}
#' }
#' @importFrom utils read.csv
#' @export
loadOpeningstijdenData <- function(){
  
  pathFile <- file.path(system.file("extdata", package = "reportingGrofwild"),
      "Openingstijden_grofwild.csv")
  
  rawData <- read.csv(pathFile, sep = ";", stringsAsFactors = FALSE)
  
  return(rawData)
  
}


#' Read toekenningen (Ree) data
#' @return data.frame with columns:
#' \itemize{
#' \item{'Provincie': }{character, province}
#' \item{'Jaar': }{integer, year}
#' \item{'Labeltype': }{character, type of Ree, one of \code{c("Geiten", "Bokken", "Kitsen")}}
#' \item{'Aantal': }{integer, frequencie per categorie}
#' }
#' @importFrom utils read.csv
#' @export
loadToekenningen <- function() {
  
  pathFile <- file.path(system.file("extdata", package = "reportingGrofwild"),
      "Toekenningen_ree.csv")
  
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
  
  return(rawData)
  
}



#' Read ecology or geography data
#' @param type data type, "eco" for ecology data and "geo" for geography data
#' @param shapeData list with objects of class SpatialPolygonsDataFrame as 
#' returned by \code{\link{loadShapeData}}; if not NULL, commune names are 
#' matched between geography (raw) data and spatial (shape) data 
#' @return data.frame, loaded ecology or geography data
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export
loadRawData <- function(type = c("eco", "geo"), shapeData = NULL) {
  
  type <- match.arg(type)
  
  
  dataFile <- switch(type,
      "eco" = "rshiny_reporting_data_ecology.csv",
      "geo" = "rshiny_reporting_data_geography.csv")
  
  rawData <- read.csv(file.path(system.file("extdata", package = "reportingGrofwild"),
          dataFile), sep = ";", stringsAsFactors = FALSE)
#  xtabs( ~ provincie + wildsoort, data = rawData)
  
  ## Replace decimal comma by dot
  if ("ontweid_gewicht" %in% names(rawData))
    rawData$ontweid_gewicht<- as.numeric(sub("\\,", ".", rawData$ontweid_gewicht))
  
  ## Mismatch names with spatial (shape) data for "Vlaams Brabant"
  rawData$provincie <- factor(ifelse(rawData$provincie == "Vlaams-Brabant",
          "Vlaams Brabant", as.character(rawData$provincie)))
#  xtabs( ~ provincie + wildsoort, data = rawData)
  
  ## Mismatch names with spatial (shape) data for multiple communes
  if (type == "geo" & !is.null(shapeData)) {
    
    communeData <- shapeData$communes@data
    # Data source: http://portal.openbelgium.be/he/dataset/gemeentecodes
    gemeenteData <- read.csv(file.path(system.file("extdata", package = "reportingGrofwild"),
            "gemeentecodes.csv"), header = TRUE, sep = ";")
    
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
  
  
  # Re-define "Adult" as "Volwassen" for leeftijd + ordering levels
  if (type == "eco") {
    
    rawData$leeftijdscategorie_MF[rawData$leeftijdscategorie_MF == "Adult"] <- "Volwassen"
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak == "Adult"] <- "Volwassen"
    
    rawData$Leeftijdscategorie_onderkaak[rawData$Leeftijdscategorie_onderkaak == ""] <- "Niet ingezameld"
    
  } 
  
  # add 'type' column to do the matching with the openingstijden table (only for ree)
  if(type == "eco"){
    idx <- which(rawData$wildsoort == "Ree")
    typeRee <- ifelse(
        rawData[idx, "leeftijdscategorie_MF"]  == "Kits", "kits",
        ifelse(rawData[idx, "leeftijdscategorie_MF"] %in% c("Jongvolwassen", "Volwassen"),
            ifelse(rawData[idx, "geslacht.MF"] == 'Mannelijk', "bok", 
                ifelse(rawData[idx, "geslacht.MF"] == 'Vrouwelijk', "geit", "")
            ),
            ""))
    rawData$type[idx] <- typeRee
    rawData$type[is.na(rawData$type)] <- ""
  }
  
  return(rawData)
  
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


