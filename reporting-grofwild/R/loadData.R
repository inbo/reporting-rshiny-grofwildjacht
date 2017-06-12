#' Read shape data from zipped file
#' @param zipFile the pathname of the zip file 
#' @param id character string, the name of a column in the shapefile .dbf 
#' containing the ID values to be used; default value is NULL
#' @return a SpatialPolygonsDataFrame object, with polygons and data as provided
#' in the zipFile
#' @importFrom maptools readShapePoly
#' @importFrom sp CRS spTransform
#' @importFrom utils unzip
#' @export
readShapeData <- function(zipFile, id = NULL) {
  
  tmpDir <- tempdir()
  unlink(file.path(tmpDir, "*.shp"))
  unlink(file.path(tmpDir, "*.shp.*"))
  unzip(zipFile, exdir = tmpDir)
  allShapeFiles <- list.files(path = tmpDir, pattern = ".shp", 
      full.names = TRUE)
  if (length(allShapeFiles) == 0)
    stop("Unzipped folder contains no files. Please make sure that the files are not in a subfolder.")
  shapeData <- readShapePoly(allShapeFiles, IDvar = id, proj4string = CRS("+init=epsg:31370"))
  shapeData <- sp::spTransform(shapeData, CRS("+proj=longlat +datum=WGS84"))
  
  return(shapeData)
  
}


#' Read ecology data
#' @return data.frame, loaded ecology data
#' @author mvarewyck
#' @importFrom utils read.csv
#' @export
loadEcologyData <- function() {
  
  ecologyData <- read.csv(file.path(system.file("extdata", package = "reportingGrofwild"),
          "rshiny_reporting_data_ecology.csv"), sep = ";")
#  xtabs( ~ provincie + wildsoort, data = ecologyData)
  
  
  ## Only for "Wild zwijn" separate province "Voeren" is considered, otherwise part of "Limburg"
  ecologyData$provincie <- factor(ifelse(ecologyData$wildsoort == "Wild zwijn", 
          as.character(ecologyData$provincie), 
          ifelse(ecologyData$provincie == "Voeren", 
              "Limburg", 
              as.character(ecologyData$provincie))),
      levels = c("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams-Brabant", "Antwerpen", "Limburg", "Voeren"))
#  xtabs( ~ provincie + wildsoort, data = ecologyData)
  
  
  return(ecologyData)
  
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


