#' Read exoten data
#' 
#' By default data for which \code{first_observed < 1950} is excluded.
#' @param dataDir character vector, defines the path to the data file(s)
#' @param type data type, "indicators" for indicator data
#' @return data.table, loaded indicator data; 
#' and attribute 'Date', the date that this data file was created
#' @importFrom data.table fread
#' @export
loadExotenData <- function(
    dataDir = system.file("extdata", package = "reportingGrofwild"),
    type = c("indicators")) {
	
  type = match.arg(type)
  
  dataFile <- file.path(dataDir, switch(type,
          "indicators" = "exoten_data_input_checklist_indicators.tsv"))
  
  rawData <- fread(dataFile, stringsAsFactors = FALSE)
  
  ## exclude data before 1950
  toExclude <- (rawData$first_observed < 1950 & !is.na(rawData$first_observed))
  warning("Exoten: ", sum(toExclude), " observaties dateren van voor 1950 en zijn dus uitgesloten")
  rawData <- rawData[!toExclude, ]
  
  
  attr(rawData, "Date") <- file.mtime(dataFile)
  
  return(rawData)
  
}