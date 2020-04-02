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
  
  if (type == "indicators") {

    # recode missing values to NA
    rawData <- fread(dataFile, stringsAsFactors = FALSE)
    rawData[rawData == ""] <- NA
                     
    
    ## extract necessary columns
    rawDataFiltered <- rawData[, c(
            # Period - slider should use first_observed
            "first_observed", "last_observed", 
            # Taxonomy
            "kingdom", "phylum", "class", "order", "family",
            # Locality
            "locality", "locationId", "native_range",
            # Degree establishment
            "degree_of_establishment",
            # Pathway
            "pathway_level1", "pathway_level2",
            # Habitat
            # "habitat",  ## I think it's easier to use the 3 below / habitat is not NA, but "" when missing
            "marine", "freshwater", "terrestrial",
            # Source
            "source"
        )]
        
    ## exclude data before 1950
    toExclude <- (rawDataFiltered$first_observed < 1950 & !is.na(rawDataFiltered$first_observed))
    warning("Exoten: ", sum(toExclude), " observaties dateren van voor 1950 en zijn dus uitgesloten")
    rawDataFiltered <- rawDataFiltered[!toExclude, ]
    
    
    attr(rawDataFiltered, "Date") <- file.mtime(dataFile)
  
  }
  
  return(rawDataFiltered)
  
}