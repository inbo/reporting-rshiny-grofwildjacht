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
    
    ## recode `source` variable
    
    ## Remove everything up until (jjjj)<space(s)>.<space(s)>
    rawDataFiltered$source <- sub("(.*?)\\(\\d{4}\\)\\s*\\.\\s*", "", rawDataFiltered$source)
    ## Remove everything after the first dot
    rawDataFiltered$source <- sub("\\..*$", "", rawDataFiltered$source)
    
    ## re-define source into shorter names
    rawDataFiltered$source[rawDataFiltered$source == "Ad hoc checklist of alien species in Belgium"] <- "Ad hoc alien species checklist"
    rawDataFiltered$source[rawDataFiltered$source == "Checklist of alien birds of Belgium"] <- "Alien Bird Checklist"
    rawDataFiltered$source[rawDataFiltered$source == "Checklist of non-native freshwater fishes in Flanders, Belgium"] <- "Alien Fish Flanders"
    rawDataFiltered$source[rawDataFiltered$source == "Inventory of alien macroinvertebrates in Flanders, Belgium"] <- "Alien Macroinverts"
    rawDataFiltered$source[rawDataFiltered$source == "Manual of the Alien Plants of Belgium"] <- "Manual of Alien Plants"
    rawDataFiltered$source[rawDataFiltered$source == "RINSE - Pathways and vectors of biological invasions in Northwest Europe"] <- "RINSE2"
    rawDataFiltered$source[rawDataFiltered$source == "World Register of Introduced Marine Species (WRiMS)"] <- "WRiMS"
    
    ## currently not in the data
    rawDataFiltered$source[rawDataFiltered$source == "RINSE - Registry of non-native species in the Two Seas region countries (Great Britain, France, Belgium and the Netherlands)" ] <- "RINSE1"
    rawDataFiltered$source[rawDataFiltered$source == "Registry of introduced terrestrial molluscs in Belgium"] <- "Alien Mollusc checklist"
    rawDataFiltered$source[rawDataFiltered$source == "Catalogue of the Rust Fungi of Belgium"] <- "Belgian rust fungi"
    
    attr(rawDataFiltered, "Date") <- file.mtime(dataFile)
  
  }
  
  return(rawDataFiltered)
  
}