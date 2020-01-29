# Project: inbo-grofwildjacht_git
# Frequency table for type schade 
# 
# Author: Eva Adriaensen
###############################################################################


#' Create summary table for schadeCode by region.
#' The table created is a datatable.
#' @param type character, defines the region variable of interest in the table
#' @inheritParams tableProvince
#' @return data.frame, number of observations per region \code{locaties} and per schadeCode
#' @author Eva Adriaensen
#' @importFrom reshape2 dcast
#' @importFrom plyr count
#' @importFrom magrittr "%>%"
#' @importFrom utils tail
#' @export
tableSchadeCode <- function(data, jaartallen = NULL,
        type = c("provinces", "flanders", "faunabeheerzones")) {
  
  type = match.arg(type)
  schadeNaam <- unique(data$schadeCode)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  allData <- data
  allData$locatie <- switch(type,
          flanders = "Vlaams Gewest",
          provinces = allData$provincie,
          faunabeheerzones = allData$FaunabeheerZone)
  
  # Force all fbz's in the summary table even if never occured    
  if (type == "faunabeheerzones") {
    allData$locatie <- factor(allData$locatie, levels = as.character(c(1:10)))    
    levelsLocatie <- levels(allData$locatie)
  } else {
    allData$locatie <- as.factor(allData$locatie)    
    levelsLocatie <- levels(allData$locatie)
  }
  
  allData$schadeCode <- as.factor(allData$schadeCode)    
  levelsSchadeCode <- levels(allData$schadeCode)
    
  # Select data
  tableData <- allData[allData$afschotjaar %in% jaartallen, c("afschotjaar", "locatie", "schadeBasisCode", "schadeCode")]
    
  if (nrow(tableData) == 0)
    stop("Niet beschikbaar: Geen data voor de gekozen periode")
  
  # Exclude records with provincie = NA, afschotjaar = NA
  tableData <- tableData[!is.na(tableData$afschotjaar) & !is.na(tableData$locatie), ]
    
  # Summary of the data
  summaryData <- count(tableData, vars = setdiff(names(tableData), "afschotjaar"))
  
  # Include all possible locations
  # TODO include all possible schadecodes as well?
  fullData <- expand.grid(
      locatie = levelsLocatie,
      schadeCode = unique(allData$schadeCode)
      )
  
  summaryData <- merge(summaryData, fullData, all = TRUE)
  summaryVariables <- "freq"
  
  # Long to wide table
  summaryTable <- dcast(summaryData, locatie ~ schadeCode, value.var = "freq")
  
  # Optimal displaying of the table
  summaryTable[is.na(summaryTable)] <- 0
  
  # Extract header info and counts
  headerCombinations <- unique(allData[c("schadeBasisCode", "schadeCode")])
  # group by schadeBasisCode
  headerCombinations <- headerCombinations[order(headerCombinations$schadeBasisCode),]
  headerCombinations$schadeCode <- as.character(headerCombinations$schadeCode, stringsAsFactors = FALSE)
  
  # number of schadeCodes by schadeBasisCode
  columsPerSchadeBasisCode <- table(headerCombinations$schadeBasisCode)
  
  # group columns together from same schadeBasisCode
  summaryTable <- summaryTable[, c(colnames(summaryTable)[1],fullNames(headerCombinations$schadeCode))]
  
  
  # Add row and column sum
  levels(summaryTable$locatie) <- c(levels(summaryTable$locatie), "Vlaanderen")
  
  if (type != "flanders") {
    summaryTable <- rbind(summaryTable, 
      c(locatie = "Vlaanderen", as.list(apply(summaryTable[, levelsSchadeCode], 2, sum))))
  }

  summaryTable <- cbind(summaryTable, 
      Totaal = apply(summaryTable[, levelsSchadeCode], 1, sum))
  
  # Rename locatie
  names(summaryTable)[names(summaryTable) == "locatie"] <- "Locatie"
  
  # Full column names
  columnFullNames <- names(fullNames(colnames(summaryTable)))
  colnames(summaryTable)[!is.na(columnFullNames)] <- columnFullNames[!is.na(columnFullNames)]

  # Manage table header with multiline
  tableHeader <- tags$table(
      class  = 'display',
      tags$thead(
          tags$tr(
            tags$th(rowspan = 2, names(summaryTable)[1]) ,
            lapply(names(columsPerSchadeBasisCode), function(iName) {
           	  tags$th(colspan = columsPerSchadeBasisCode[iName], names(fullNames(iName)))
            }),
            tags$th(rowspan = 2, tail(names(summaryTable), n=1))
          ),
          tags$tr(
              #TODO
              lapply(names(summaryTable)[names(summaryTable) %in% names(fullNames(headerCombinations$schadeCode))], tags$th)
          )
      )
   )


  return(list(data = summaryTable, header = tableHeader))
  


	
}