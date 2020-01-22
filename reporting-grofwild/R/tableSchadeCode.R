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
#' @importFrom DT datatable formatRound
#' @importFrom magrittr "%>%"
#' @importFrom utils tail
#' @export
tableSchadeCode <- function(data, jaar = NULL,
        type = c("provinces", "flanders", "faunabeheerzones")) {
  
  type = match.arg(type)
  schadeNaam <- unique(data$schadeCode)
  
  if (is.null(jaar))
    stop("Gelieve jaartal te selecteren")
  
  allData <- data
  allData$locatie <- switch(type,
          flanders = "Vlaams Gewest",
          provinces = allData$provincie,
          faunabeheerzones = allData$FaunabeheerZone)
  
  allData$locatie <- as.factor(allData$locatie)    
  levelsLocatie <- levels(allData$locatie)
  
  allData$schadeCode <- as.factor(allData$schadeCode)    
  levelsSchadeCode <- levels(allData$schadeCode)
  
  if (!jaar %in% allData$afschotjaar)
    stop("Niet beschikbaar: Geen data voor het gekozen jaar")
  
  # Select data
  tableData <- allData[allData$afschotjaar == jaar, c("afschotjaar", "locatie", "schadeBasisCode", "schadeCode")]
  # Exclude records with provincie = NA, jaar = NA
  tableData <- tableData[!is.na(tableData$afschotjaar) & !is.na(tableData$locatie), ]
    
  # Summary of the data
  summaryData <- count(tableData, vars = names(tableData))
  
  # Include all possible locations
  # TODO include all possible schadecodes as well?
  fullData <- expand.grid(
      afschotjaar = jaar,
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
  summaryTable <- summaryTable[, c(colnames(summaryTable)[1],headerCombinations$schadeCode)]
  
  
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
  
  # Nice column names
  # TODO: nice column names currently don't work due to how table header is build up

#  columnFullNames <- fullNames(colnames(summaryTable))
#  colnames(summaryTable)[!is.na(columnFullNames)] <- names(columnFullNames)[!is.na(columnFullNames)]

  # Manage table header with multiline
  tableHeader <- tags$table(
      class  = 'display',
      tags$thead(
          tags$tr(
            tags$th(rowspan = 2, names(summaryTable)[1]) ,
            lapply(names(columsPerSchadeBasisCode), function(iName) {
           	  tags$th(colspan = columsPerSchadeBasisCode[iName], iName)
            }),
            tags$th(rowspan = 2, tail(names(summaryTable), n=1))
          ),
          tags$tr(
              lapply(names(summaryTable)[names(summaryTable) %in% headerCombinations$schadeCode], tags$th)
          )
      )
   )
  

  summaryTableDT <- datatable(summaryTable, rownames = FALSE, container = tableHeader,
              selection = "single",
              options = list(dom = 'ltirp')) %>%
              formatRound(colnames(summaryTable), digits = 0)
          

  return(summaryTableDT)
  


	
}