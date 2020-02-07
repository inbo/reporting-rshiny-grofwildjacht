# Project: inbo-grofwildjacht_git
# Frequency table for gewas 
# 
# Author: mvarewyck
###############################################################################


#' Create summary table for gewas by region and year.
#' @inheritParams tableSchadeCode
#' @param variable character, defines the dependent variable in the table besides \code{type}
#' @return data.frame, number of opbservations per region and per \code{variable}
#' @author Eva Adriaensen
#' @importFrom plyr count
#' @importFrom reshape2 dcast
#' @export
tableGewas <- function(data, jaartallen = NULL, variable,
    type = c("provinces", "flanders", "faunabeheerzones")) {
  
  type = match.arg(type)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  allData <- data

  allData$locatie <- switch(type,
      flanders = as.factor("Vlaams Gewest"),
      provinces = allData$provincie,
      faunabeheerzones = allData$FaunabeheerZone)

  # Define all provinces/fbz's in the summary table even if never occured     
  levelsLocatie <- switch(type,
      flanders = levels(allData$locatie),
      provinces = levels(allData$locatie),
      faunabeheerzones = c(1:10)) #levels fbz's are hardcoded
  
  # redefine location levels
  allData$locatie <- factor(allData$locatie, levels = levelsLocatie)
    
  # select relevant columns
  tableData <- allData[allData$afschotjaar %in% jaartallen, c(variable, c("afschotjaar", "locatie"))] #nrow(tableData) [1] 1525
  # exclude logs with unknown location or NA for variable of interest
  tableData <- tableData[!is.na(tableData[, variable]) & !is.na(tableData$locatie), ] #nrow(tableData) [1] 1255
  # generate counts
  summaryData <- count(tableData, vars = setdiff(names(tableData), "afschotjaar"))
  
  if (nrow(summaryData) == 0)
    return(NULL)
  
  # include all provinces/fbz
  fullData <- expand.grid(
      unique(tableData[, variable]),
      locatie = levelsLocatie
          )
  names(fullData)[1] <- variable
  fullSummaryData <- merge(summaryData, fullData, all = TRUE)
  
  # rename var of interest
  colnames(fullSummaryData)[which(colnames(fullSummaryData) == variable)] <- "varOfInterest" 
  
  # generate var x location table
  summaryTable <- dcast(fullSummaryData, varOfInterest ~ locatie, value.var = "freq")
  summaryTable[is.na(summaryTable)] <- 0
  
  # add col and row sum 
  if (type != "flanders") {
    summaryTable <- cbind(summaryTable, 
        Vlaanderen = apply(subset(summaryTable, select = -1), 1, sum))
  }
  summaryTable <- rbind(summaryTable,
      c(varOfInterest = "Alle", as.list(apply(subset(summaryTable, select = -1), 2, sum))))   
  
  variableLabel <- switch(variable,
#        wildsoort = "Wildsoort",
#        schadeBasisCode = "Type Schade",
#        schadeCode = "Type Subschade",
      SoortNaam = "Gewas")
  
  colnames(summaryTable)[colnames(summaryTable) == "varOfInterest"] <- variableLabel
  
  return(summaryTable)
    
}
