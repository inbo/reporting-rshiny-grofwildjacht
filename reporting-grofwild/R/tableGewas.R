# Project: inbo-grofwildjacht_git
# Frequency table for gewas 
# 
# Author: mvarewyck
###############################################################################


tableGewas <- function(data, variable,
    type = c("provinces", "flanders", "faunabeheerzones")) {
  
  type = match.arg(type)

  data$locatie <- switch(type,
      flanders = "Vlaams Gewest",
      provinces = data$provincie,
      faunabeheerzones = data$FaunabeheerZone)

  # Force all fbz's in the summary table even if never occured    
  if (type == "faunabeheerzones") {
    data$locatie <- factor(data$locatie, levels = as.character(c(1:10)))    
    levelsLocatie <- levels(data$locatie)
  } else {
    data$locatie <- as.factor(data$locatie)    
    levelsLocatie <- levels(data$locatie)
  }

  # select relevant columns
  tableData <- data[, c(variable, c("afschotjaar", "locatie"))]
  # exclude logs with unknown location and NA for variable of interest
  tableData <- tableData[!is.na(tableData[, variable]) & !is.na(tableData$locatie), ]
  # generate counts
  summaryData <- count(tableData, vars = setdiff(names(tableData), "afschotjaar"))
  # generate var x location table
  summaryTable <- dcast(summaryData, !!variable ~ locatie, value.var = "freq")
  
  
#  freqTable <- as.data.frame(table(data[, variable]), stringsAsFactors = FALSE)
#  if (nrow(freqTable) == 0)
#    return(NULL)
#  
#  freqTable <- freqTable[rev(order(freqTable$Freq)), ]
#  if (nrow(freqTable) == 0)
#    return(NULL)
#  
  summaryData <- count(tableData, vars = setdiff(names(tableData), "afschotjaar"))
  
  
  variableLabel <- switch(variable,
#        wildsoort = "Wildsoort",
#        schadeBasisCode = "Type Schade",
#        schadeCode = "Type Subschade",
      SoortNaam = "Gewas")
  
  colnames(freqTable) <- c(variableLabel, "Aantal")
  
  return(freqTable)
    
}
