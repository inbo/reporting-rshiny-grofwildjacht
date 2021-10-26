# Filters for subsetting the data in plot functions
# 
# Author: mvarewyck
###############################################################################





#' Filter \code{plotData} based on the \code{indieningType} if required
#' @param plotData data.frame, to be filtered
#' @param sourceIndicator character, source used to filter \code{data} ('indieningType' column)
#' should be one of \code{c("E-loker", "HVV", "Natuurpunt")}.
#' @param returnStop character, should be one of \code{c("message", "data")}
#' what needs to be returned if the filtered data has no rows left
#' @return data.frame, filtered version of \code{plotData}
#' 
#' @author mvarewyck
#' @export
filterSource <- function(plotData, sourceIndicator = NULL,
  returnStop = c("message", "data")) {
  
  returnStop <- match.arg(returnStop)
  
  if (!is.null(sourceIndicator)) {
    
    sources <- unlist(sapply(sourceIndicator, function(source) sourcesSchade()[[source]]))
    plotData <- plotData[plotData$indieningType %in% sources, ]
    
    if (nrow(plotData) == 0) {
      if (returnStop == "message")
        stop("Geen data beschikbaar voor de geselecteerde bron: ", sourceIndicator, ". ")
    }
  }
  
  return(plotData)
  
}




#' Filter \code{plotData} based on leeftijd and geslacht bron
#' @inheritParams filterSource
#' @return data.frame, filtered version of \code{plotData}
#' 
#' @author mvarewyck
#' @export
filterLeeftijdGeslacht <- function(plotData, sourceIndicator = NULL, 
  sourceIndicator_geslacht = NULL, returnStop = c("message", "data")) {
  
  returnStop <- match.arg(returnStop)
  
  if (!is.null(sourceIndicator) && !"leeftijd_bron" %in% colnames(plotData))
    stop("Bron voor leeftijd niet in data")
  
  if (!is.null(sourceIndicator_geslacht) && !"geslacht_bron" %in% colnames(plotData))
    stop("Bron voor geslacht niet in data")
  
  if (sourceIndicator == "inbo") {
    
    # To prevent error with R CMD check
    leeftijd_bron <- NULL
    
    # filters out NA and 'meldingsformulier'
    plotData <- subset(plotData, leeftijd_bron == "inbo")
    
  }
  
  if (sourceIndicator_geslacht == "inbo") {
    # To prevent error with R CMD check
    geslacht_bron <- NULL
    
    # filters out NA and 'meldingsformulier' en 'onbekend'
    plotData <- subset(plotData, geslacht_bron == "inbo")
    
  } else if (sourceIndicator_geslacht == "both"){
    # To prevent error with R CMD check
    geslacht_bron <- NULL
    
    # filters out NA and 'onbekend'
    plotData <- subset(plotData, !is.na(geslacht_bron) & geslacht_bron != "onbekend")
    
  }
  
  if (nrow(plotData) == 0)
    if (returnStop == "message")
      stop("Geen data beschikbaar. ")
  
  return(plotData)
  
}
