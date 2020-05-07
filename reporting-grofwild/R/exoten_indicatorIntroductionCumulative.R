#' Cumulative plot of yearly number of new introductions
#' 
#' @inheritParams countIntroductionYear
#' @importFrom trias indicator_total_year
#' @return list with plot and data
#' 
#' @export
cumulativeIntroductionYear <- function(data, region = NULL){
  
  ## apply region filter
#  filteredData <- data[data$locality == region,]
#  filteredData = data
  
#  ## filter out missing valuse for first_observed
#  toExclude <- is.na(filteredData$first_observed)
#  plotData <- filteredData[!toExclude,]
  
  ## generate plot
  plot <- indicator_total_year(
      df = data, 
      start_year_plot = min(data$first_observed, na.rm = TRUE) - 1,
      x_lab = "Jaar",
      y_lab = "Aantal ge\u00EFntroduceerde uitheemse soorten"
  
  )
  
  ## convert to plotly object
  p <- ggplotly(plot)
  
  ## add info on proportion of used data
#  attr(p, "proportionInfo") <- paste0("Info beschikbaar en weergegeven voor ", round((sum(!toExclude)/nrow(filteredData))*100, 1),
#                            " % van de totale gegevens (", sum(!toExclude), "/", nrow(filteredData), ")")
  
  return(list(plot = p, data = data))
  
}