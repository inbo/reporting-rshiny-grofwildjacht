#' Cumulative plot of yearly number of new introductions
#' 
#' @param region
#' @inheritParams countIntroductionYear
#' @importFrom trias indicator_total_year
#' @return plotly plot with attirubte "proportionInfo" indicating the proportion of 
#' data that was used for the plot (i.e. without missing values for the )
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
      start_year_plot = min(data$first_observed, na.rm = TRUE) - 1
  )
  
  ## convert to plotly object
  p <- ggplotly(plot)
  
  ## add info on proportion of used data
#  attr(p, "proportionInfo") <- paste0("Info beschikbaar en weergegeven voor ", round((sum(!toExclude)/nrow(filteredData))*100, 1),
#                            " % van de totale gegevens (", sum(!toExclude), "/", nrow(filteredData), ")")
  
  return(p)
  
}