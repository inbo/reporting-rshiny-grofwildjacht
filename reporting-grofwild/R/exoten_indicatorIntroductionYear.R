#' Plot number of new introductions per year
#' 
#' @param data 
#' @param region
#' @importFrom trias indicator_introduction_year
#' @return plotly plot with attirubte "proportionInfo" indicating the proportion of 
#' data that was used for the plot (i.e. without missing values for the )
#' 
#' @export
countIntroductionYearExoten <- function(data, region = NULL){
  
  ## apply region filter
#  filteredData <- data[data$locality == region,]
  filteredData = data
  
  ## filter out missing valuse for first_observed
  toExclude <- is.na(filteredData$first_observed)
  plotData <- filteredData[!toExclude,]
  
  ## generate plot
  plot <- indicator_introduction_year(
            df = plotData, 
            start_year_plot = min(plotData$first_observed)
          )
  
  ## convert to plotly object
  p <- ggplotly(plot)
  
  ## add info on proportion of used data
  attr(p, "proportionInfo") <- paste0("Info beschikbaar en weergegeven voor ", round((sum(!toExclude)/nrow(filteredData))*100, 1),
                            " % van de totale gegevens (", sum(!toExclude), "/", nrow(filteredData), ")")
  
  return(p)
  
}