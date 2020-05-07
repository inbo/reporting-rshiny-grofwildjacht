#' Plot number of new introductions per year
#' 
#' @param data data.frame with raw data for plotting
#' @param region NULL currently not implemented
#' @importFrom trias indicator_introduction_year
#' @return list with plot and data
#' data that was used for the plot (i.e. without missing values for the )
#' 
#' @export
countIntroductionYear <- function(data, region = NULL){
  
  ## apply region filter
#  filteredData <- data[data$locality == region,]
#  filteredData = data
  
#  ## filter out missing valuse for first_observed
#  toExclude <- is.na(filteredData$first_observed)
#  plotData <- filteredData[!toExclude,]
  
  ## generate plot
  plot <- indicator_introduction_year(
            df = data, 
            start_year_plot = min(data$first_observed) - 1,
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