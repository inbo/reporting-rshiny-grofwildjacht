# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' Create interactive plot for biotoop description - irrespective of species
#' 
#' @param data data.frame, background data for the WBE, as read from \code{loadWbeHabitats}
#' @param jaar numeric, year of interest
#' @inheritParams countAgeGender
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given year a summary of the WBE parameters
#' defining the percentage of land, water, forest etc}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'Naam': }{characteristic}
#' \item{'Value': }{value}
#' }
#' }
#' }
#' @import plotly
#' @author mvarewyck
#' @export
barBiotoop <- function(data, jaar = NULL, 
  width = NULL, height = NULL) {
  
  
  # For R CMD check
  year <- NULL
    
  if (is.null(jaar))
    jaar <- unique(data$year)
  
  wbeInfo <- subset(data, year == jaar)
  percVars <- grep("perc", colnames(wbeInfo), value = TRUE)
  
  plotData <- data.frame(
    name = gsub("perc_", "", percVars),
    value = round(unlist(wbeInfo[, percVars]*100), 2)
  )
  # For correct display in plot
  plotData$name <- factor(plotData$name, levels = rev(plotData$name))
  plotData <- plotData[nrow(plotData):1, ]
  
  
  totalCounts <- data.frame(
    name = c("Totale oppervlakte (ha)", "Totale oppervlakte (km2)"),
    value = round(unlist(wbeInfo[, c("Area_ha", "Area_km2")]), 2)
  )
  
  colors <- replicateColors(nColors = 1)$colors[1]
  
 
  # Create plot
  pl <- plot_ly(data = plotData, x = ~value, y = ~name, color = "Percentage", 
      colors = colors,
      type = "bar", orientation = 'h', width = width, height = height) %>%
    
    layout(title = paste0("Totale oppervlakte: ", totalCounts$value[2], " km\U00B2"),
      xaxis = list(title = "", zeroline = FALSE, showline = FALSE), 
      yaxis = list(title = "", zeroline = FALSE, showline = FALSE),    
      margin = list(l = 100), 
      annotations = list(x = plotData$value,  
        y = levels(plotData$name),
        text = paste(plotData$value, "%"),
        xanchor = 'left', yanchor = 'center',
        showarrow = FALSE)) %>%
    
    add_annotations(text = "Percentages (%)",
      xref = "paper", yref = "paper", x = 0, xanchor = "right",
      y = 1, yanchor = "top", showarrow = FALSE)  
  
  finalData <- rbind(
    totalCounts,    
    plotData[nrow(plotData):1, ]
  )
  rownames(finalData) <- NULL
  colnames(finalData) <- c("Naam", "Waarde")
  
  # To prevent warnings in UI
  pl$elementId <- NULL
    
  return(list(plot = pl, data = finalData))
  
}

