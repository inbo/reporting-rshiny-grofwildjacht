# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' Create interactive plot for biotoop description - irrespective of species
#' 
#' @param data data.frame, background data for the WBE, as read from \code{loadHabitats}
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
    
  if (!is.null(jaar))
    subData <- subset(data, year == jaar) else
    subData <- data
  
  percVars <- grep("perc", colnames(subData), value = TRUE)
  
  plotData <- melt(subData[, c("regio", percVars)], id.vars = "regio")
  
  if (nrow(plotData) == 0)
    stop("Geen data beschikbaar")
  
  plotData$value <- round(plotData$value * 100, 2)
  plotData$variable <- gsub("perc_", "", plotData$variable)
    
  # For correct display in plot
  plotData$variable[plotData$variable == "bos"] <- "bos & natuur"
  plotData$variable <- factor(plotData$variable, 
    levels = c("andere", "bebouwd", "water", "landbouw", "grasland", "bos & natuur"))
  
  tmpRegions <- round(tapply(plotData$value, plotData$variable, mean), 2)
  totalRegions <- data.frame(
    variable = names(tmpRegions),
    value = tmpRegions) 
  
  totalCounts <- melt(subData[, c("Area_ha", "Area_km2", "regio")], id.vars = "regio")
  totalCounts$value <- round(totalCounts$value)
  totalCounts$name <- totalCounts$variable
  totalCounts$variable <- ifelse(totalCounts$variable == "Area_ha", 
    "Totale oppervlakte (ha)", "Totale oppervlakte (km2)")
  
  # More than 9 regions -> total
  warningText <- NULL
  selectedRegions <- unique(plotData$regio)
  if (length(selectedRegions) > 9) {
    plotData <- totalRegions
    plotData$regio <- "Totaal"
    selectedRegions <- "Totaal"
    warningText <- "Door het grote aantal gekozen regio's wordt het gemiddelde percentage weergegeven. Selecteer minder regio's om individuele percentages te bekomen." 
  }
  colors <- replicateColors(nColors = length(selectedRegions))$colors
  names(colors) <- selectedRegions
  
  # Create plot
  pl <- plot_ly(data = plotData, x = ~value, y = ~variable, 
      color = ~as.factor(regio), colors = colors,
      hovertemplate = paste('%{y} <br>%{x:/100\U0025}'),
      type = "bar", orientation = 'h', width = width, height = height) %>%
    
    layout(title = paste0("Totale oppervlakte: ", 
        sum(totalCounts$value[totalCounts$name == "Area_km2"]), " km\U00B2"),
      xaxis = list(title = "", zeroline = FALSE, showline = FALSE, ticksuffix = "%"), 
      yaxis = list(title = "", zeroline = FALSE, showline = FALSE),    
      margin = list(l = 100)
    ) %>%
    
    add_annotations(text = "Percentages (%)",
      xref = "paper", yref = "paper", x = 0, xanchor = "right",
      y = 1, yanchor = "top", showarrow = FALSE)  
  
  
  # Percentage printed at top of bar
  if (length(unique(plotData$regio)) == 1) {
      pl <- pl %>% layout(
        annotations = list(x = totalRegions$value,  
        y = totalRegions$variable,
        text = paste(totalRegions$value, "%"),
        xanchor = 'left', yanchor = 'center',
        showarrow = FALSE))
  }
    
  totalCounts$name <- NULL
  finalData <- rbind(
    totalCounts,    
    plotData[nrow(plotData):1, ]
  )
  rownames(finalData) <- NULL
  colnames(finalData) <- c("Regio", "Naam", "Waarde")
  
  # To prevent warnings in UI
  pl$elementId <- NULL
    
  return(list(plot = pl, data = finalData, warning = warningText))
  
}

