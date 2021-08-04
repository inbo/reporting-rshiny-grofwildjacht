# Project: grofWild_git
# 
# Author: dbemelmans
###############################################################################

#' Create interactive plot for percentage per hunting method
#' @param data data.frame with raw data for plotting
#' @param regio character, regional level of interest should be one of 
#' \code{c("provinces", "flanders", "faunabeheerzones")}
#' @param jaartallen integer vector, defines the year(s) that should be considered
#' in the plot; if NULL no selection on year(s) is made
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given specie the observed number 
#' per year and per province is plotted in a stacked bar chart}
#' @import plotly
#' @importFrom INBOtheme inbo_palette
#' @export
countHuntingMethod <- function(data, regio, locaties, jaartallen, width = NULL, height = NULL) {
  
  plotData <- data
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  
  # Select data
  if(regio == "provinces") {
    plotData <- plotData[plotData$provincie %in% locaties, ]
  } else if(regio == "faunabeheerzones") {
    plotData <- plotData[plotData$FaunabeheerZone %in% locaties, ]
  }
  
  plotData <- plotData[plotData$afschotjaar %in% jaartallen, c("afschotjaar", "jachtmethode_comp")]
  plotData <- plotData[!is.na(plotData$afschotjaar), ]
  
  plotData$jachtmethode_comp[is.na(plotData$jachtmethode_comp)] <- "onbekende jachtmethode"
  
  
  # Summarize data per province and year
  plotData$afschotjaar <- with(plotData, factor(afschotjaar, levels = 
              min(jaartallen):max(jaartallen)))
  
  summaryData <- melt(table(plotData), id.vars = "afschotjaar")
  
  # Summarize data per year
  totalCount <- table(plotData$afschotjaar)
  
  # For optimal displaying in the plot
  summaryData$jachtmethode_comp <- as.factor(summaryData$jachtmethode_comp)
  summaryData$jachtmethode_comp <- factor(summaryData$jachtmethode_comp, levels = rev(levels(summaryData$jachtmethode_comp)))
  summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
  
  colors <- rev(inbo_palette(n = nlevels(summaryData$jachtmethode_comp)))
  title <- paste0("Afschot van ",
      ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
          jaartallen)
  )
  
  # Create plot
  pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~value, color = ~jachtmethode_comp,
          colors = colors, type = "bar",  width = width, height = height) %>%
      layout(title = title,
          xaxis = list(title = "Jaar"), 
          yaxis = list(title = "Aantal"),
          margin = list(b = 80, t = 100), 
          barmode = ifelse(nlevels(summaryData$afschotjaar) == 1, "group", "stack"),
          annotations = list(x = levels(summaryData$afschotjaar), 
              y = totalCount, 
              text = paste(ifelse(nlevels(summaryData$afschotjaar) == 1, "totaal:", ""), totalCount),
              xanchor = 'center', yanchor = 'bottom',
              showarrow = FALSE),
          showlegend = TRUE)  
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  return(list(plot = pl, data = summaryData))
}

