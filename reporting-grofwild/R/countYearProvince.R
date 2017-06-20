# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Create interactive plot for counts per province and year
#' 
#' Figure p. 4 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param data data.frame with raw data for plotting
#' @param wildNaam character, the name that should be displayed for y-label
#' @param jaartallen integer vector, defines the year(s) that should be considered
#' in the plot; if NULL no selection on year(s) is made
#' @param doodsoorzaak character vector, defines the cause(s) of death that should
#' be considered in the plot 
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return plotly object, for a given specie the observed number 
#' per year and per province is plotted in a stacked bar chart
#' @import plotly
#' @importFrom reshape2 melt
#' @importFrom INBOtheme inbo.2015.colours
#' @export
countYearProvince <- function(data, wildNaam = "", jaartallen = NULL, 
    doodsoorzaak = c("afschot", "valwild"), 
    width = NULL, height = NULL) {
  
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  # Select data
  plotData <- data[data$afschotjaar %in% jaartallen &
          data$doodsoorzaak %in% doodsoorzaak, c("afschotjaar", "provincie")]
  plotData <- plotData[!is.na(plotData$afschotjaar) & !is.na(plotData$provincie), ]
  
  # Summarize data per province and year
  plotData$afschotjaar <- with(plotData, factor(afschotjaar, levels = 
          min(afschotjaar):max(afschotjaar)))

  summaryData <- melt(table(plotData), id.vars = "afschotjaar")
  
  # Summarize data per year
  totalCount <- table(plotData$afschotjaar)
  
  
  # For optimal displaying in the plot
  summaryData$provincie <- factor(summaryData$provincie, levels = rev(levels(summaryData$provincie)))
  summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
  
  colors <- rev(inbo.2015.colours(n = nlevels(summaryData$provincie)))
  title <- paste0(wildNaam, " ",
      ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
              jaartallen),
      " (", paste(doodsoorzaak, collapse = " en "), ")")
    
  
  # Create plot
  plot_ly(data = summaryData, x = ~afschotjaar, y = ~value, color = ~provincie,
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
              showarrow = FALSE))  
  
  
}
