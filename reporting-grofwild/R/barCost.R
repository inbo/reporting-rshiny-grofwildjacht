# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate stacked bar plot for kost landbouwschade (F09_2)
#' 
#' @param plotData dataframe with columns SoortNaam, afschotjaar and x
#' 
#' 
#' @return plotly object, 
#' plot per year (xaxis) and group (color): freq x schadeBedrag (yaxis)
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom stats aggregate
#' @export 
barCost <- function(schadeData, groupVariable = c("SoortNaam", "season")) {
  
  groupVariable <- match.arg(groupVariable)
  groupLabel <- switch(groupVariable,
    SoortNaam = "Gewas",
    season = "Seizoen"
  )
  
  subData <- subset(schadeData, schadeBasisCode == "GEWAS",
    select = c("schadeBedrag", groupVariable, "afschotjaar"))
  
  summaryData <- count(df = subData, vars = names(subData))
  summaryData$schadeBedrag <- summaryData$schadeBedrag * summaryData$freq
  summaryData$freq <- NULL
  plotData <- aggregate(summaryData$schadeBedrag, by = summaryData[, c(groupVariable, "afschotjaar")], 
    FUN = sum, na.rm = TRUE)
  plotData <- plotData[plotData$x != 0, ]
  # TODO bedrag column will change #325
  
  
  myPlot <- plot_ly(plotData, 
      x = as.character(plotData$afschotjaar), 
      y = ~x , type = 'bar', name = ~get(groupVariable),
      hovertemplate = paste0(
        '<b>Bedrag</b>: %{y:.2f}', '<br>',
        '<b>', groupLabel, '</b>: %{text}<extra></extra>'), 
      text = ~get(groupVariable)) %>%
    layout(
      legend = list(title = list(text = paste0("<b>", groupLabel, "</b>"))),
      yaxis = list(title = "Bedrag"),
      xaxis = list(title = "Afschotjaar"),
      barmode = "stack"
    )
  
  colnames(plotData)[colnames(plotData) == "x"] <- "bedrag"
  
  return(list(plot = myPlot, data = plotData[, c("afschotjaar", groupVariable, "bedrag")]))
  
}
