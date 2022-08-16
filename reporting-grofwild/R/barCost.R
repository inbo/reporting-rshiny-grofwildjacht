# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate stacked bar plot for kost landbouwschade (F09_2)
#' 
#' @param plotData dataframe with columns SoortNaam, afschotjaar and x
#' 
#' 
#' @return list containing a bar plot and a line plot
#' 
#' @author wverlinden
#' @import plotly
#' @export 

barCost <- function(plotData){
  
  # bar plot
  barPlot <- plot_ly(plotData, x = as.character(plotData$afschotjaar), y = ~x , type = 'bar', name = ~SoortNaam) %>%
      layout(
          legend = list(title = list(text = "<b> Gewas </b>")),
          yaxis = list(title = "Bedrag"),
          xaxis = list(title = "Afschotjaar"),
          barmode = "stack"
      )
  
  # line plot
  linePlot <- plot_ly(plotData, x = ~afschotjaar,  y = ~x, type = "scatter", mode = "lines", name = ~SoortNaam)%>%
      layout(
          legend = list(title = list(text = "<b> Gewas </b>")),
          yaxis = list(title = "Bedrag"),
          xaxis = list(title = "Afschotjaar")
      )
  
  return(
      list(
          barPlot = barPlot,
          linePlot = linePlot
          )
      )
}
