# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplots for  Maatschappelijk draagvlak (F12_1)
#' 
#' @param plotData dataframe 
#' @param ficheNumber character vector with the fiche number of the figure to be plotted

#' @return plotly-object
#' 
#' @author wverlinden
#' @import plotly
#' @export 

barDraagkracht <- function(plotData, ficheNumber){
  
  if (ficheNumber == "F12_1") {
  plot_ly(plotData, x = as.character(plotData$Jaar), y = ~Aantal, type = 'bar', name = ~Type) %>%
      layout(
          legend = list(title = list(text = "<b> Type </b>")),
          yaxis = list(title = "Aantal"),
          xaxis = list(title = "Jaar"),
          barmode = "stack"
      )
} else {
  
  plotList <- list()
  for (i in seq_along(unique(plotData$Year))) {
    
    tmpPlot <- plot_ly(plotData[plotData$Year == unique(plotData$Year)[i],], x = ~percentage, 
            y = ~Sector, type = 'bar', name = ~Antwoord, color = ~Antwoord, colors = "RdBu", 
            legendgroup = ~Antwoord, 
            showlegend = if(i == 1){
                  TRUE
                } else {
                  FALSE
                }, 
            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
                '<b>Antwoord</b>: %{text}<extra></extra>'),
            text = ~Antwoord
        ) %>%
        layout(
            annotations = list(x = 0.5 , y = 1, text = as.character(unique(plotData$Year)[i]), showarrow = F, font = list(size = 16),
                xref='paper', yref='paper'),
            legend = list(title = list(text = "<b> Antwoord </b>")),
            barmode = "stack",
            yaxis = list(title = "Sector"),
            xaxis = list(title = "Percentage") 
        ) 
    
    plotList[[i]] <- tmpPlot
  }
  do.call(subplot, c(plotList, shareY = TRUE, shareX = TRUE))
  
}
}