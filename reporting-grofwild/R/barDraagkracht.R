# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplots for  Maatschappelijk draagvlak (F12_1)
#' 
#' @param plotData dataframe 
#' @param ficheNumber character vector with the fiche number of the figure to be plotted
#' @param subplotVar character vector containing the name of the column in plotData
#' for which each unique value a separate subplot is created
#' @param yVar character containing the name of the column in plotData that needs to be plotted on the y-axis
#' 
#' @return plotly-object
#' 
#' @author wverlinden
#' @import plotly
#' @export 

barDraagkracht <- function(plotData, ficheNumber, subplotVar = NULL, yVar = NULL){
  
  if (ficheNumber == "F12_1") {
    plot_ly(plotData, x = as.character(plotData$Jaar), y = plotData[[yVar]], type = 'bar', name = ~Type) %>%
        layout(
            legend = list(title = list(text = "<b> Type </b>")),
            yaxis = list(title = "Aantal"),
            xaxis = list(title = "Jaar"),
            barmode = "stack"
        )
    
  }else if(ficheNumber == "F18_1") {
    
    plot_ly(plotData, x = ~percentage, y = plotData[[yVar]], type = 'bar', name = ~Antwoord
#            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
#                '<b>Antwoord</b>: %{text}<extra></extra>'), text = ~Antwoord
        ) %>%
        layout(
            legend = list(title = list(text = "<b> Antwoord </b>")),
            barmode = "stack",
            yaxis = list(title = "Vraag",
                ticktext = list("Populatie everzwijnen"), 
                tickvals = list(0),
                tickmode = "array"),
            xaxis = list(title = "Percentage") 
        )
    
  } else {
    
    plotList <- list()
    for (i in seq_along(unique(plotData[[subplotVar]]))) {
      
      tmpPlot <- plot_ly(plotData[plotData[[subplotVar]] == unique(plotData[[subplotVar]])[i],], x = ~percentage, 
              y = plotData[[yVar]][plotData[[subplotVar]] == unique(plotData[[subplotVar]])[i]], type = 'bar', name = ~Antwoord, color = ~Antwoord, colors = "RdBu", 
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
              annotations = list(x = 0.5 , 
                  y = 1.025, 
                  text = as.character(unique(plotData[[subplotVar]])[i]), showarrow = F, font = list(size = 16),
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