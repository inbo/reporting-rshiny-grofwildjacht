# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplots for  Maatschappelijk draagvlak (F12_1)
#' 
#' @param plotData data.frame 
#' @param subplotVar character vector containing the name of the column in plotData
#' for which each unique value a separate subplot is created
#' @param xVar character, column in \code{plotData} for x-axis;
#' default value is 'percentage'
#' @param yVar character, column in \code{plotData} for y-axis
#' 
#' @return list with plotly object and data.frame
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom RColorBrewer brewer.pal
#' @export 

barDraagkracht <- function(plotData, subplotVar = NULL, 
  xVar = "percentage", yVar = NULL) {
  
  if (xVar == "percentage") {
    
    plotData$percentage <- as.numeric(plotData$percentage)
    plotData$Antwoord <- factor(plotData$Antwoord , 
      levels = c('Erg veel toegenomen',
        'Heel erg positief', 'Ja, zeker wel', 'Zeer groot', 'Erg belangrijk', 'Veel toegenomen',
        'Positief', 'Ja, waarschijnlijk wel', 'Groot', 'Belangrijk', 'Beetje toegenomen',
        'Neutraal', 'Hetzelfde gebleven',
        'Negatief', 'Nee, waarschijnlijk niet', 'Klein', 'Niet belangrijk', 'Beetje afgenomen',
        'Heel erg negatief', 'Nee, zeker niet', 'Zeer klein', 'Helemaal niet belangrijk', 'Veel afgenomen',
        'Erg veel afgenomen',
        'Onbestaand',
        'Geen mening', 'Geen idee'))
    
    plotData$Antwoord <- droplevels(plotData$Antwoord)  
    nExcept <- sum(c("Onbestaand", "Geen mening", "Geen idee") %in% levels(plotData$Antwoord))
    myColors <- c(
      brewer.pal(n = length(levels(plotData$Antwoord)) - nExcept, name = "RdBu"), 
      rev(brewer.pal(n = 3, name = "Greys"))
    )[1:length(levels(plotData$Antwoord))]
     
  }
  
  if (!is.null(subplotVar)) {
    
    groupLevels <- unique(plotData[[subplotVar]])
    
    plotList <- lapply(groupLevels, function(iVar) {
      
      plot_ly(
          data = plotData[plotData[[subplotVar]] %in% iVar, ], 
          x = ~get(xVar), 
          y = ~get(yVar), 
          type = 'bar', name = ~Antwoord, color = ~Antwoord, colors = myColors, 
          legendgroup = ~Antwoord, 
          showlegend = iVar == groupLevels[1], 
          hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
            '<b>Antwoord</b>: %{text}'),
          text = ~Antwoord
        ) %>%
        layout(
          annotations = list(x = 50, 
            y = 1.025, 
            text = as.character(iVar), showarrow = FALSE, font = list(size = 16),
            yref = 'paper'),
          legend = list(title = list(text = "<b> Antwoord </b>")),
          barmode = "stack",
          yaxis = list(title = "Sector"),
          xaxis = list(title = "Percentage") 
        ) 
      
    })
    
    myPlot <- do.call(subplot, c(plotList, shareY = TRUE, shareX = TRUE))
    
    extraVars <- c("Antwoord", subplotVar)
    
  } else if (xVar == "percentage") {
    
    myPlot <- plot_ly(plotData, x = ~get(xVar), y = ~get(yVar), 
        type = 'bar', color = ~Antwoord, colors = myColors
#            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
#                '<b>Antwoord</b>: %{text}')
        ) %>%
        layout(
            legend = list(title = list(text = "<b> Antwoord </b>")),
            barmode = "stack",
            yaxis = list(title = yVar,
                ticktext = list("Populatie everzwijnen"), 
                tickvals = list(0),
                tickmode = "array"),
            xaxis = list(title = "Percentage") 
        )
      
      extraVars <- c("Antwoord")
      
    } else {
      
        myPlot <- plot_ly(plotData, x = as.character(plotData[[xVar]]), 
            y = plotData[[yVar]], type = 'bar', name = ~Type) %>%
          layout(
            legend = list(title = list(text = "<b> Type </b>")),
            yaxis = list(title = "Aantal"),
            xaxis = list(title = "Jaar"),
            barmode = "stack"
          )
        
        extraVars <- c("Type")
        
      } 
 
  
  return(list(plot = myPlot, data = plotData[, c(extraVars, xVar, yVar), with = FALSE]))
  
}