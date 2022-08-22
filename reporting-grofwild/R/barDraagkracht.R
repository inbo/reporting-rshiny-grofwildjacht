# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplots for  Maatschappelijk draagvlak (F12_1)
#' 
#' @param data data.frame 
#' @param groupVariable character vector containing the name of the column in plotData
#' for which each unique value a separate subplot is created
#' @param xVar character, column in \code{plotData} for x-axis;
#' default value is 'percentage'
#' @param yVar character, column in \code{plotData} for y-axis
#' @inheritParams barBiotoop
#' 
#' @return list with plotly object and data.frame
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom RColorBrewer brewer.pal
#' @export 
barDraagkracht <- function(data, groupVariable = NULL, 
  xVar = "percentage", yVar = NULL, width = 1000, height = NULL) {
  
  if (xVar == "percentage") {
    
    data$percentage <- as.numeric(data$percentage)
    data$Antwoord <- factor(data$Antwoord , 
      levels = c('Erg veel toegenomen',
        'Heel erg positief', 'Ja, zeker wel', 'Zeer groot', 'Erg belangrijk', 'Veel toegenomen',
        'Positief', 'Ja, waarschijnlijk wel', 'Groot', 'Belangrijk', 'Beetje toegenomen',
        'Neutraal', 'Hetzelfde gebleven',
        'Negatief', 'Nee, waarschijnlijk niet', 'Klein', 'Niet belangrijk', 'Beetje afgenomen',
        'Heel erg negatief', 'Nee, zeker niet', 'Zeer klein', 'Helemaal niet belangrijk', 'Veel afgenomen',
        'Erg veel afgenomen',
        'Onbestaand',
        'Geen mening', 'Geen idee'))
    
    data$Antwoord <- droplevels(data$Antwoord)  
    nExcept <- sum(c("Onbestaand", "Geen mening", "Geen idee") %in% levels(data$Antwoord))
    myColors <- c(
      brewer.pal(n = length(levels(data$Antwoord)) - nExcept, name = "RdBu"), 
      rev(brewer.pal(n = 3, name = "Greys"))
    )[1:length(levels(data$Antwoord))]
    
  }
  
  if (!is.null(groupVariable)) {
    
    groupLevels <- unique(data[[groupVariable]])
    
    plotList <- lapply(groupLevels, function(iVar) {
        
        plot_ly(
            data = data[data[[groupVariable]] %in% iVar, ], 
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
    
    extraVars <- c("Antwoord", groupVariable)
    
  } else if (xVar == "percentage") {
    
    # rename y-axis ticktext
    yLabels <- unique(data[[yVar]])
    yLabels[yLabels == "populatie_evolutie"] <- "Populatie everzwijnen"
    yLabels[yLabels == "schade_landbouw_evolutie"] <- "Schade aan de landbouw"
    yLabels[yLabels == "schade_privpub_evolutie"] <- "Schade aan privéterreinen"
    yLabels[yLabels == "schade_verkeer_evolutie"] <- "Schade in het verkeer"
    
    myPlot <- plot_ly(data, x = ~get(xVar), y = ~get(yVar), 
        type = 'bar', color = ~Antwoord, colors = myColors, text = ~Antwoord,
        width = width, height = height
#            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
#                '<b>Antwoord</b>: %{text}')
      ) %>%
      layout(
        legend = list(title = list(text = "<b> Antwoord </b>")),
        barmode = "stack",
        yaxis = list(title = yVar,
          ticktext = as.list(yLabels), 
          tickvals = as.list((length(yLabels)-1):0),
          tickmode = "array"),
        xaxis = list(title = "Percentage") 
      )
    
    extraVars <- c("Antwoord")
    
  } else {
    
    myPlot <- plot_ly(data, x = as.character(data[[xVar]]), 
        y = data[[yVar]], type = 'bar', name = ~Type) %>%
      layout(
        legend = list(title = list(text = "<b> Type </b>")),
        yaxis = list(title = "Aantal"),
        xaxis = list(title = "Jaar"),
        barmode = "stack"
      )
    
    extraVars <- c("Type")
    
  } 
  
  
  return(list(plot = myPlot, data = data[, c(extraVars, xVar, yVar), with = FALSE]))
  
}



#' Shiny module for creating the plot \code{\link{barDraagkracht}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @inheritParams barDraagkracht
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
barDraagkrachtServer <- function(id, data, groupVariable = NULL, xVar = "percentage", yVar) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      callModule(module = optionsModuleServer, id = "barDraagkracht", 
        data = data
      )
      callModule(module = plotModuleServer, id = "barDraagkracht",
        plotFunction = "barDraagkracht", 
        data = data,
        groupVariable = groupVariable,
        xVar = xVar,
        yVar = yVar
      )
      
    })
  
}


#' Shiny module for creating the plot \code{\link{barDraagkracht}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
barDraagkrachtUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkDraagkracht"),
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkDraagkracht % 2 == 1", ns = ns,
      
      plotModuleUI(id = ns("barDraagkracht")),
      optionsModuleUI(id = ns("barDraagkracht"), exportData = TRUE,
        doWellPanel = FALSE),
      tags$hr()
    )
  )
  
}



