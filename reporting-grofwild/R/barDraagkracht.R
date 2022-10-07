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
    groupLevels <- sapply(groupVariable, function(x) unique(data[[x]]), simplify = FALSE)
    
    plotList <- list()
    
    secondGroup <- if (length(groupVariable) > 1)
        groupLevels[[2]] else
        ""
    
    for (jVar in secondGroup) 
      for (iVar in groupLevels[[1]]) {
        
        subData <- if (jVar == "")
            data[data[[groupVariable[1]]] %in% iVar, ] else
            data[data[[groupVariable[1]]] %in% iVar & data[[groupVariable[2]]] %in% jVar, ]
        
        plotList[[(length(plotList) + 1)]] <- plot_ly(
            data = subData, 
            x = ~get(xVar), 
            y = ~get(yVar), 
            type = 'bar', name = ~Antwoord, color = ~Antwoord, colors = myColors, 
            legendgroup = ~Antwoord, 
            showlegend = (iVar == groupLevels[[1]] && jVar == secondGroup[1]), 
            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
              '<b>Antwoord</b>: %{text}'),
            text = ~Antwoord
          ) %>%
          layout(
            annotations = list(
              # columns
              list(x = 50, y = 1.05, 
                text = if (jVar == secondGroup[1]) as.character(iVar) else "", 
                showarrow = FALSE, font = list(size = 16), 
                yref = 'paper'),
              # rows
              list(x = 1.05, y = 0.5, 
                text = if (iVar == tail(groupLevels[[1]], n=1)) as.character(jVar) else "", 
                showarrow = FALSE, font = list(size = 16), textangle = 90,
                xref = 'paper', yref = 'paper')
            ),
            legend = list(title = list(text = "<b>Antwoord</b>")),
            barmode = "stack",
            yaxis = list(title = ""),
            xaxis = list(title = "Percentage") 
          ) 
        
      }
    
    myPlot <- do.call(subplot, c(plotList, shareY = TRUE, shareX = TRUE, nrows = length(secondGroup)))
    
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


#' Shiny module for creating the plot \code{\link{barDraagkracht}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @inheritParams barDraagkracht
#' @param title reactive object, title to be displayed in the plot
#' 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
barDraagkrachtServer <- function(id, data, groupVariable = NULL, xVar = "percentage", yVar,
  title = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subData <- reactive({
          
          if (!is.null(input$subGroup)) {
            
            switch(input$subGroup,
              "stakeholders" = data()[data()[[groupVariable[1]]] %in% c('Jagers', 'Landbouwers', 'Natuurvereniging'), ],
              "public" = data()[data()[[groupVariable[1]]] %in% c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied'), ]
            )
            
          } else {
            
            data()
            
          } 
          
        })
      
      observeEvent(title(), {
          
          updateActionLink(session = session, inputId = "linkDraagkracht",
            label = paste("FIGUUR:", title()))
          
        })
      
      callModule(module = optionsModuleServer, id = "barDraagkracht", 
        data = subData
      )
      callModule(module = plotModuleServer, id = "barDraagkracht",
        plotFunction = "barDraagkracht", 
        data = subData,
        groupVariable = groupVariable,
        xVar = xVar,
        yVar = yVar
      )
      
    })
  
}


#' Shiny module for creating the plot \code{\link{barDraagkracht}} - UI side
#' @template moduleUI
#' @param title character, title for the plot
#' 
#' @author mvarewyck
#' @export
barDraagkrachtUI <- function(id, title, subGroups = NULL) {
  
  ns <- NS(id)
  
  tagList(
    
    actionLink(inputId = ns("linkDraagkracht"),
      label = h3(HTML(paste("FIGUUR:", title)))),
    conditionalPanel("input.linkDraagkracht % 2 == 1", ns = ns,
      
      if (!is.null(subGroups))
        wellPanel(
          radioButtons(inputId = ns("subGroup"), label = NULL, choices = subGroups)
        ),
      plotModuleUI(id = ns("barDraagkracht")),
      optionsModuleUI(id = ns("barDraagkracht"), exportData = TRUE,
        doWellPanel = FALSE),
      tags$hr()
    )
  )
  
}