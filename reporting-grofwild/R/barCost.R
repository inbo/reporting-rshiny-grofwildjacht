# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate stacked bar plot for kost landbouwschade (F09_2)
#' 
#' @param data data.frame with schadeData
#' @param unit character, variable in \code{data} to summarize on
#' @inheritParams barDraagkracht
#' 
#' @return list with plotly object and data.frame 
#' plot per year (xaxis) and group (color): freq x schadeBedrag (yaxis)
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom stats aggregate
#' @export 
barCost <- function(data, unit = NULL, yVar = c("schadeBedrag", "count")) {
  
  
  yVar <- match.arg(yVar)  
  
  yLabel <- switch(yVar,
    schadeBedrag = "Bedrag",
    count = "Aantal"
  )
  groupLabel <- if (!is.null(unit))
      switch(unit,
        SoortNaam = "Gewas",
        season = "Seizoen"
      ) else 
      NULL
  
  
  subData <- data[, c(if (yVar != "count") yVar, unit, "afschotjaar")] 
  
  summaryData <- count(df = subData, vars = names(subData))
  if (yVar %in% names(summaryData))
    summaryData$yVar <- summaryData[, yVar] * summaryData$freq else
    summaryData$yVar <- summaryData$freq
  summaryData$freq <- NULL
  
  if (ncol(summaryData) > 2) {
    plotData <- aggregate(summaryData$yVar, by = summaryData[, c(unit, "afschotjaar")], 
      FUN = sum, na.rm = TRUE) 
  } else {
    plotData <- summaryData
    plotData$x <- plotData$yVar
  }
  plotData <- plotData[plotData$x != 0, ]
  
  selectedGroups <- if (is.null(unit)) "group" else unique(summaryData[[unit]])
  colors <- replicateColors(nColors = length(selectedGroups))$colors
  names(colors) <- selectedGroups
  
  myPlot <- plot_ly(plotData, 
      x = as.character(plotData$afschotjaar), 
      y = ~x , type = 'bar', name = if (!is.null(unit)) ~get(unit),
      color = if (!is.null(unit)) ~as.factor(get(unit)) else "group", 
      colors = colors,
      hovertemplate = paste0(
        '<b>', yLabel, '</b>: ', if (yVar == "schadeBedrag") '%{y:.2f}' else '%{y:.0f}', '<br>',
        if (!is.null(groupLabel)) paste0('<b>', groupLabel, '</b>: %{text}'), '<extra></extra>'), 
      text = if (!is.null(unit)) ~get(unit)) %>%
    layout(
      legend = list(title = list(text = paste0("<b>", groupLabel, "</b>"))),
      yaxis = list(title = yLabel),
      xaxis = list(title = "Jaar"),
      barmode = "stack"
    )
  
  colnames(plotData)[colnames(plotData) == "x"] <- yLabel
  colnames(plotData)[colnames(plotData) == "afschotjaar"] <- "jaar"
  
  return(list(plot = myPlot, data = plotData[, c("jaar", unit, yLabel)]))
  
}




#' Shiny module for creating the plot \code{\link{barCost}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams barDraagkrachtServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
barCostServer <- function(id, yVar, data, title = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observe({
          
          req(title())
          updateActionLink(session = session, inputId = "linkBarCost",
            label = paste("FIGUUR:", title()))
          
        })  
      
      subData <- reactive({
          
          if (!is.null(input$typeMelding))
            subset(data(), typeMelding %in% input$typeMelding) else 
            data()
          
        })
      
      output$unitChoices <- renderUI({
          
          choices <- c("Seizoen" = "season", "Soortnaam" = "SoortNaam")
          if (input$typeMelding != "landbouw") 
            choices <- choices[1]
          
          selectInput(inputId = ns("unit"), label = "Groep per",
            choices = choices)
        
        })
      
      
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "barCost", 
        data = subData
      )
      
      toReturn <- callModule(module = plotModuleServer, id = "barCost",
        plotFunction = "barCost", 
        data = subData,
        yVar = yVar,
        unit = reactive(input$unit)
      )
      
      return(reactive(toReturn()))
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{barCost}} - UI side
#' @template moduleUI
#' @param typeMelding character vector, choices for filtering on `typeMelding` in data
#' 
#' @author mvarewyck
#' @export
barCostUI <- function(id, uiText, typeMelding = NULL) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == paste(strsplit(id, "_")[[1]][-1], collapse = "_"), ]
  
  tagList(
    
    actionLink(inputId = ns("linkBarCost"), 
      label = paste("FIGUUR:", uiText$title), class = "action-h3"),
    conditionalPanel("input.linkBarCost % 2 == 0", ns = ns,
      
      fixedRow(
        
        column(4,
          wellPanel(
            if (!is.null(typeMelding))
              selectInput(inputId = ns("typeMelding"), label = "Type schade",
                choices = typeMelding),
            uiOutput(ns("unitChoices")),
            optionsModuleUI(id = ns("barCost"), exportData = TRUE, doWellPanel = FALSE)
          ),
          tags$p(HTML(uiText[, strsplit(id, split = "_")[[1]][1]]))
        ),
        column(8, 
          plotModuleUI(id = ns("barCost"))
        ),
        tags$hr()
      )
    )    
  )
  
  
}