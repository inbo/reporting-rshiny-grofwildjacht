# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate stacked bar plot for kost landbouwschade (F09_2)
#' 
#' @param data data.frame with schadeData
#' @param summarizeBy character, variable in \code{data} to summarize on
#' @inheritParams barDraagkracht
#' 
#' @return list with plotly object and data.frame 
#' plot per year (xaxis) and group (color): freq x schadeBedrag (yaxis)
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom stats aggregate
#' @export 
barCost <- function(data, summarizeBy = NULL, 
  yVar = c("schadeBedrag", "count")) {
  
  
  yVar <- match.arg(yVar)  
  
  yLabel <- switch(yVar,
    schadeBedrag = "Bedrag",
    count = "Aantal"
  )
  groupLabel <- if (!is.null(summarizeBy))
      switch(summarizeBy,
        SoortNaam = "Gewas",
        season = "Seizoen"
      ) else 
      NULL
  
  
  if (!is.null(summarizeBy))
    subData <- subset(data, schadeBasisCode == "GEWAS", 
      c(if (yVar != "count") yVar, summarizeBy, "afschotjaar")) else 
    subData <- subset(data, ,
      c(if (yVar != "count") yVar, "afschotjaar"))
  
  summaryData <- count(df = subData, vars = names(subData))
  if (yVar %in% names(summaryData))
    summaryData$yVar <- summaryData[, yVar] * summaryData$freq else
    summaryData$yVar <- summaryData$freq
  summaryData$freq <- NULL
  
  if (ncol(summaryData) > 2) {
    plotData <- aggregate(summaryData$yVar, by = summaryData[, c(summarizeBy, "afschotjaar")], 
      FUN = sum, na.rm = TRUE) 
  } else {
    plotData <- summaryData
    plotData$x <- plotData$yVar
  }
  plotData <- plotData[plotData$x != 0, ]
  
  
  myPlot <- plot_ly(plotData, 
      x = as.character(plotData$afschotjaar), 
      y = ~x , type = 'bar', name = if (!is.null(summarizeBy)) ~get(summarizeBy),
      hovertemplate = paste0(
        '<b>', yLabel, '</b>: ', if (yVar == "schadeBedrag") '%{y:.2f}' else '%{y:.0f}', '<br>',
        if (!is.null(groupLabel)) paste0('<b>', groupLabel, '</b>: %{text}'), '<extra></extra>'), 
      text = if (!is.null(summarizeBy)) ~get(summarizeBy)) %>%
    layout(
      legend = list(title = list(text = paste0("<b>", groupLabel, "</b>"))),
      yaxis = list(title = yLabel),
      xaxis = list(title = "Jaar"),
      barmode = "stack"
    )
  
  colnames(plotData)[colnames(plotData) == "x"] <- yLabel
  colnames(plotData)[colnames(plotData) == "afschotjaar"] <- "jaar"
  
  return(list(plot = myPlot, data = plotData[, c("jaar", summarizeBy, yLabel)]))
  
}




#' Shiny module for creating the plot \code{\link{barCost}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams barDraagkrachtServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
barCostServer <- function(id, yVar, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "barCost", 
        data = data
      )
      
      callModule(module = plotModuleServer, id = "barCost",
        plotFunction = "barCost", 
        data = data,
        yVar = yVar)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{barCost}} - UI side
#' @template moduleUI
#' @inheritParams barDraagkrachtUI
#' @param summarizeBy character vector, choices for the \code{summarizeBy} parameter
#' in \code{\link{barCost}}
#' 
#' @author mvarewyck
#' @export
barCostUI <- function(id, title, summarizeBy = NULL) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkBarCost"), 
      label = h3(HTML(paste("FIGUUR:", title)))),
    conditionalPanel("input.linkBarCost % 2 == 0", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("barCost"), 
            summarizeBy = summarizeBy, exportData = TRUE)
        ),
        column(8, 
          plotModuleUI(id = ns("barCost"))
        ),
        tags$hr()
      )
    )    
  )
  
  
}