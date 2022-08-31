# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate stacked bar plot for kost landbouwschade (F09_2)
#' 
#' @param data data.frame with schadeData
#' @param summarizeBy character, variable in \code{data} to summarize on
#' 
#' @return list with plotly object and data.frame 
#' plot per year (xaxis) and group (color): freq x schadeBedrag (yaxis)
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom stats aggregate
#' @export 
barCost <- function(data, summarizeBy = c("SoortNaam", "season")) {
  
  summarizeBy <- match.arg(summarizeBy)
  groupLabel <- switch(summarizeBy,
    SoortNaam = "Gewas",
    season = "Seizoen"
  )
  
  subData <- subset(data, schadeBasisCode == "GEWAS",
    select = c("schadeBedrag", summarizeBy, "afschotjaar"))
  
  summaryData <- count(df = subData, vars = names(subData))
  summaryData$schadeBedrag <- summaryData$schadeBedrag * summaryData$freq
  summaryData$freq <- NULL
  plotData <- aggregate(summaryData$schadeBedrag, by = summaryData[, c(summarizeBy, "afschotjaar")], 
    FUN = sum, na.rm = TRUE)
  plotData <- plotData[plotData$x != 0, ]
  
  
  myPlot <- plot_ly(plotData, 
      x = as.character(plotData$afschotjaar), 
      y = ~x , type = 'bar', name = ~get(summarizeBy),
      hovertemplate = paste0(
        '<b>Bedrag</b>: %{y:.2f}', '<br>',
        '<b>', groupLabel, '</b>: %{text}<extra></extra>'), 
      text = ~get(summarizeBy)) %>%
    layout(
      legend = list(title = list(text = paste0("<b>", groupLabel, "</b>"))),
      yaxis = list(title = "Bedrag"),
      xaxis = list(title = "Afschotjaar"),
      barmode = "stack"
    )
  
  colnames(plotData)[colnames(plotData) == "x"] <- "bedrag"
  
  return(list(plot = myPlot, data = plotData[, c("afschotjaar", summarizeBy, "bedrag")]))
  
}




#' Shiny module for creating the plot \code{\link{barCost}} - server side
#' @inheritParams countAgeGenderServer 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
barCostServer <- function(id, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "barCost", 
        data = data
      )
            
      callModule(module = plotModuleServer, id = "barCost",
        plotFunction = "barCost", 
        data = data)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{barCost}} - UI side
#' @template moduleUI
#' @inheritParams barDraagkrachtUI
#' 
#' @author mvarewyck
#' @export
barCostUI <- function(id, title) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkBarCost"), 
      label = h3(HTML(paste("FIGUUR:", title)))),
    conditionalPanel("input.linkBarCost % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("barCost"), 
            summarizeBy = c("Seizoen" = "season",
              "Soortnaam" = "SoortNaam"), exportData = TRUE)
        ),
        column(8, 
          plotModuleUI(id = ns("barCost"))
        ),
        tags$hr()
      )
    )    
  )
  
  
}