# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate stacked bar plot for kost landbouwschade (F09_2)
#' 
#' @param data data.frame with schadeData
#' @param unit character, variable in \code{data} to summarize on
#' @param typeMelding character vector, choices for filtering on `typeMelding` in data
#' @inheritParams barDraagkracht
#' @inheritParams reportingGrofwild-common-args
#' @return list with plotly object and data.frame 
#' plot per year (xaxis) and group (color): freq x schadeBedrag (yaxis)
#' @author wverlinden
#' @import plotly
#' @importFrom stats aggregate
#' @export 
barCost <- function(data, 
  unit = NULL, yVar = c("schadeBedrag", "count"), 
  typeMelding = NULL, regio = "") {
  
  wildNaam <- unique(data$wildsoort)
  
  yVar <- match.arg(yVar)  
  
  yLabel <- switch(yVar,
    schadeBedrag = "Bedrag (x 1000 EUR)",
    count = "Aantal"
  )
  groupLabel <- if (!is.null(unit))
      switch(unit,
        SoortNaam = "Gewas",
        season = "Seizoen",
        typeMelding = "Type schade"
      ) else 
      NULL
  
  
  subData <- data[, c(if (yVar != "count") yVar, unit, "afschotjaar")] 
  
  # Replace by group name
  if (unit == "SoortNaam") {
    fullNames <- loadMetaSchade()$gewassen
    newNames <- unlist(sapply(names(fullNames), function(x) rep(x, length(fullNames[[x]]))))
    subData[[unit]] <- newNames[match(subData[[unit]], unlist(fullNames))]
  }
  if (yVar == "schadeBedrag")
    subData[, yVar] <- subData[, yVar]/1000
  
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
  colors <- replicateColors(values = selectedGroups)$colors
  
  totalCount <- table(subData$afschotjaar)
  
  title <- paste0(
    yLabel, " kosten voor schadegevallen",
    if(!is.null(typeMelding)) paste(" over", toString(typeMelding)),
    paste(" van", tolower(wildNaam)),
    if(!is.null(groupLabel)) paste(" per", tolower(groupLabel)),
    if (!all(regio == "")) paste0("\n(", toString(regio), ")")
  )
  
  myPlot <- plot_ly(plotData, 
      x = as.character(plotData$afschotjaar), 
      y = ~x , type = 'bar', name = if (!is.null(unit)) ~base::get(unit),
      color = if (!is.null(unit)) ~as.factor(base::get(unit)) else "group", 
      colors = colors,
      hovertemplate = paste0(
        '<b>', yLabel, '</b>: ', if (yVar == "schadeBedrag") '%{y:.2f}' else '%{y:.0f}', '<br>',
        if (!is.null(groupLabel)) paste0('<b>', groupLabel, '</b>: %{text}'), '<extra></extra>'), 
      text = if (!is.null(unit)) ~base::get(unit),
      textposition = "none") %>%
    plotly::layout(
      title = title,
      margin = list(t = 100),
      legend = list(title = list(text = paste0("<b>", groupLabel, "</b>"))),
      yaxis = if (max(totalCount) < 5)
        list(title = yLabel, dtick = 1) else
        list(title = yLabel),
      xaxis = list(title = "Jaar"),
      barmode = "stack"
    )
  
  colnames(plotData)[colnames(plotData) == "x"] <- yLabel
  colnames(plotData)[colnames(plotData) == "afschotjaar"] <- "jaar"
  
  return(list(plot = myPlot, data = plotData[, c("jaar", unit, yLabel)]))
  
}




#' Shiny module for creating the plot \code{\link{barCost}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams barDraagkracht
#' @param title reactive, title to be printed above plot
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
      
      output$disclaimerBarCost <- renderUI({
          
          req(title())
          
          if (grepl("\\*", title()))
            getDisclaimerLimited()
          
        })      
      
      
      subData <- reactive({
          
          # Type melding
          plotData <- if (!is.null(input$typeMelding) && input$typeMelding != "all")
            data()[data()$typeMelding %in% input$typeMelding, ] else 
            data()
          
          # Bron
          filterDataSource(plotData = plotData,
            sourceIndicator = input$bron, returnStop = "message")
          
        })
      
      output$unitChoices <- renderUI({
          
          choices <- c("Seizoen" = "season", "Soortnaam" = "SoortNaam")
          if (input$typeMelding == "all") 
            choices <- c(choices[1], "Type schade" = "typeMelding") else if (input$typeMelding != "landbouw") 
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
        unit = reactive(input$unit),
        typeMelding = reactive(input$typeMelding)
      )
      
      
      return(reactive(c(
            toReturn(),
            isolate(reactiveValuesToList(input))
          )))
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{barCost}} - UI side
#' @inherit welcomeSectionUI
#' @inheritParams getOutputDescription
#' @inheritParams barCost
#' @inheritParams optionsModuleUI
#' @inheritParams reportingGrofwild-common-args
#' @export
barCostUI <- function(id, 
  uiText, context = strsplit(id, split = "_")[[1]][1], 
  specie = NULL,
  typeMelding = NULL, doHide = TRUE,
  regionLevels = NULL) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(
    output = "barCostUI", specie = specie, 
    uiText = uiText)
  description <- getOutputDescription(
    output = "barCostUI", 
    specie = specie, uiText = uiText, context = context
  )
  
  metaSchade <- loadMetaSchade()
  
  tagList(
    
    actionLink(inputId = ns("linkBarCost"), 
      label = title, class = "action-h3"),
    conditionalPanel(
      condition = 
        paste("input.linkBarCost % 2 ==", as.numeric(doHide)), 
      ns = ns,
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("barCost"))
        ),
          
        column(4,
          wellPanel(
            optionsModuleUI(
              id = ns("barCost"), 
              regionLevels = regionLevels, 
              doWellPanel = FALSE
            ),
            if (!is.null(typeMelding))
              selectInput(
                inputId = ns("typeMelding"), 
                label = "Type schade",
                choices = typeMelding
              ),
            uiOutput(ns("unitChoices")),
            selectInput(inputId = ns("bron"), label = "Databron(nen)",
              choices = metaSchade$sources,
              selected = metaSchade$sources,
              multiple = TRUE),
            optionsModuleUI(
              id = ns("barCost"), 
              exportData = TRUE, 
              doWellPanel = FALSE
            )
          )
        )
      ),
      uiOutput(ns("disclaimerBarCost")),
      tags$p(HTML(description))
    )    
  )
  
  
}