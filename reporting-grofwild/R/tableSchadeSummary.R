#' Shiny module for creating the schade summary tables - UI side
#' @inheritParams reportingGrofwild-common-args
#' @inheritParams getOutputTitle
#' @export
tableSchadeSummaryUI <- function(id, uiText, specie = NULL) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(
    output = "tableSchadeSummaryUI", specie = specie, uiText = uiText
  )
  
  tagList(
    
    h3(HTML(title)),
    
    wellPanel(
      uiOutput(ns("time")),
      uiOutput(ns("source_schade"))
      ),

    fixedRow(
      column(4, tableModuleUI(id = ns("wildsoort"), includeTotal = TRUE)),
      column(4, tableModuleUI(id = ns("schade"), includeTotal = TRUE)),
      conditionalPanel("input.schade_code.includes('GEWAS') || input.schade_code.includes('VRTG')", 
        column(4, tableModuleUI(id = ns("subschade"), includeTotal = TRUE))
      )
    )

  )
  
}

#' Shiny module for creating the schade summary tables- server side
#' @inheritParams dataModuleServer
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @import shiny
#' @export
tableSchadeSummaryServer <- function(id, data, schadeTypes, schadeCodes,
  definedYear = config::get("defaultYear", file = system.file("config.yml", package = "reportingGrofwild"))) {
  
  moduleServer(id,
    function(input, output, session) {
        
      ns <- session$ns
      
      current <- reactiveValues(
        time = NULL)
      
      output$time <- renderUI({
          req(data())
          
          value <- if (is.null(current$time)) {
              c(min(data()$afschotjaar), definedYear)
            } else {
              current$time
            }
          
          sliderInput(inputId = ns("time"), label = "Periode", 
            value = value,
            min = min(data()$afschotjaar),
            max = max(definedYear, max(data()$afschotjaar), na.rm = TRUE),
            step = 1,
            sep = "")
          
        })
      observe(current$time <- input$time)
      
      output$source_schade <- renderUI({
          
          sourcesSchade <- loadMetaSchade()$sources
          
          selectInput(inputId = ns("dataSource_schade"), 
            label = "Databron(nen)",
            choices = sourcesSchade, selected = if (is.null(current$sources_schade)) sourcesSchade else current$sources_schade,
            multiple = TRUE)
          
          
        })
      observe(current$sources_schade <- input$dataSource_schade)
      
      
      filteredData <- reactive({
          
          req(input$time)
          req(input$dataSource_schade)
          
          subData <- data()
          
          if (!is.null(input$time))
            subData <- subset(subData, afschotjaar >= input$time[1] & afschotjaar <= input$time[2])
          
          if (!is.null(input$dataSource_schade))
            subData <- filterDataSource(plotData = subData, sourceIndicator = input$dataSource_schade,
              returnStop = "message")
          
          return(subData)
          
        })
      
      # Create frequency tables for filtered data
        
      # wildsoort
      dataModuleServer(
        id = "wildsoort",
        data = filteredData, variable = "wildsoort"
      )
      
      # schade
      dataModuleServer(
        id = "schade",
        data = filteredData,
        variable = "schadeBasisCode", fullNames = schadeTypes
      )
      
      # subschade
      dataModuleServer(
        id = "subschade",
        data = filteredData, variable = "schadeCode", fullNames = schadeCodes
      )
      
  })
  
} 





