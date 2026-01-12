#' Shiny module for creating the schade summary tables - UI side
#' @inheritParams reportingGrofwild-common-args
#' @inheritParams getOutputTitle
#' @export
tableSchadeSummaryUI <- function(id, uiText, specie = NULL, doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(
    output = "tableSchadeSummaryUI", specie = specie, uiText = uiText
  )
  
  tagList(
    
    actionLink(inputId = ns("linkTableSummary"), label = tags$h3(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkTableSummary % 2 ==", as.numeric(doHide)),
      ns = ns,
      fixedRow(
        column(4, tableModuleUI(id = ns("wildsoort"), includeTotal = TRUE)),
        column(4, tableModuleUI(id = ns("schade"), includeTotal = TRUE)),
        column(4, tableModuleUI(id = ns("subschade"), includeTotal = TRUE))
      )
    
    )
  )
  
}

#' Shiny module for creating the schade summary tables- server side
#' @inheritParams dataModuleServer
#' @inheritParams reportingGrofwild-common-args
#' @param definedYear numeric, single numeric value specifying the year value 
#' 
#' @return no return value
#' @import shiny
#' @export
tableSchadeSummaryServer <- function(id, data, schadeTypes, schadeCodes,
  definedYear = config::get("defaultYear", file = system.file("config.yml", package = "reportingGrofwild")),
  preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
        
      # For R CMD check
      afschotjaar <- NULL
      
      ns <- session$ns
      
      current <- reactiveValues(
        time = NULL)
      
      
      filteredData <- reactive({
          
          req(preSelected())
          
          subData <- data()
          
          if (!is.null(preSelected()$time()))
            subData <- subset(subData, afschotjaar >= preSelected()$time()[1] & afschotjaar <= preSelected()$time()[2])
          
          if (!is.null(preSelected()$dataSource_schade()))
            subData <- filterDataSource(plotData = subData, sourceIndicator = preSelected()$dataSource_schade(),
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
      
      return(reactive({
            # Update when any of these change
            filteredData()
            # Return the static values
            c(
              data = reactive(filteredData())
            )
          }))
      
  })
  
} 





