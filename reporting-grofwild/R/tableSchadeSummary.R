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
tableSchadeSummaryServer <- function(id, data, schadeTypes, schadeCodes) {
  
  moduleServer(id,
    function(input, output, session) {
        
      # Create frequency tables for filtered data
        
      # wildsoort
      dataModuleServer(
        id = "wildsoort",
        data = data, variable = "wildsoort"
      )
      
      # schade
      dataModuleServer(
        id = "schade",
        data = data,
        variable = "schadeBasisCode", fullNames = schadeTypes
      )
      
      # subschade
      dataModuleServer(
        id = "subschade",
        data = data, variable = "schadeCode", fullNames = schadeCodes
      )
      
  })
  
} 





