# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' User input for controlling specific plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param showLegend boolean, whether to show input field for the legend
#' @param showExtraVariables boolean, whether to show input field for 
#' extra variables (popup)
#' @param showTime boolean, whether to show input field for time range
#' @param showRegion boolean, whether to show input fields for region selection
#' @param showGlobe boolean, whether to show input field for map of the globe
#' @return ui object (tagList)
#' @export
optionsModuleUI <- function(id, 
    showLegend = FALSE, showExtraVariables = FALSE, showTime = FALSE, 
    showRegion = FALSE, showGlobe = FALSE) {
  
  ns <- NS(id)
  
  tagList(
      
      wellPanel(
          if (showLegend)
            selectInput(inputId = "legend", "Legende",
                choices = c("<none>" = "none", 
                    "Bovenaan rechts" = "topright", 
                    "Onderaan rechts" = "bottomright", 
                    "Bovenaan links" = "topleft",
                    "Onderaan links" = "bottomleft")
            ),
          if (showExtraVariables)
            uiOutput(ns("extraVariables")),
          if (showTime)
            uiOutput(ns("time")),
          if (showRegion)
            list(selectInput(inputId = "regionLevel", label = "Regio-schaal",
                    choices = c("Vlaanderen" = "flanders", "Provincie" = "provinces", "Gemeente" = "communes")),
                uiOutput(ns("region"))
            ),
          if (showGlobe)
            actionLink(inputId = "globe", label = "Voeg landkaart toe",
                icon = icon("globe"))
      
      )
  )
  
}



#' User input for controlling specific plot (server-side)
#' @param input shiny input variable for specific namespace
#' @param output shiny output variable for specific namespace
#' @param session shiny session variable for specific namespace
#' @param data reactive data.frame, data for chosen species
#' @return no return value; some output objects are created
#' @export
optionsModuleServer <- function(input, output, session, data) {
  
  ns <- session$ns
  
  output$extraVariables <- renderUI({
        
        selectInput(inputId = ns("extraVariables"), 
            label = "Extra variabelen in popup",
            choices = {
              vars <- names(data()); vars[!vars %in% c("counts", "year")]
            }, multiple = TRUE)
        
      })
  
  
  output$time <- renderUI({
        
        sliderInput(inputId = ns("time"), label = "Tijdstip(pen)", 
            value = c(min(data()$afschotjaar), max(data()$afschotjaar)),
            min = min(data()$afschotjaar),
            max = max(data()$afschotjaar),
            step = 1,
            sep = "")
        
      })
  
  
  output$region <- renderUI({
        
        selectInput(inputId = ns("region"), label = "Regio('s)",
#            choices = sort(data()$NAAM),
            choices = letters[1:4],
            multiple = TRUE)
        
      })
  
}



#' Interactive plot (ui-side)
#' @param id character, module id, unique name per plot
#' @return ui object
#' @author mvarewyck
#' @export
plotModuleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
      verbatimTextOutput(ns("print")),
      plotlyOutput(ns("plot"))
  )
}



#' Interactive plot (server-side)
#' @param input shiny input variable for specific namespace
#' @param output shiny output variable for specific namespace
#' @param session shiny session variable for specific namespace
#' @param plotFunction character, defines the plot function to be called
#' @param data reactive data.frame, data for chosen species
#' @param wildNaam character, defines the species name for y-label in plot
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @export
plotModuleServer <- function(input, output, session, plotFunction, data, wildNaam) {
  
  
  output$plot <- renderPlotly({
        
        argList <- c(
            list(data = data(), wildNaam = wildNaam),
            if (!is.null(input$legend))
              list(legend = input$legend), 
            if (!is.null(input$extraVariables))
              list(extraVariables = input$extraVariables),
            if (!is.null(input$time))
              list(jaartallen = input$time[1]:input$time[2]),
            if (!is.null(input$regionLevel))
              list(regionLevel = input$regionLevel,
                  region = input$region),
            if (!is.null(input$globe))
              list(globe = input$globe)
        )
        
        do.call(plotFunction, args = argList)
        
      })
  
}
