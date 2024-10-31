

#' Create interactive plot for counts in Flanders per selected years
#' @param data data.frame, formatted data for plotting as returned by \code{\link{createTrendData}}
#' @inheritParams trendYearRegion
#' @return list with plot and data
#' 
#' @author mvarewyck
#' @import plotly
#' @export
trendYearFlanders <- function(data, timeRange, 
  unit = c("absolute", "relative", "relativeDekking"), isSchade = FALSE, 
		width = NULL, height = NULL) {
	
	
	trendYearRegion(data = data, timeRange = timeRange,
    unit = unit, isSchade = isSchade, isFlanders = TRUE,
    width = width, height = height)
  
  
}

#' Shiny module for creating the time plot Flanders - server side
#' @inheritParams mapFlandersServer
#' @return no returned value
#' @export
trendYearFlandersServer <- function(id, 
    geoData, allSpatialData, biotoopData, species,
    type = c("grofwild", "wildschade", "wbe", "empty", "dash"),
    includeOptions = TRUE){
  
  type <- match.arg(type)
  
  moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        if(includeOptions){
          maxYear <- max(geoData()$afschotjaar)
          minYear <- min(geoData()$afschotjaar)
          defaultYear <- config::get("defaultYear", 
            file = system.file("config.yml", package = "reportingGrofwild"))
          output$period <- renderUI(
              sliderInput(
                  inputId = ns("period"), 
                  label = "Periode", 
                  value = c(minYear, defaultYear),
                  min = minYear,
                  max = maxYear,
                  step = 1,
                  sep = ""
              )
          )
        }
        
        timeDataFlanders <- reactive({
              
              validate(need(input$period, "Gelieve periode te selecteren"))
              
              ## Get data for Flanders
              createTrendData(
                  data = geoData(),
                  allSpatialData = allSpatialData,
                  biotoopData = biotoopData$flanders,
                  timeRange = input$period,
                  species = species(),
                  regionLevel = "flanders",
                  unit = input$unit
              )
              
            })
        
        callModule(
            module = optionsModuleServer, id = "trendYearFlanders", 
            data = timeDataFlanders
        )
        
        callModule(
            module = plotModuleServer, id = "trendYearFlanders",
            plotFunction = "trendYearFlanders", 
            data = timeDataFlanders,
            timeRange = reactive(input$period),
            unit = reactive(input$unit),
            isSchade = (type == "wildschade")
        )
        
      })
  
}

#' Shiny module for creating the time plot Flanders - UI side
#' @inheritParams mapFlandersUI 
#' @params includeOptions logical, if TRUE (by default) the options
#' to filter the plot are included.
#' @return UI object
#' @export
trendYearFlandersUI <- function(id,
    type = c("grofwild", "wildschade", "wbe", "empty", "dash"),
    includeOptions = TRUE,
    unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative", 
        "Aantal/100ha bos & natuur" = "relativeDekking"),
    uiText, specie = NULL
){
  
  type <- match.arg(type)
  
  ns <- NS(id)
  
  title <- if(!is.null(uiText)){
        getOutputTitle(output = "timePlotFlandersUI", 
            specie = specie, uiText = uiText)
      }else{
        tagList("Evolutie", 
            if (type == "wildschade") "schadegevallen" else 
                  "gerapporteerd afschot", 
            tags$br(), "Vlaanderen"
        )
      }
  
  fluidRow(
    column(8, plotModuleUI(id = ns("trendYearFlanders"), height = "400px")),
    column(4,
      h3(title),
      if(includeOptions)
        tagList(
          uiOutput(ns("period")),
          selectInput(
              inputId = ns("unit"), 
              label = "Eenheid",
              choices = unitChoices
          )
      ),
      optionsModuleUI(id = ns("trendYearFlanders"), exportData = TRUE,
          doWellPanel = FALSE)
    )
  )
  
}

