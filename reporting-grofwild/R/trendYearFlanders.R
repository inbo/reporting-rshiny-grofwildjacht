

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
#' @inheritParams trendYearFlandersUI
#' @inheritParams createTrendData
#' @inheritParams reportingGrofwild-common-args
#' @return no returned value
#' @export
trendYearFlandersServer <- function(id, 
    geoData, allSpatialData, biotoopData, species,
    type = c("grofwild", "wildschade", "wbe", "empty", "dash"),
    includeOptions = TRUE, uiText){
  
  type <- match.arg(type)
  
  moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        results <- reactiveValues()
        
        output$title <- renderUI({
            
            req(!is.null(uiText))
            title <- getOutputTitle(output = if (type == "wildschade") 
                    "trendYearFlandersUI-schade" else 
                    "trendYearFlandersUI", 
                specie = species(), uiText = uiText)
            
            h3(title)
            
          })
        
        defaultYear <- config::get("defaultYear", 
          file = system.file("config.yml", package = "reportingGrofwild"))
        
        # freeze value period
        observe({
            
            req(nrow(geoData()) > 0)
            
            if (is.null(input$period)) {
              results$period <- c(min(geoData()$afschotjaar), defaultYear)
            } else {
              results$period <- input$period
            }
            
          })
        
         
        output$period <- renderUI({
            
            req(geoData())
            
            maxYear <- max(geoData()$afschotjaar)
            minYear <- min(geoData()$afschotjaar)
            
            sliderInput(
              inputId = ns("period"), 
              label = "Periode", 
              value = isolate(results$period),
              min = minYear,
              max = maxYear,
              step = 1,
              sep = ""
            )
          })
        
        # Not via updateSelectInput: this is slower, multiple rendering of the plot
        output$bronMap <- renderUI({
            
            req(input$period)
            
            newChoices <- unique(geoData()$dataSource[
                geoData()$afschotjaar >= input$period[1] & 
                  geoData()$afschotjaar <= input$period[2]])
            isolate(previousChoice <- if (is.null(input$bronMap)) newChoices else input$bronMap)
            
            sourceChoices <- loadMetaSchade()$sources 
            
            selectInput(inputId = ns("bronMap"),
              label = "Databron(nen)",
              choices = sourceChoices[sourceChoices %in% newChoices], 
              selected = previousChoice[previousChoice %in% newChoices],
              multiple = TRUE)
            
          })
        
        
        
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
                  unit = input$unit,
                  sourceIndicator = input$bronMap
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
            isSchade = (type == "wildschade"),
            height = "400px"
        )
        
      })
  
}

#' Shiny module for creating the time plot Flanders - UI side
#' @inheritParams mapFlandersUI 
#' @inheritParams getOutputTitle
#' @param includeOptions logical, if TRUE (by default) the options
#' to filter the plot are included.
#' @return UI object
#' @export
trendYearFlandersUI <- function(id,
    type = c("grofwild", "wildschade"),
    includeOptions = TRUE,
    unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative", 
        "Aantal/100ha bos & natuur" = "relativeDekking")
){
  
  type <- match.arg(type)
  
  ns <- NS(id)
  
 
  tagList(
    uiOutput(ns("title")),
    fluidRow(
      column(8, plotModuleUI(id = ns("trendYearFlanders"))),
      column(4,
        if(includeOptions)
          wellPanel(
            uiOutput(ns("period")),
            selectInput(
                inputId = ns("unit"), 
                label = "Eenheid",
                choices = unitChoices
            ),
            if (type == "wildschade")
              uiOutput(ns("bronMap"))
        ),
        optionsModuleUI(id = ns("trendYearFlanders"), exportData = TRUE,
            doWellPanel = FALSE)
      )
    )
  )
}

