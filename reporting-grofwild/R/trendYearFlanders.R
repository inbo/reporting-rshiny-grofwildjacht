

#' Create interactive plot for counts in Flanders per selected years
#' @param data data.frame, formatted data for plotting as returned by \code{\link{createTrendData}}
#' @inheritParams trendYearRegion
#' @return list with plot and data
#' 
#' @author mvarewyck
#' @import plotly
#' @export
trendYearFlanders <- function(data, timeRange, jaartallen = NULL, sourceIndicator = NULL,
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
#' 
#' @importFrom dplyr coalesce
#' 
#' @return no returned value
#' @export
trendYearFlandersServer <- function(id, 
    geoData, allSpatialData, biotoopData, species,
    type = c("grofwild", "wildschade", "wbe", "empty", "dash"),
    includeOptions = TRUE, uiText,
    preSelected = reactive(NULL)){
  
  type <- match.arg(type)
  
  moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        results <- reactiveValues()
        
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
            
            period <- coalesce(input$period, preSelected()$time(), NA)
            
            req(!is.na(period))
            
            newChoices <- unique(geoData()$dataSource[
                geoData()$afschotjaar >= period[1] & 
                  geoData()$afschotjaar <= period[2]])
            isolate(previousChoice <- if (is.null(input$bronMap)) newChoices else input$bronMap)
            
            sourceChoices <- loadMetaSchade()$sources 
            
            selectInput(inputId = ns("bronMap"),
              label = "Databron(nen)",
              choices = sourceChoices[sourceChoices %in% newChoices], 
              selected = previousChoice[previousChoice %in% newChoices],
              multiple = TRUE)
            
          })
        
        
        
        timeDataFlanders <- reactive({
            
            period <- coalesce(input$period, preSelected()$time(), NA)
            bronMap <- coalesce(input$bronMap, preSelected()$dataSource_schade(), NA)
            
            validate(need(!is.na(period), "Gelieve periode te selecteren"))
           
            ## Get data for Flanders
            df <- createTrendData(
              data = geoData(),
              allSpatialData = allSpatialData,
              biotoopData = biotoopData$flanders,
              timeRange = period,
              species = species(),
              regionLevel = "flanders",
              unit = input$unit,
              sourceIndicator = if (all(is.na(bronMap))) NULL else bronMap
            )
            
            df
          })
        
        callModule(
            module = optionsModuleServer, id = "trendYearFlanders", 
            data = timeDataFlanders
        )
        
        callModule(
            module = plotModuleServer, id = "trendYearFlanders",
            plotFunction = "trendYearFlanders", 
            data = timeDataFlanders,
            timeRange = reactive(coalesce(input$period, preSelected()$time(), NA)),
            unit = reactive(input$unit),
            isSchade = (type == "wildschade"),
            height = "400px",
            preSelected = preSelected
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
    "Aantal/100ha bos & natuur" = "relativeDekking"),
  uiText, specie = NULL, doHide = TRUE
){
  
  type <- match.arg(type)
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = if (type == "wildschade") 
        "trendYearFlandersUI-schade" else 
        "trendYearFlandersUI", specie = specie, 
    uiText = uiText)
  
  
  tagList(
    actionLink(inputId = ns("linkTrendYearFlanders"), label = tags$h3(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkTrendYearFlanders % 2 ==", as.numeric(doHide)), 
      ns = ns,
      fixedRow(
        column(8, plotModuleUI(id = ns("trendYearFlanders"))),
        column(4,
          wellPanel(
            selectInput(
              inputId = ns("unit"), 
              label = "Eenheid",
              choices = unitChoices
            ),
            if(includeOptions)
              tagList(
                uiOutput(ns("period")),
                if (type == "wildschade")
                  uiOutput(ns("bronMap"))
              ),
            optionsModuleUI(id = ns("trendYearFlanders"), exportData = TRUE,
              doWellPanel = FALSE)
          )
        )
      )
    )
  )
}

