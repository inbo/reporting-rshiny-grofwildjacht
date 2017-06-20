# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' User input for controlling specific plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param showLegend boolean, whether to show input field for the legend
#' @param showExtraVariables boolean, whether to show input field for 
#' extra variables (popup)
#' @param showTime boolean, whether to show slider input field for time range
#' @param showYear boolean, whether to show numeric input field for year selection
#' @param showType, boolean, whether to select a select input field with type
#' @param regionLevels numeric vector, if not NULL, defines the choices for 
#' region levels: 1 = flanders, 2 = provinces, 3 = communes
#' @param showGlobe boolean, whether to show input field for map of the globe
#' @param showSummarizeBy boolean, whether to show input field to choose between
#' aantal of percentage
#' @return ui object (tagList)
#' @export
optionsModuleUI <- function(id, 
    showLegend = FALSE, showExtraVariables = FALSE, 
    showTime = FALSE, showYear = FALSE, showType = FALSE,
    regionLevels = NULL, showGlobe = FALSE, showSummarizeBy = FALSE) {
  
  ns <- NS(id)
  
  
  tagList(
      
      wellPanel(
          if (showSummarizeBy)
            radioButtons(inputId = ns("summarizeBy"), label = "Rapporteer",
                choices = c("Aantal (alle data)" = "count", 
                    "Percentage (subset van ingezamelde onderkaken)" = "percent")),
#          if (showLegend)
#            selectInput(inputId = ns("legend"), "Legende",
#                choices = c("<none>" = "none", 
#                    "Bovenaan rechts" = "topright", 
#                    "Onderaan rechts" = "bottomright", 
#                    "Bovenaan links" = "topleft",
#                    "Onderaan links" = "bottomleft")
#            ),
#          if (showExtraVariables)
#            uiOutput(ns("extraVariables")),
          if(showYear)
            uiOutput(ns("year")),
          if (showYear & showTime)
            tags$b("Referentieperiode"),
          if (showTime)
            uiOutput(ns("time")),
          if(showType)
            uiOutput(ns("type")),
          if (!is.null(regionLevels))
            fluidRow(
                column(4, selectInput(inputId = ns("regionLevel"), label = "Regio-schaal",
                        choices = c("Vlaanderen" = "flanders", "Provincie" = "provinces", 
                            "Fusiegemeenten" = "communes")[regionLevels])),
                column(8, uiOutput(ns("region")))
            )
#          if (showGlobe)
#            actionLink(inputId = ns("globe"), label = "Voeg landkaart toe",
#                icon = icon("globe")),
              
      )
  )
  
}



#' User input for controlling specific plot (server-side)
#' @param input shiny input variable for specific namespace
#' @param output shiny output variable for specific namespace
#' @param session shiny session variable for specific namespace
#' @param data reactive data.frame, data for chosen species
#' @param types, defines the species types that can be selected
#' @param extraVariables character vector, defines the variables in \code{data}
#' for which values should be shown in popup-window in the plot
#' @param timeLabel label for the time slider, 'Tijdstip(pen)' by default
#' @return no return value; some output objects are created
#' @export
optionsModuleServer <- function(input, output, session, 
    data, types = NULL, extraVariables = NULL, timeLabel = "Tijdstip(pen)") {
  
  ns <- session$ns
  
  output$extraVariables <- renderUI({
        
        selectInput(inputId = ns("extraVariables"), 
            label = "Extra variabelen in popup",
            choices = extraVariables,
            multiple = TRUE)
        
      })
  
  
  output$time <- renderUI({
        
        sliderInput(inputId = ns("time"), label = timeLabel, 
            value = c(min(data()$afschotjaar), max(data()$afschotjaar)),
            min = min(data()$afschotjaar),
            max = max(data()$afschotjaar),
            step = 1,
            sep = "")
        
      })
  
  output$year <- renderUI({
        
        sliderInput(inputId = ns("year"), label = "Geselecteerd Jaar", 
            value = max(data()$afschotjaar),
            min = min(data()$afschotjaar),
            max = max(data()$afschotjaar),
            step = 1,
            sep = "")
        
        
      })
  
  
  output$region <- renderUI({
        
        validate(need(input$regionLevel, "Selecteer regio-schaal aub"))
        
        if (input$regionLevel == "flanders") 
          choices <- "Vlaams Gewest" else if (input$regionLevel == "provinces")
          choices <- unique(data()$provincie) else
          choices <- unique(data()$gemeente_afschot_locatie)
        
        selectInput(inputId = ns("region"), label = "Regio('s)",
            choices = choices[!is.na(choices)],
            multiple = TRUE)
        
      })
  
  output$type <- renderUI({
        
        selectInput(inputId = ns("type"), label = "Type",
            choices = types(), multiple = FALSE)
        
      })
  
}



#' Interactive plot (ui-side)
#' @param id character, module id, unique name per plot
#' @return ui object
#' @author mvarewyck
#' @export
plotModuleUI <- function(id) {
  
  ns <- NS(id)
  
  plotlyOutput(ns("plot"), height = "600px")
  
}


#' Interactive table (ui-side)
#' @param id character, module id, unique name per plot
#' @return ui object
#' @author mvarewyck
#' @export
tableModuleUI <- function(id) {
  
  ns <- NS(id)
  
  tableOutput(ns("table"))
  
}


#' Interactive plot or table (server-side)
#' @param input shiny input variable for specific namespace
#' @param output shiny output variable for specific namespace
#' @param session shiny session variable for specific namespace
#' @param plotFunction character, defines the plot function to be called
#' @param data reactive data.frame, data for chosen species
#' @param openingstijdenData data with openingstijden, optional
#' @param toekenningsData data with toekenningen, optional
#' @param wildNaam character, defines the species name for y-label in plot
#' @param categorie character, defines which type of table should be made
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @export
plotModuleServer <- function(input, output, session, plotFunction, 
    data, openingstijdenData, toekenningsData = NULL, wildNaam, categorie = NULL) {
  
  subData <- reactive({
        
        provincie <- NULL  # to prevent warnings with R CMD check
        subData <- data()
        
        if (!is.null(input$regionLevel)) {
          
          validate(need(input$region, "Gelieve regio('s) te selecteren"))
          
          if (input$regionLevel == "provinces")
            subData <- subset(subData, provincie %in% input$region)
          
        }
        
        
        return(subData)
        
      })
  
  
  argList <- reactive({
        
        req(nrow(subData()) > 0)
        
        argList <- c(
            list(data = subData(), wildNaam = wildNaam()),
            if (!is.null(input$year))
              list(jaar = input$year),
            if (!is.null(input$time))
              list(jaartallen = input$time[1]:input$time[2]),
            # Currently these options are never used
#            if (!is.null(input$legend))
#              list(legend = input$legend), 
#            if (!is.null(input$extraVariables))
#              list(extraVariables = input$extraVariables),
            if (!is.null(input$regionLevel))
              list(regio = input$region),
            if (!is.null(input$type))
              list(type = input$type),
            if (!is.null(input$type) & !is.null(input$year))
              list(openingstijden = unlist(
                      openingstijdenData()[
                          openingstijdenData()$Soort == wildNaam() &
                              openingstijdenData()$Type == input$type &
                              openingstijdenData()$Jaar == input$year, 
                          c("Startdatum", "Stopdatum")
                      ])
              ),
            if (!is.null(toekenningsData))
              list(assignedData = toekenningsData()),
            if (!is.null(categorie))
              list(categorie = categorie),
            if (!is.null(input$summarizeBy))
              list(summarizeBy = input$summarizeBy)        
#            if (!is.null(input$globe))
#              list(globe = input$globe)
        )
        
        
      })

  
  output$plot <- renderPlotly({       
        
        do.call(plotFunction, args = argList())
        
      })
  
  
  output$table <- renderTable({
        
        toReturn <- do.call(plotFunction, args = argList())
        validate(need(!is.null(toReturn), "Niet beschikbaar"))
        
        
        return(toReturn)
        
      }, digits = 0)

}

