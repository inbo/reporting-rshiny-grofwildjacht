# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' User input for controlling specific plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param showLegend boolean, whether to show input field for the legend
#' @param showTime boolean, whether to show slider input field for time range
#' @param showYear boolean, whether to show numeric input field for year selection
#' @param showType, boolean, whether to select a select input field with type
#' @param regionLevels numeric vector, if not NULL, defines the choices for 
#' region levels: 1 = flanders, 2 = provinces, 3 = communes
#' @param summarizeBy character, choices to be shown as summary statistics
#' (expect count or percent)
#' @param exportData boolean, whether a download button for the data is shown
#' @param showDataSource boolean, whether to show choices of data source to be 
#' used for plotted bioindicator
#' @param doWellPanel boolean, whether to display the options within a 
#' \code{shiny::wellPanel()}
#' @return ui object (tagList)
#' @export
optionsModuleUI <- function(id, 
    showLegend = FALSE, showTime = FALSE, showYear = FALSE, showType = FALSE,
    regionLevels = NULL, summarizeBy = NULL,
    exportData = FALSE, showDataSource = FALSE,
    doWellPanel = TRUE) {
  
  ns <- NS(id)
  
  
  toReturn <- tagList(
      
      if (!is.null(summarizeBy))
        radioButtons(inputId = ns("summarizeBy"), label = "Rapporteer",
            choices = summarizeBy),
#          if (showLegend)
#            selectInput(inputId = ns("legend"), "Legende",
#                choices = c("<none>" = "none", 
#                    "Bovenaan rechts" = "topright", 
#                    "Onderaan rechts" = "bottomright", 
#                    "Bovenaan links" = "topleft",
#                    "Onderaan links" = "bottomleft")
#            ),
      if(showYear)
        uiOutput(ns("year")),
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
        ),
      if (showDataSource)
        selectInput(inputId = ns("sourceIndicator"), label = "Data bron voor onderkaaklengte",
            choices = c("INBO" = "inbo", "Meldingsformulier" = "meldingsformulier", 
                "INBO en meldingsformulier" = "both")),
      if(exportData)
        downloadButton(ns("dataDownload"), "Download data")
  
  )
  
  if (doWellPanel)
    wellPanel(toReturn) else
    toReturn
  
}



#' User input for controlling specific plot (server-side)
#' @param input shiny input variable for specific namespace
#' @param output shiny output variable for specific namespace
#' @param session shiny session variable for specific namespace
#' @param data reactive data.frame, data for chosen species
#' @param types, defines the species types that can be selected
#' @param labelTypes character, the displayed label for selecting options field
#' @param typesDefault, defines the default values for \code{types},
#' same as \code{types} by defualt
#' @param timeRange numeric vector of length 2 with time range (in year)
#' @param timeLabel character, label for the time slider, 'Periode' by default
#' @param multipleTypes boolean, whether multiple types can be selected or not
#' @return no return value; some output objects are created
#' @export
optionsModuleServer <- function(input, output, session, 
    data, types = NULL, labelTypes = "Type", typesDefault = types, 
    timeRange = NULL, timeLabel = "Periode", 
    multipleTypes = FALSE) {
  
  ns <- session$ns
  
  output$time <- renderUI({
        
        sliderInput(inputId = ns("time"), label = timeLabel, 
            value = timeRange(),
            min = if (!is.null(input$sourceIndicator)) {
                  if (input$sourceIndicator == "inbo") 2014 else min(timeRange())
                } else {min(timeRange())},
            max = max(timeRange()),
            step = 1,
            sep = "")
        
      })
  
  
  output$year <- renderUI({
        
        div(class = "sliderBlank", 
            sliderInput(inputId = ns("year"), label = "Geselecteerd Jaar", 
                value = max(timeRange()),
                min = min(timeRange()),
                max = max(timeRange()),
                step = 1,
                sep = "")
        )
        
        
      })
  
  
  output$region <- renderUI({
        
        validate(need(input$regionLevel, "Selecteer regio-schaal aub"))
        
        if (input$regionLevel == "flanders") {
          
          choices <- "Vlaams Gewest"
          
        } else if (input$regionLevel == "provinces") {
          
          choices <- levels(droplevels(factor(unique(data()$provincie), 
                      levels = c("West-Vlaanderen", "Oost-Vlaanderen", 
                          "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren")))) 
          
        } else {
          
          choices <- unique(data()$gemeente_afschot_locatie)
          choices <- choices[!is.na(choices)]
          choices <- choices[order(choices)]
          
        }
        
        
        if (input$regionLevel == "flanders")
          selected <- choices[1] else
          selected <- NULL
        
        selectInput(inputId = ns("region"), label = "Regio('s)",
            choices = choices, selected = selected, multiple = TRUE)
        
      })
  
  
  output$type <- renderUI({
        
        selectInput(inputId = ns("type"), label = labelTypes,
            choices = types(), 
            selected = typesDefault(), multiple = multipleTypes)
        
      })
  
}



#' Interactive plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param height character, plot height, default is "600px" 
#' @return ui object
#' @author mvarewyck
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#' @importFrom shiny NS
#' @export
plotModuleUI <- function(id, height = "600px") {
  
  ns <- NS(id)
  
  withSpinner(plotlyOutput(ns("plot"), height = height))
  
}


#' Interactive table (ui-side)
#' @param id character, module id, unique name per plot
#' @param includeTotal boolean, whether include text with total number of records in table
#' @return ui object
#' @author mvarewyck
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny tableOutput NS
#' @export
tableModuleUI <- function(id, includeTotal = FALSE) {
  
  ns <- NS(id)
  
  tagList(
      withSpinner(tableOutput(ns("table"))),
      if (includeTotal)
        uiOutput(ns("total"))
  )
  
}

#' Interactive table generated with datatable (ui-side)
#' @inheritParams tableModuleUI
#' @return ui object 
#' @author Eva Adriaensen
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny NS
#' @importFrom DT dataTableOutput
#' @export
datatableModuleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(withSpinner(DT::dataTableOutput(ns("table"))))
  
}


#' Interactive plot or table (server-side)
#' @param input shiny input variable for specific namespace
#' @param output shiny output variable for specific namespace
#' @param session shiny session variable for specific namespace
#' @param plotFunction character, defines the plot function to be called
#' @param data reactive data.frame, data for chosen species
#' @param openingstijdenData data with openingstijden, optional
#' @param toekenningsData data with toekenningen, optional
#' @param categorie character, defines which type of table should be made
#' @param locaties character, defines on which locations to filter on;
#' defined externally for large map
#' @param timeRange numeric vector, defines on which year range to filter on;
#' defined externally for large map
#' @param unit character, defines whether absolute or relative frequencies are reported;
#' defined externally for large map
#' @param schade boolean, indicates whether module is used for schadeData; default is FALSE
#' @param datatable boolean, indicates whether module should be used to output a datatable object for table; default is FALSE
#' @param schadeChoices character, chosen schade types (basisCode) to filter on, optional
#' @param schadeChoicesVrtg character, chosen schade types related to "VRTG" to filter on, optional
#' @param schadeChoicesGewas character, chosen schade types related to "GEWAS" to filter on, optional
#' @inheritParams plotBioindicator
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @importFrom utils write.table
#' @importFrom DT datatable formatRound renderDataTable
#' @export
plotModuleServer <- function(input, output, session, plotFunction, 
    data, openingstijdenData, toekenningsData = NULL,
    categorie = NULL, bioindicator = NULL,
    locaties = NULL, timeRange = NULL, unit = NULL, schade = FALSE, 
    datatable = FALSE, schadeChoices = NULL, schadeChoicesVrtg = NULL,
    schadeChoicesGewas = NULL, variable = NULL) {
  
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
  
  wildNaam <- reactive(unique(data()$wildsoort))
  
  
  subToekenningsData <- reactive({
        
        if (is.null(toekenningsData))
          return(NULL)
        
        Provincie <- NULL  # to prevent warnings with R CMD check
        Jaar <- NULL  # to prevent warnings with R CMD check
        subData <- toekenningsData()
        
        if (!is.null(input$regionLevel)) {
          
          validate(need(input$region, "Gelieve regio('s) te selecteren"))
          
          if (input$regionLevel == "provinces")
            subData <- subset(subData, Provincie %in% input$region)
          
        }
        
        if (!is.null(input$time))
          subData <- subset(subData, Jaar >= input$time[1] & Jaar <= input$time[2])
        
        
        return(subData)
        
      })
  
  
  argList <- reactive({
        
        req(nrow(subData()) > 0)
        
        argList <- c(
            list(data = subData()),
            if (!is.null(input$year))
              list(jaar = input$year),
            if (!is.null(input$time))
              list(jaartallen = input$time[1]:input$time[2]),
            # Currently these options are never used
#            if (!is.null(input$legend))
#              list(legend = input$legend), 
            if (!is.null(input$regionLevel))
              list(regio = input$region),
            if (!is.null(input$type))
              list(type = input$type),
            if (!is.null(input$type) & !is.null(input$year) & isFALSE(schade))
              list(openingstijdenData = openingstijdenData()),
            if (!is.null(subToekenningsData()))
              list(assignedData = subToekenningsData()),
            if (!is.null(categorie))
              list(categorie = categorie),
            if (!is.null(input$summarizeBy))
              list(summarizeBy = input$summarizeBy),
            if(!is.null(bioindicator))
              list(bioindicator = bioindicator),
            if(!is.null(input$sourceIndicator))
              list(sourceIndicator = input$sourceIndicator),
            if (!is.null(locaties))
              list(locaties = locaties()),
            if (!is.null(timeRange))
              list(timeRange = timeRange()),
            if (!is.null(unit))
              list(unit = unit()),
            if (!is.null(schadeChoices))
              list(schadeChoices = schadeChoices()),
            if (!is.null(schadeChoicesVrtg))
              list(schadeChoicesVrtg = schadeChoicesVrtg()),
            if (!is.null(schadeChoicesGewas))
              list(schadeChoicesGewas = schadeChoicesGewas()),
            if (!is.null(variable))
              list(variable = variable)
        
        )
        
        
      })
  
  resultFct <- reactive({
        
        toReturn <- tryCatch(
            do.call(plotFunction, args = argList()),
            error = function(err)
              validate(need(FALSE, err$message))
        )		
        
        validate(need(!is.null(toReturn), "Niet beschikbaar"))
        
        return(toReturn)
        
        
      })
  
  
  output$plot <- renderPlotly({  
        
        resultFct()$plot
        
      })
  
  
  output$dataDownload <- downloadHandler(
      filename = function() nameFile(species = wildNaam(),
            year = if (!is.null(input$year)) 
                  input$year else if (!is.null(input$time))
                  unique(c(input$time[1], input$time[2])) else
                  timeRange(), 
            content = paste0(plotFunction, "_data"), fileExt = "csv"),
      content = function(file) {
        
        resFct <- resultFct()
        
        ## checks
        
        # Note: a data.frame is a list!
        isDataPresent <- ifelse(!is.null(resFct),
            ifelse(is.data.frame(resFct), !is.null(resFct), !is.null(resFct$plot)),
            FALSE
        )
        
        validate(
            need(resFct, "Niet beschikbaar"),
            need(
                if(is.data.frame(resFct))	resFct	
                else if (is.data.frame(resFct$data)) resFct$data
                else	resFct$plot,
                "Niet beschikbaar"
            )
        )
        
        ## extract data to export
        dataPlot <- if(is.data.frame(resFct))	resFct	else	resFct$data
        
        ## write data to exported file
        write.table(x = dataPlot, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
        
      }
  )
  
  if (datatable == TRUE) {
    output$table <- DT::renderDataTable({
          
          DT::datatable(resultFct()$data, rownames = FALSE, container = resultFct()$header,
              selection = "single",
              options = list(dom = 't', pageLength = -1)) %>%
              formatRound(colnames(resultFct()$data), digits = 0)
          
        })
  } else {
    output$table <- renderTable({
          
          return(resultFct())
          
        }, digits = 0)
  }
  
  
}



#' Display formatted frequency table of data (ui-side)
#' @inheritParams plotModuleServer
#' @param data, character vector, values for which frequency table should be generated
#' @param variableLabel character, label of the variable that is summarized
#' @return ui object (tagList)
#' @export
dataModuleServer <- function(input, output, session, data, variable) {
  
  
  # TODO include in formatLabels()
  
  freqTable <- reactive({
        
        myTable <- as.data.frame(table(data()@data[, variable]), stringsAsFactors = FALSE)
        if (nrow(myTable) == 0)
          return(NULL)
        myTable <- myTable[rev(order(myTable$Freq)), ]
        
        if (nrow(myTable) == 0)
          return(NULL)
        
        variableLabel <- switch(variable,
            wildsoort = "Wildsoort",
            schadeBasisCode = "Type Schade",
            schadeCode = "Type Subschade",
            SoortNaam = "Gewas")
        
        colnames(myTable) <- c(variableLabel, "Aantal")
        if (!variable %in% c("wildsoort", "SoortNaam"))
          myTable[, variableLabel] <- names(fullNames(myTable[, variableLabel]))
        
        myTable
        
      })
  
  # Frequency table
  output$table <- renderTable({
        
        validate(need(freqTable(), "Geen data beschikbaar"))
        return(freqTable())
        
      }, digits = 0)
  
  # Total number of records
  output$total <- renderUI({
        
        req(freqTable())
        helpText("Totaal:", sum(freqTable()$Aantal))
      })    
  
}