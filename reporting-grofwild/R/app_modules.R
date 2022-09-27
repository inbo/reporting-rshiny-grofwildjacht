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
#' @param showDataSource character vector, for which variables to show choices 
#' of data source levels. 
#' Should be one or more of \code{c("schade", "leeftijd", "geslacht", "onderkaak", "embryos")}
#' @param doWellPanel boolean, whether to display the options within a 
#' \code{shiny::wellPanel()}
#' @param showCategorie boolean, if TRUE gives user option to select categorie
#' @param showInterval boolean, if TRUE gives user option to select interval
#' @return ui object (tagList)
#' @export
optionsModuleUI <- function(id, 
    showLegend = FALSE, showTime = FALSE, showYear = FALSE, showType = FALSE,
    showCategorie = FALSE, showInterval = FALSE, 
    regionLevels = NULL, summarizeBy = NULL,
    exportData = FALSE, 
    showDataSource = NULL,
    doWellPanel = TRUE
    ) {
  
  
  ns <- NS(id)
  
  
  toReturn <- tagList(
      
      if (!is.null(summarizeBy))
        radioButtons(inputId = ns("summarizeBy"), label = "Rapporteer",
            choices = summarizeBy),
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
                        "Fusiegemeenten" = "communes", "Faunabeheerzones" = "faunabeheerzones")[regionLevels])),
            column(8, uiOutput(ns("region")))
        ),
      if ("schade" %in% showDataSource)
        uiOutput(ns("dataSourceSchade")),
      if ("onderkaak" %in% showDataSource)
        uiOutput(ns("dataSourceOnderkaak")),
      if ("embryos" %in% showDataSource)
        list(
          uiOutput(ns("dataSourceEmbryos")),
          uiOutput(ns("dataSourceWarning"))
        ),
      if ("leeftijd" %in% showDataSource)
        uiOutput(ns("dataSourceLeeftijd")),
      if ("geslacht" %in% showDataSource)
        uiOutput(ns("dataSourceGeslacht")),
      
      if(showInterval)
        uiOutput(ns("interval")),
      if(showCategorie)
        uiOutput(ns("categorie")),
      if(exportData) {
        downloadButton(ns("dataDownload"), "Download data", class = "downloadButton")
      }
  
  
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
#' @param definedYear numeric, single numeric value specifying the year value 
#' (or max year value within a range) that is selected upon opening, default is
#' \code{defaultYear} which is globally defined as \code{currentYear - 1}
#' @param intervals character vector, defines the choices for interval
#' @param categories character vector, defines the choices for categorie 
#' @return no return value; some output objects are created
#' @export
optionsModuleServer <- function(input, output, session, 
    data, types = NULL, labelTypes = "Type", typesDefault = types, 
    timeRange = NULL, timeLabel = "Periode", 
    multipleTypes = FALSE, definedYear = defaultYear,
    categories = NULL, intervals = NULL) {
  
  ns <- session$ns
  
  results <- reactiveValues()
  
  sourcesSchade <- loadMetaSchade()$sources
  
  output$time <- renderUI({
        
        sliderInput(inputId = ns("time"), label = timeLabel, 
            value = c(min(timeRange()), definedYear),
            min = min(timeRange()),
            max = max(timeRange()),
            step = 1,
            sep = "")
        
      })
  
  
  
  observe({
      
      req(input$dataSource_leeftijd)
        
        results$time <- input$time
       
        updateSliderInput(session, inputId = "time", 
          value = results$time,
          min = {
            # TODO for indieningType??
            subData <- tryCatch(filterGrofwild(
                plotData = data(), 
                sourceIndicator_leeftijd = input$dataSource_leeftijd,
                sourceIndicator_geslacht = input$dataSource_geslacht,
                sourceIndicator_onderkaak = input$dataSource_onderkaak,
                sourceIndicator_embryos = input$dataSource_embryos
              ), error = function(err) validate(need(FALSE, err$message)))
            min = min(subData$afschotjaar)
          }
        
        )
        
        
      })
  
  
  output$year <- renderUI({
        
        div(class = "sliderBlank", 
            sliderInput(inputId = ns("year"), label = "Geselecteerd Jaar", 
                value = definedYear,
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
          
        } else if (input$regionLevel == "faunabeheerzones") {
          
          choices <- levels(droplevels(factor(unique(data()$FaunabeheerZone), 
                      levels = c(1:10))))
          
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
  
  
  ## this is applicable for 
  ## FIGUUR: Leeggewicht per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
  ## grofwild - they will have no effects on types and typesDefault in the other cases
  
  observe({
      
      if (!is.null(input$dataSource_leeftijd) && any(grepl("6m", types(), ignore.case = TRUE))) {
        if (input$dataSource_leeftijd == "both") {
          
          ## overrule types for Wild Zwijn in case selected source = "both" i.e. inbo en meldingsfomulier
          updateSelectInput(session, inputId = "type",
            choices = c("Frisling", "Overloper", "Volwassen", "Onbekend"),
            selected = c("Frisling", "Overloper", "Volwassen", "Onbekend"))
          
        } else {
          
          updateSelectInput(session, inputId = "type",
            choices = types(),
            selected = typesDefault())
        }
      }
      
    })
  
  output$type <- renderUI({
        
        selectInput(inputId = ns("type"), label = labelTypes,
            choices = types(), 
            selected = typesDefault(), multiple = multipleTypes)
        
      })
    
  output$categorie <- renderUI({
      
      selectInput(inputId = ns("categorie"), label = "Categorie",
        choices = categories())
      
    })  
  
  output$dataSourceSchade <- renderUI({
        
        selectInput(inputId = ns("dataSource_schade"), 
            label = "Data bron",
            choices = names(sourcesSchade),
            multiple = TRUE)
        
      })
    
    output$dataSourceLeeftijd <- renderUI({
        
        selectInput(inputId = ns("dataSource_leeftijd"), 
          label = "Data bron leeftijd", 
          choices = c("INBO" = "inbo", "INBO en meldingsformulier" = "both"),
          selected = "both"
        )
        
      })
    
    output$dataSourceGeslacht <- renderUI({
        
        selectInput(inputId = ns("dataSource_geslacht"), 
          label = "Data bron geslacht", 
          choices = c("INBO" = "inbo", "INBO en meldingsformulier" = "both"),
          selected = "both"
        )
        
      })
    
    output$dataSourceOnderkaak <- renderUI({
        
        selectInput(inputId = ns("dataSource_onderkaak"), 
          label = "Data bron voor onderkaaklengte",
          choices = c("INBO" = "inbo", 
            "Meldingsformulier" = "meldingsformulier",  
            "INBO en meldingsformulier" = "both"),
          selected = "both"
        )
        
      })
    
    output$dataSourceEmbryos <- renderUI({
        
        selectInput(inputId = ns("dataSource_embryos"), 
          label = "Data bron voor aantal embryo's", 
          choices = c("INBO" = "inbo", 
            "Meldingsformulier" = "meldingsformulier",  
            "INBO en meldingsformulier" = "both"),
          selected = "both"
        )
        
      })
  
  output$dataSourceWarning <- renderUI({
        
        req(input$dataSourceEmbryos)
        
        if (input$dataSourceEmbryos %in% c("both", "meldingsformulier"))
          tags$div(style = "margin-bottom:10px;",
              helpText("Observaties", HTML("v&#x00F3;&#x00F3;r"), "2014 afkomstig van het meldingsformulier met nul embryo's zijn niet opgenomen in de figuur.")
          )
        
        
      })
  
  output$interval <- renderUI({
        
        selectInput(inputId = ns("interval"), label = "Interval", choices = intervals)
        
      })
  
}



#' Interactive plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param height character, plot height, default is "600px" 
#' @param filter boolean, whether to display filters UI
#' @return ui object
#' @author mvarewyck
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#' @importFrom shiny NS
#' @export
plotModuleUI <- function(id, height = "600px", filter = FALSE) {
  
  ns <- NS(id)
  
  tagList(
      tags$div(align = "center",
          withSpinner(plotlyOutput(ns("plot"), height = height), hide.ui = FALSE)
      ),
      uiOutput(outputId = ns("warning"))
  )
}


#' Accuracy gauge - UI side
#' @inheritParams plotModuleUI
#' @param title character, title to be displayed above the gauge 
#' @return ui object
#' 
#' @author mvarewyck
#' @importFrom flexdashboard gaugeOutput
#' @export
accuracyModuleUI <- function(id, title) {
  
  ns <- NS(id)
  
  tagList(
    tags$div(style = "text-align:center", tags$h4(title)),
    flexdashboard::gaugeOutput(outputId = ns("accuracy"))
  )
  
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
  
  tags$div(style = "margin-bottom: 10px",
      withSpinner(DT::dataTableOutput(ns("table"))),
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
#' @param isSchade boolean, indicates whether module is used for schadeData; default is NULL
#' @param datatable boolean, indicates whether module should be used to output a datatable object for table; default is FALSE
#' @param schadeChoices character, chosen schade types (basisCode) to filter on, optional
#' @param schadeChoicesVrtg character, chosen schade types related to "VRTG" to filter on, optional
#' @param schadeChoicesGewas character, chosen schade types related to "GEWAS" to filter on, optional
#' @param variable character, defines which variable is of interest for the table
#' @param combinatie logical, summarised view of selected regions
#' @param schadeTitles boolean, whether title should include 'schade' instead of 'afschot'
#' @inheritParams plotBioindicator
#' @inheritParams trendYearRegion
#' @inheritParams createSpaceData
#' @inheritParams countYearShotAnimals
#' @param fullNames named character vector, values for the \code{variable} to be 
#' displayed instead of original data values
#' 
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @importFrom utils write.table
#' @importFrom DT datatable formatRound renderDataTable formatStyle styleEqual
#' @importFrom flexdashboard renderGauge gauge gaugeSectors
#' @export
plotModuleServer <- function(input, output, session, plotFunction, 
    data, openingstijdenData, toekenningsData = NULL,
    categorie = NULL, bioindicator = NULL, groupVariable = NULL,
    locaties = NULL, timeRange = NULL, unit = NULL, isSchade = NULL, 
    datatable = FALSE,  
    schadeChoices = NULL, schadeChoicesVrtg = NULL, schadeChoicesGewas = NULL, 
    variable = NULL, combinatie = NULL, schadeTitles = FALSE,
    fullNames = NULL) {
  
  subData <- reactive({
        
        provincie <- NULL  # to prevent warnings with R CMD check
        subData <- data()
        
        if (!is.null(input$regionLevel)) {
          
          validate(need(input$region, "Gelieve regio('s) te selecteren"))
          
          # filtering regions
          if (input$regionLevel == "provinces") {
            subData <- subset(subData, provincie %in% input$region)
          } else if (input$regionLevel == "faunabeheerzones") {   
            subData <- subData[subData$FaunabeheerZone %in% as.numeric(input$region), ]
          }
        }
        
        
        return(subData)
        
      })
  
  wildNaam <- reactive(unique(data()$wildsoort))
  
  
  subToekenningsData <- reactive({
        
        if (is.null(toekenningsData))
          return(NULL)
        
        provincie_toek <- NULL  # to prevent warnings with R CMD check
        labeljaar <- NULL  # to prevent warnings with R CMD check
        subData <- toekenningsData()
        
        if (!is.null(input$regionLevel)) {
          
          validate(need(input$region, "Gelieve regio('s) te selecteren"))
          
          if (input$regionLevel == "provinces")
            subData <- subset(subData, provincie_toek %in% input$region)
          
        }
        
        if (!is.null(input$time))
          subData <- subset(subData, labeljaar >= input$time[1] & labeljaar <= input$time[2])
        
        
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
            if (!is.null(input$regionLevel))
              list(regio = input$region),
            if (!is.null(input$type))
              list(type = input$type),
            if (!is.null(input$type) & !is.null(input$year) & is.null(input$dataSource_schade))
              list(openingstijdenData = openingstijdenData()),
            if (!is.null(subToekenningsData()))
              list(assignedData = subToekenningsData()),
            if (!is.null(categorie))          
              list(categorie = categorie) else if (!is.null(input$categorie))          
              list(categorie = input$categorie),
            if (!is.null(input$summarizeBy))
              list(summarizeBy = input$summarizeBy),
            if(!is.null(bioindicator))
              list(bioindicator = bioindicator),
            if(!is.null(groupVariable))
              list(groupVariable = groupVariable),
            if(!is.null(fullNames))
              list(fullNames = fullNames),
            
            if (!is.null(isSchade))
              list(isSchade = isSchade),
            
            # Sources
            if(!is.null(input$dataSource_schade))
              list(sourceIndicator = input$dataSource_schade),            
            if(!is.null(input$dataSource_leeftijd))
              list(sourceIndicator_leeftijd = input$dataSource_leeftijd),
            if(!is.null(input$dataSource_geslacht))
              list(sourceIndicator_geslacht = input$dataSource_geslacht),
            if(!is.null(input$dataSource_onderkaak))
              list(sourceIndicator = input$dataSource_onderkaak),
            if(!is.null(input$dataSource_embryos))
              list(sourceIndicator = input$dataSource_embryos),
            
            if (!is.null(locaties))
              list(locaties = locaties()),
            if (!is.null(timeRange))
              list(timeRange = timeRange()),
            if (!is.null(unit))
              list(unit = unit()),
            if (isTRUE(schadeTitles))
              list(schadeTitles = schadeTitles),
            if (!is.null(schadeChoices))
              list(schadeChoices = schadeChoices()),
            if (!is.null(schadeChoicesVrtg))
              list(schadeChoicesVrtg = schadeChoicesVrtg()),
            if (!is.null(schadeChoicesGewas))
              list(schadeChoicesGewas = schadeChoicesGewas()),
            if (!is.null(variable))
              list(variable = variable),
            if (plotFunction == "trendYearRegion") {
              if (!is.null(combinatie()))
                list(combinatie = combinatie())
            },
            if (!is.null(input$interval))
              list(interval = input$interval)
        
        )
        
        
      })
  
  resultFct <- reactive({
        
        tryCatch({
            tmpResult <- do.call(plotFunction, args = argList())
            validate(need(!is.null(tmpResult), "Niet beschikbaar"))
            tmpResult
          },
            error = function(err) {
              validate(need(FALSE, err$message))
            }
        )		
        
      })
  
  
  output$plot <- renderPlotly({  
        
        resultFct()$plot
        
      })
    
  # Prevent that plotly images are squeezed
  outputOptions(output, "plot", suspendWhenHidden = FALSE)
    
  output$accuracy <- flexdashboard::renderGauge({
      
        flexdashboard::gauge(
          value = resultFct()$accuracy$value,
          min = 0, max = 100, 
          symbol = "%",
          # this label is not updated when value changes
#          label = paste0("(totaal ", resultFct()$accuracy$total, ")"),
          sectors = flexdashboard::gaugeSectors(
            success = c(50, 100), 
            warning = c(30, 50),
            danger = c(0, 30)),
        )
        
      })
  
  
  output$warning <- renderUI({
        
      tags$em(resultFct()$warning)
        
      })
  
  
  output$dataDownload <- downloadHandler(
      filename = function() nameFile(species = wildNaam(),
            year = if (!is.null(input$year)) 
                  input$year else if (!is.null(input$time))
                  unique(c(input$time[1], input$time[2])) else if (!is.null(timeRange))
                  timeRange() else
                  unique(data()$year), 
            extraInfo = input$type,
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
  
  
  output$table <- DT::renderDataTable({
      
      if (datatable) {
        
        DT::datatable(resultFct()$data, rownames = FALSE, container = resultFct()$header,
            selection = "single",
            options = list(dom = 't', pageLength = -1)) %>%
          formatRound(colnames(resultFct()$data)[-1], digits = 0, mark = "") %>%
          formatStyle(
            colnames(resultFct()$data)[1],
            target = "row",
            fontWeight = styleEqual(tail(resultFct()$data[, 1], n = 1), "bold")
          )
        
      } else {
        
        DT::datatable(resultFct(), rownames = FALSE,
            options = list(dom = 't', pageLength = -1,
              columnDefs = list(list(targets = grep("Warning", colnames(resultFct())) - 1, visible = FALSE)))) %>%
          formatStyle(
            colnames(resultFct())[1],
            target = "row",
            fontWeight = styleEqual(tail(resultFct()[, 1], n = 1), "bold")
          ) %>%
          formatStyle(
            grep("Verandering", colnames(resultFct()), value = TRUE),
            grep("Warning", colnames(resultFct()), value = TRUE),
            color = styleEqual(c("oranje", "rood"), c("orange", "red"))
          )
        
      }
    })
  
  
  return(reactive(resultFct()))
  
}



#' Display formatted frequency table of data (ui-side)
#' @inheritParams plotModuleServer
#' @param data, character vector, values for which frequency table should be generated
#' @param variable character, name of the variable that is summarized
#' @param fullNames named character vector, values for the \code{variable} to be 
#' displayed instead of original data values
#' @return ui object (tagList)
#' @export
dataModuleServer <- function(input, output, session, data, variable, fullNames = NULL) {
  
  
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
        if (!is.null(fullNames))
          myTable[, variableLabel] <- names(fullNames)[match(myTable[, variableLabel], fullNames)]
        
        myTable
        
      })
  
  # Frequency table
  output$table <- DT::renderDataTable({
        
        validate(need(freqTable(), "Geen data beschikbaar"))
        DT::datatable(freqTable(), rownames = FALSE,
            options = list(dom = 't', pageLength = -1))
        
      })
  
  # Total number of records
  output$total <- renderUI({
        
        req(freqTable())
        helpText("Totaal:", sum(freqTable()$Aantal))
      })    
  
}