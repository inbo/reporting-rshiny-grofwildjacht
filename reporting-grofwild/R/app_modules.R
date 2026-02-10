# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' User input for controlling specific plot (ui-side)
#' @param showLegend boolean, whether to show input field for the legend
#' @param showTime boolean, whether to show slider input field for time range
#' @param showYear boolean, whether to show numeric input field for year selection
#' @param showType, boolean, whether to select a select input field with type
#' @param regionLevels numeric vector, if not NULL, defines the choices for 
#' region levels: 1 = flanders, 2 = provinces, 3 = communes, 4 = faunabeheerzones
#' @param summarizeBy character, choices to be shown as summary statistics
#' (expect count or percent)
#' @param exportPlot boolean, whether a download button for the plot is shown
#' @param exportData boolean, whether a download button for the data is shown
#' @param showDataSource character vector, for which variables to show choices 
#' of data source levels. 
#' Should be one or more of \code{c("schade", "leeftijd", "geslacht", "onderkaak", "embryos")}
#' @param doWellPanel boolean, whether to display the options within a 
#' \code{shiny::wellPanel()}
#' @param showCategorie boolean, if TRUE gives user option to select categorie
#' @param showInterval boolean, if TRUE gives user option to select interval
#' @param oneRow boolean, if TRUE (FALSE by default) all the
#' options are combined in one row.
#' @inheritParams reportingGrofwild-common-args
#' @return ui object (tagList)
#' @importFrom shinyjs hidden
#' @export
optionsModuleUI <- function(id, 
    showLegend = FALSE, showTime = FALSE, showYear = FALSE, showType = FALSE,
    showCategorie = FALSE, showInterval = FALSE, 
    regionLevels = NULL, summarizeBy = NULL,
    exportPlot = FALSE, regionLevelSelected = NULL, exportData = FALSE, 
    showDataSource = NULL,
    doWellPanel = TRUE, oneRow = FALSE
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
          column(12, selectInput(inputId = ns("regionLevel"), label = "Regio-schaal",
              choices = c(
                "Vlaanderen" = "flanders", 
                "Provincie" = "provinces", 
                "Fusiegemeenten" = "communes", 
                "Faunabeheerzones" = "faunabeheerzones")[regionLevels],
              selected = regionLevelSelected)),
            column(11, offset = 1, uiOutput(ns("region")))
        ),
      if ("schade" %in% showDataSource)
        uiOutput(ns("source_schade")),
      if ("onderkaak" %in% showDataSource)
        selectInput(inputId = ns("dataSource_onderkaak"), 
          label = "Databron(nen) voor onderkaaklengte",
          choices = c("INBO" = "inbo", 
            "Meldingsformulier" = "meldingsformulier",  
            "INBO en meldingsformulier" = "both"),
          selected = "both"
        ),
      if ("embryos" %in% showDataSource)
        list(
          selectInput(inputId = ns("dataSource_embryos"), 
            label = "Databron(nen) voor aantal embryo's", 
            choices = c("INBO" = "inbo", 
              "Meldingsformulier" = "meldingsformulier",  
              "INBO en meldingsformulier" = "both"),
            selected = "both"
          ),
          shinyjs::hidden(tags$div(id = "dataSource_warning", style = "margin-bottom:10px;",
              helpText("Observaties", HTML("v&#x00F3;&#x00F3;r"), "2014 afkomstig van het meldingsformulier met nul embryo's zijn niet opgenomen in de figuur.")
            ))
        ),
      if ("leeftijd" %in% showDataSource)
        selectInput(inputId = ns("dataSource_leeftijd"), 
          label = "Databron(nen) leeftijd", 
          choices = c("INBO" = "inbo", "INBO en meldingsformulier" = "both"),
          selected = "both"
        ),
      if ("geslacht" %in% showDataSource)
        selectInput(inputId = ns("dataSource_geslacht"), 
          label = "Databron(nen) geslacht", 
          choices = c("INBO" = "inbo", "INBO en meldingsformulier" = "both"),
          selected = "both"
        ),      
      if(showInterval)
        uiOutput(ns("interval")),
      if(showCategorie)
        uiOutput(ns("categorie")),
      if(exportPlot)
        downloadButton(ns("plotDownload"), "Download plot", class = "downloadButton"),
      if(exportData) {
        downloadButton(ns("dataDownload"), "Download data", class = "downloadButton")
      }
  
  
  )
  
  toReturn <- toReturn[!sapply(toReturn, is.null)]
  
  if(oneRow){
    width <- 12/floor(length(toReturn))
    toReturn <- lapply(toReturn, function(x) column(width = width, x))
    toReturn <- fluidRow(toReturn)
  }
  
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
#' @param allRegionsSelected boolean, whether to automatically select all 
#' options from the region selectizeInput
#' 
#' @return no return value; some output objects are created
#' @importFrom shinyjs toggle
#' @export
optionsModuleServer <- function(input, output, session, 
    data, types = NULL, labelTypes = "Type", typesDefault = types, 
    timeRange = NULL, timeLabel = "Periode", 
    multipleTypes = FALSE, allRegionsSelected = FALSE,
    definedYear = config::get("defaultYear", file = system.file("config.yml", package = "reportingGrofwild")),
    categories = NULL, intervals = NULL) {
  
  ns <- session$ns
  
  results <- reactiveValues()
  current <- reactiveValues(
    year = definedYear)
  
  
  output$time <- renderUI({
      
      results$minTime <- min(timeRange())
      
      value <- if (is.null(isolate(current$time))) {
          if (grepl("referentieper", tolower(timeLabel))) {
            c(min(timeRange()), definedYear - 1)
          } else {
            c(min(timeRange()), definedYear)
          }
        } else {
          current$time
        }
      
      sliderInput(inputId = ns("time"), label = timeLabel, 
        value = value,
        min = min(timeRange()),
        max = max(timeRange()),
        step = 1,
        sep = "")
      
    })
  observe(current$time <- input$time)
  
  
    observe({
        
        req(input$dataSource_leeftijd)
        
        # TODO for indieningType??
        subData <- tryCatch({
          tmpData <- filterGrofwild(
                plotData = data(), 
                sourceIndicator_leeftijd = input$dataSource_leeftijd,
                sourceIndicator_geslacht = input$dataSource_geslacht,
                sourceIndicator_onderkaak = input$dataSource_onderkaak,
                sourceIndicator_embryos = input$dataSource_embryos
              )
              if (!is.null(input$dataSource_embryos))
                tmpData <- tmpData[tmpData$geslacht_comp == "Vrouwelijk", ]
                            
              tmpData
            }, error = function(err) validate(need(FALSE, err$message)))
        
        newMin <- min(subData$afschotjaar)
        
        if (!is.na(newMin) && !is.infinite(newMin) && req(results$minTime) != newMin) {
          
          results$minTime <- newMin
          currentTime <- req(input$time)
          
          if (currentTime[2] < newMin) 
            currentTime[2] <- newMin
          current$time <- c(max(newMin, currentTime[1]), currentTime[2])
          updateSliderInput(session, inputId = "time", 
            value = current$time,
            min = newMin)      
        }
        
      })
    
  
  output$year <- renderUI({
        
        div(class = "sliderBlank", 
            sliderInput(inputId = ns("year"), label = "Geselecteerd Jaar", 
                value = if (is.null(current$year)) definedYear else current$year,
                min = min(timeRange()),
                max = max(timeRange()),
                step = 1,
                sep = "")
        )
        
        
      })
  observe(current$year <- input$year)
  
  
  output$source_schade <- renderUI({
      
      sourcesSchade <- loadMetaSchade()$sources
      
      selectInput(inputId = ns("dataSource_schade"), 
        label = "Databron(nen)",
        choices = sourcesSchade, selected = if (is.null(current$sources_schade)) sourcesSchade else current$sources_schade,
        multiple = TRUE)
      
      
    })
  observe(current$sources_schade <- input$dataSource_schade)
  
  
  output$region <- renderUI({
        
        validate(need(input$regionLevel, "Selecteer regio-schaal aub"))
        
        if (input$regionLevel == "flanders") {
          
          choices <- c("Vlaams Gewest")
          
        } else if (input$regionLevel == "provinces") {
          
          choices <- levels(droplevels(factor(unique(data()$provincie), 
                levels = c("West-Vlaanderen", "Oost-Vlaanderen", 
                  "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren", "Onbekend")))) 
          
          
        } else if (input$regionLevel == "faunabeheerzones") {
          
          choices <- levels(droplevels(factor(unique(data()$FaunabeheerZone), 
                levels = c(as.character(1:10), "Onbekend"))))
          
        } else {
          
          choices <- unique(data()$gemeente_afschot_locatie)
          choices <- choices[!is.na(choices)]
          choices <- choices[order(choices)]
          
        }
        
        if (!is.null(isolate(current$region)) & all(isolate(current$region) %in% choices)) {
          selected <- isolate(current$region)
        } else if (allRegionsSelected && input$regionLevel %in% c("flanders", "provinces", "faunabeheerzones")) {
          selected <- choices
        } else if (input$regionLevel == "flanders") {
          selected <- choices[1]
        } else selected <- NULL
        
        selectInput(inputId = ns("region"), label = "Regio('s)",
            choices = choices, 
            selected = selected, 
            multiple = TRUE)
        
      })
  observe(current$region <- input$region)
  
  
  ## this is applicable for 
  ## FIGUUR: Leeggewicht per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
  ## FIGUUR: Verdeling afschot over de jaren
  ## grofwild - they will have no effects on types and typesDefault in the other cases
  updateSwineDatasource <- reactiveVal(FALSE)
  
  observeEvent(input$dataSource_leeftijd, {
      updateSwineDatasource(TRUE)
    })
  
  observe({ 
      req(!is.null(input$type))   # Make sure this only runs AFTER the initial values are filled in
      
      req(updateSwineDatasource())
      
      if (!is.null(input$dataSource_leeftijd) && any(grepl("6m", types(), ignore.case = TRUE))) {
        updateSwineDatasource(FALSE)
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
      
      # Reset when types() change - switch species
      types()
      
      isolate({
          
          if (!is.null(current$type) && all(current$type %in% typesDefault())) { 
            selected <- current$type
          } else {
            selected <- typesDefault()
          }
          
          updateSwineDatasource(TRUE)
          
          selectInput(inputId = ns("type"), label = labelTypes,
            choices = types(), 
            selected = selected, 
            multiple = multipleTypes)
        
        })
      
    })
  observe(current$type <- input$type)
    
  output$categorie <- renderUI({
      
      selectInput(inputId = ns("categorie"), label = "Categorie",
        choices = categories(), selected = current$categorie)
      
    })  
  observe(current$categorie <- input$categorie)
   
  observe({
        
        shinyjs::toggle(id = "dataSource_warning", 
          condition = input$dataSourceEmbryos %in% c("both", "meldingsformulier"))
        
      })
  
  output$interval <- renderUI({
        
        selectInput(inputId = ns("interval"), label = "Interval", 
          choices = intervals, selected = current$interval)
        
      })
  observe(current$interval <- input$interval)
 
}



#' Interactive plot (ui-side)
#' @param filter boolean, whether to display filters UI
#' @inheritParams reportingGrofwild-common-args
#' @return ui object
#' @author mvarewyck
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#' @importFrom shiny NS
#' @export
plotModuleUI <- function(id, filter = FALSE) {
  
  ns <- NS(id)
  
  tagList(
    tags$div(align = "center",
      withSpinner(uiOutput(outputId = ns("plot")), hide.ui = FALSE)),
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
#' @param includeTotal boolean, whether include text with total number of records in table
#' @inheritParams reportingGrofwild-common-args
#' @return ui object
#' @author mvarewyck
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny tableOutput NS
#' @export
tableModuleUI <- function(id, includeTotal = FALSE) {
  
  ns <- NS(id)
  
  tags$div(
      style = "overflow-x: auto;",
      DT::dataTableOutput(ns("table")),
      if (includeTotal)
        uiOutput(ns("total"))
  )
  
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
#' @param typeMelding reactive with type of notification ('melding')
#' @inheritParams plotBioindicator
#' @inheritParams trendYearRegion
#' @inheritParams createSpaceData
#' @inheritParams countYearShotAnimals
#' @inheritParams barCostServer
#' @param fullNames named character vector, values for the \code{variable} to be 
#' displayed instead of original data values
#' @param height character, plot height, default is "600px" 
#' @param exportPlotWidth numeric width of the exported plot (.png)
#' @param exportPlotHeight numeric height of the exported plot (.png)
#' 
#' @return no return value; plot output object is created
#' @author mvarewyck
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.table
#' @importFrom DT datatable formatRound renderDataTable formatStyle styleEqual
#' @importFrom flexdashboard renderGauge gauge gaugeSectors
#' @importFrom dplyr coalesce
#' @export
plotModuleServer <- function(input, output, session, plotFunction, 
    data, openingstijdenData = NULL, toekenningsData = NULL,
    categorie = NULL, bioindicator = NULL, groupVariable = NULL,
    yVar = NULL,
    locaties = NULL, timeRange = NULL, unit = NULL, isSchade = NULL, 
    datatable = FALSE,  
    schadeChoices = NULL, schadeChoicesVrtg = NULL, schadeChoicesGewas = NULL, 
    variable = NULL, combinatie = NULL, title = NULL,
    fullNames = NULL, type = NULL,
    typeMelding = NULL, preSelected = reactive(NULL), filterDataOnRegion = TRUE,
    height = "600px", isWBE = FALSE,
    exportPlotWidth = 6, exportPlotHeight = 6) {
  
  subData <- reactive({
      
        provincie <- NULL  # to prevent warnings with R CMD check
        subData <- data()
        
        regionLevel <- coalesce(input$regionLevel, preSelected()$regionLevel(), NA)
        region <- coalesce(input$region, preSelected()$region(), NA)
       
        if (filterDataOnRegion && !is.na(regionLevel)) {
          
          validate(need(!is.na(region), "Gelieve regio('s) te selecteren"))
          
          # filtering regions
          if (regionLevel == "provinces" && "provincie" %in% colnames(subData)) {
            subData <- subset(subData, provincie %in% region)
          } else if (regionLevel == "faunabeheerzones") {  
            validate(need("FaunabeheerZone" %in% colnames(subData), getOutputTitle(output ="regioSchaal_warningMessage", uiText = uiText, regioSchaal = "faunabeheerzone")))
            subData <- subData[as.character(subData$FaunabeheerZone) %in% region, ]
          } else if(regionLevel == "communes") { 
            validate(need("gemeente_afschot_locatie" %in% colnames(subData), getOutputTitle(output ="regioSchaal_warningMessage", uiText = uiText, regioSchaal = "gemeente")))
            subData <- subData[subData$gemeente_afschot_locatie %in% region, ]
          } else if(regionLevel == "fbz_gemeentes") {   
            validate(need("fbz_gemeentes" %in% colnames(subData), getOutputTitle(output ="regioSchaal_warningMessage", uiText = uiText, regioSchaal = "Gemeente per faunabeheerzone")))
            subData <- subData[subData$fbz_gemeentes %in% region, ]
          } else if(regionLevel == "utm5") {   
            validate(need("utm5" %in% colnames(subData), getOutputTitle(output ="regioSchaal_warningMessage", uiText = uiText, regioSchaal = "5x5 UTM")))
            subData <- subData[subData$utm5 %in% region, ]
          }
        }
        
        
        return(subData)
        
      })
  
  wildNaam <- reactive({
      
      toReturn <- unique(data()$wildsoort)
      if (is.null(toReturn))
        toReturn <- "Wild zwijn"
      
      toReturn
      
    })
  
  
  subToekenningsData <- reactive({
        
        if (is.null(toekenningsData))
          return(NULL)
        
        provincie_toek <- NULL  # to prevent warnings with R CMD check
        labeljaar <- NULL  # to prevent warnings with R CMD check
        subData <- toekenningsData()
        
        regionLevel <- coalesce(input$regionLevel, preSelected()$regionLevel(), NA)
        region <- coalesce(input$region, preSelected()$region(), NA)
        
        if (!is.na(regionLevel)) {
          
          validate(need(!is.na(region), "Gelieve regio('s) te selecteren"))
          
          if (regionLevel == "provinces")
            subData <- subset(subData, provincie_toek %in% region)
          
        }
        
        time <- coalesce(input$time, preSelected()$time(), NA)
        if (!is.na(time))
          subData <- subset(subData, labeljaar >= time[1] & labeljaar <= time[2])
        
        return(subData)
        
      })
  
  argList <- reactive({
      
        validate(need(nrow(subData()) > 0, "Er is geen data aanwezig voor de geselecteerde filters. Gelieve een andere selectie te maken."))
#        if (plotFunction == "plotBioindicator") browser()
        
        year <- coalesce(input$year, preSelected()$year(), NA)
        time <- coalesce(input$time, preSelected()$time(), NA)
        interval <- coalesce(input$interval, preSelected()$interval(), NA)
        # In case of bio-indicator ontweid_gewicht, both leeftijd and geslacht come from type-selector
        if (!is.null(input$type) && !is.null(preSelected()$type()) && plotFunction == "plotBioindicator") {   
          typeReact <- input$type
          typeLeeftijd <- preSelected()$type()
        } else {
          typeReact <- if (!is.null(input$type)) input$type else if (!is.null(preSelected()$type())) preSelected()$type() else NA
          typeLeeftijd <- NULL
        }
        # In case of countYearShot_leeftijdscategory, both leeftijd and jachtmethode come from type-selector
        if (!is.null(input$type) && !is.null(preSelected()$type()) && plotFunction == "countYearShotAnimals") {   
          typeReact <- preSelected()$type()
          typeJacht <- input$type
        } else {
          typeReact <- if (!is.null(input$type)) input$type else if (!is.null(preSelected()$type())) preSelected()$type() else NA
          typeJacht <- NULL
        }
        regionLevel <- coalesce(input$regionLevel, preSelected()$regionLevel(), NA)
        region <- coalesce(input$region, preSelected()$region(), NA)
        summarizeBy <- coalesce(input$summarizeBy, preSelected()$summarizeBy(), NA)
        dataSource_schade <- coalesce(input$dataSource_schade, preSelected()$dataSource_schade(), NA)
        dataSource_onderkaak <- coalesce(input$dataSource_onderkaak, preSelected()$dataSource_onderkaak(), NA)
        dataSource_embryos <- coalesce(input$dataSource_embryos, preSelected()$dataSource_embryos(), NA)
        dataSource_leeftijd <- coalesce(input$dataSource_leeftijd, preSelected()$dataSource_leeftijd(), NA)
        dataSource_geslacht <- coalesce(input$dataSource_geslacht, preSelected()$dataSource_geslacht(), NA)
        
        argList <- c(
            list(data = subData()),
            if (!is.na(year))
              list(jaar = year),
            if (!any(is.na(time)))
              list(jaartallen = time[1]:time[2]),
            if (!is.na(regionLevel))
              list(regio = region),
            if (!all(is.na(typeReact)))
              list(type = typeReact),
            if (!is.null(type))
              list(type = type),
            if (plotFunction %in% c("countYearProvince")) 
              list(type = regionLevel),
            if (plotFunction %in% c("tableGewas", "tableSchadeCode")) 
              list(regionLevel = regionLevel),
            if (plotFunction == "plotBioindicator") 
              list(isWBE = isWBE),
            if (!is.null(typeLeeftijd))  # In case of bio-indicator ontweid_gewicht, both leeftijd and geslacht come from type-selector
              list(type_leeftijd = typeLeeftijd),
            if (!is.null(typeJacht))  # In case of countYearShot_leeftijdscategory, both leeftijd and jachtmethode come from type-selector
              list(type_jachtmethode = typeJacht),
            if (!is.null(openingstijdenData) && !all(is.na(typeReact)) & !is.na(year) & all(is.na(dataSource_schade)))
              list(openingstijdenData = openingstijdenData()),
            if (!is.null(subToekenningsData()))
              list(assignedData = subToekenningsData()),
            if (!is.null(categorie))          
              list(categorie = categorie) else if (!is.null(input$categorie))          
              list(categorie = input$categorie),
            if (!all(is.na(summarizeBy)))
              list(summarizeBy = summarizeBy),
            if(!is.null(bioindicator))
              list(bioindicator = bioindicator),
            if(!is.null(groupVariable))
              list(groupVariable = groupVariable),
            if(!is.null(yVar))
              list(yVar = yVar),
            if(!is.null(fullNames))
              list(fullNames = fullNames),
            
            if (!is.null(isSchade))
              list(isSchade = isSchade),
            
            # Sources
            if(!all(is.na(dataSource_schade)))
              list(sourceIndicator = dataSource_schade),            
            if(!all(is.na(dataSource_leeftijd)))
              list(sourceIndicator_leeftijd = dataSource_leeftijd),
            if(!all(is.na(dataSource_geslacht)))
              list(sourceIndicator_geslacht = dataSource_geslacht),
            if(!all(is.na(dataSource_onderkaak)))
              list(sourceIndicator = dataSource_onderkaak),
            if(!all(is.na(dataSource_embryos)))
              list(sourceIndicator = dataSource_embryos),
            
            if (!is.null(locaties))
              list(locaties = locaties()),
            if (!is.null(timeRange))
              list(timeRange = timeRange()),
            if (!is.null(unit))
              list(unit = unit()),
            if(!is.null(typeMelding))
              list(typeMelding = typeMelding()),
            if (!is.null(schadeChoices))
              list(schadeChoices = schadeChoices()),
            if (!is.null(schadeChoicesVrtg))
              list(schadeChoicesVrtg = schadeChoicesVrtg()),
            if (!is.null(schadeChoicesGewas))
              list(schadeChoicesGewas = schadeChoicesGewas()),
            if (!is.null(variable))
              list(variable = variable),
            if (plotFunction %in% c("trendYearRegion", "countYearProvince")) {
              if (!is.null(combinatie()))
                list(combinatie = combinatie())
            },
            if (!is.null(title))
              list(title = title),
            if (!is.na(interval))
              list(interval = interval)
        
        )
        
        
      })
  
  resultFct <- reactive({
        
        tryCatch({
            tmpResult <- do.call(plotFunction, args = argList())
            validate(need(!is.null(tmpResult), "Niet beschikbaar"))
            c(tmpResult, isolate(reactiveValuesToList(input)))
          },
            error = function(err) {
              validate(need(FALSE, err$message))
            }
        )		
        
      })
  
    
    output$plotly <- renderPlotly({  
        
        req("plotly" %in% class(resultFct()$plot))
        
        resultFct()$plot %>%
          config(toImageButtonOptions = list(width = 1300, height = 800))
        
      })
    
    output$ggplot <- renderPlot({  
        
        req("ggplot" %in% class(resultFct()$plot))
        
        resultFct()$plot
        
      })  
    
    output$plot <- renderUI({
        
        tryCatch({
            if ("plotly" %in% class(resultFct()$plot))
              plotlyOutput(session$ns("plotly"), height = height) else if ("ggplot" %in% class(resultFct()$plot))
              plotOutput(session$ns("ggplot"), height = height)
          },
          error = function(e) {
            return(NULL) 
          })
        
        
      })
    
  # Prevent that plotly images are squeezed
  outputOptions(output, "plotly", suspendWhenHidden = FALSE)
    
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
        
      tryCatch({
          tags$em(resultFct()$warning)
        },
        error = function(e) {
          return(e) 
        })
        
      })
    
  output$plotDownload <- downloadHandler(
    filename = function() nameFile(species = wildNaam(),
            year = if (!is.null(input$year)) 
                  input$year else if (!is.null(input$time))
                  unique(c(input$time[1], input$time[2])) else if (!is.null(timeRange))
                  timeRange() else
                  unique(data()$year), 
            extraInfo = input$type,
            content = paste0(plotFunction, "_data"), fileExt = "png"),
        content = function(file) {
          
          resPlot <- resultFct()$plot
          
          validate(
            need(resPlot, "Niet beschikbaar"),
            need("ggplot" %in% class(resPlot), "Niet beschikbaar")
          )
          
          ggsave(file, resPlot, width = exportPlotWidth, height = exportPlotHeight, dpi = 150)
          
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
#          formatRound(colnames(resultFct()$data)[-1], digits = 0, mark = "") %>%
          formatStyle(
            colnames(resultFct()$data)[1],
            target = "row",
            fontWeight = styleEqual(tail(resultFct()$data[, 1], n = 1), "bold")
          )
        
      } else {
        
        tmpTable <- resultFct()$data
        
        DT::datatable(tmpTable, rownames = FALSE,
            options = list(dom = 't', pageLength = -1,
              columnDefs = list(list(targets = grep("Warning", colnames(tmpTable)) - 1, visible = FALSE)))) %>%
          formatStyle(
            colnames(tmpTable)[1],
            target = "row",
            fontWeight = styleEqual(tail(tmpTable[, 1], n = 1), "bold")
          ) %>%
          formatStyle(
            grep("Verandering", colnames(tmpTable), value = TRUE),
            grep("Warning", colnames(tmpTable), value = TRUE),
            color = styleEqual(c("oranje", "rood"), c("orange", "red"))
          )
        
      }

    })
  
  return(reactive(resultFct()))
  
}


#' Display formatted frequency table of data (ui-side)
#' @param data, character vector, values for which frequency table should be generated
#' @param variable character, name of the variable that is summarized
#' @param fullNames named character vector, values for the \code{variable} to be 
#' displayed instead of original data values
#' @inheritParams reportingGrofwild-common-args
#' @return ui object (tagList)
#' @importFrom sf st_drop_geometry
#' @export
dataModuleServer <- function(id, data, variable, fullNames = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      
      freqTable <- reactive({
          
          req(data())
          validate(need(nrow(data()) > 0, "Geen data beschikbaar"))
          
          myTable <- as.data.frame(table(data()[, variable]), stringsAsFactors = FALSE)
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
      output$table <- DT::renderDT({
          
          validate(need(freqTable(), "Geen data beschikbaar"))
          DT::datatable(freqTable(), rownames = FALSE,
            options = list(dom = 't', pageLength = -1))
          
        })
      
      # Total number of records
      output$total <- renderUI({
          
          req(freqTable())
          helpText("Totaal:", sum(freqTable()$Aantal))
        })
      
    })
  
}
