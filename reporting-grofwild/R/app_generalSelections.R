# TODO: Add comment
# 
# Author: sjunius
###############################################################################


#' UI function for the topbar of the different Category pages
#' @param hideGeneralFilters boolean whether to hide all filters
#' @inheritParams optionsModuleServer
#' @inheritParams reportingGrofwild-common-args
#' @importFrom shinyjs useShinyjs
#' @author sjunius
generalSelectionUI <- function(id, showTime = FALSE, showType = FALSE, showYear = FALSE,
  showRegion = FALSE, showInterval = FALSE, showDataSource = c(), showUnit = FALSE,
  summarizeBy = NULL, hideGeneralFilters = FALSE){
  
  ns <- NS(namespace = id)

  if (hideGeneralFilters) {
    NULL
  } else {
    tagList(
      useShinyjs(),
      div(class = "collapsible-well",
        wellPanel(class = "well-white", 
          div(class = "well-header",
            tagList(
              tags$h3("Algemene filters", style = "margin-top: 0px;"),
              tags$button("—", 
                id = ns("toggleBtn1"), 
                class = "collapse-btn",
                `data-target` = ns("content"))
            )
          ),
          
          div(id = ns("content"), class = "well-content",
            tags$script(HTML("  
                  // When Shiny creates or updates any selectize input
                  $(document).on('shiny:bound', function(e) {
                  var el = e.target;
                  // Check that the element is a selectize input
                  if (el && el.selectize && $(el).attr('multiple')) {
                  var s = el.selectize;
                  
                  // Remove Shiny's default immediate change trigger
                  s.off('change');
                  
                  // On blur, send the value back to Shiny
                  s.on('blur', function() {
                  Shiny.setInputValue(el.id, s.getValue(), {priority: 'event'});
                  });
                  }
                  });
                  ")),   ## Only trigger selectizeInputs on blur
            fluidRow(column(11, 
                fluidRow(
                  uiOutput(ns("schadeCodeSelection")),
                  uiOutput(ns("schadeGewasSelection")),
                  uiOutput(ns("schadeVoertuigSelection"))
                ),
                uiOutput(outputId = ns("schade_warning")),
                fluidRow(
                  if(showYear)
                    column(6, uiOutput(ns("year"))),
                  if (showTime)
                    column(6, uiOutput(ns("time"))),
                  if(showType)
                    column(6, uiOutput(ns("type"))),
                  if(showInterval)
                    column(6, uiOutput(ns("interval"))),
                  if (!is.null(summarizeBy))
                    column(6, 
                      uiOutput(ns("summarizeBy"))),
                  if (showUnit)
                    column(6, 
                      uiOutput(ns("unit")))
                ),
                fluidRow(
                  if(showRegion)
                    column(6, 
                      tagList(
                        fluidRow(
                          column(12, uiOutput(ns("regionLevels")))
                        ),
                        fluidRow(
                          column(11, offset = 1, uiOutput(ns("region")))
                        ))
                    )
                ),
                fluidRow(
                  if ("schade" %in% showDataSource)
                    column(4, uiOutput(ns("source_schade"))),
                  if ("onderkaak" %in% showDataSource)
                    column(4, uiOutput(ns("source_onderkaak"))),
                  if ("embryos" %in% showDataSource)
                    column(4, uiOutput(ns("source_embryos"))),
                  if ("leeftijd" %in% showDataSource)
                    column(4, uiOutput(ns("source_leeftijd"))),
                  if ("geslacht" %in% showDataSource)
                    column(4, uiOutput(ns("source_geslacht")))
                )
              )))
        ),
        tags$script(HTML("
              // Function to initialize collapse functionality
              function initializeCollapse() {
              // Remove any existing event listeners to prevent duplicates
              $(document).off('click.collapse', '.collapse-btn');
              
              // Add event listener with namespace
              $(document).on('click.collapse', '.collapse-btn', function() {
              var btn = $(this);
              var targetId = btn.attr('data-target');
              var content = $('#' + targetId);
              
              if (content.hasClass('collapsed')) {
              content.removeClass('collapsed');
              btn.html('—');
              } else {
              content.addClass('collapsed');
              btn.html('+');
              }
              });
              }
              
              $(document).ready(function() {
              initializeCollapse();
              
              // Re-initialize when tab is shown (for tabPanel/navbarPage)
              $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
              setTimeout(initializeCollapse, 100);
              });
              
              // Alternative for Shiny's tabsetPanel
              $(document).on('shiny:value', function(event) {
              if (event.target.classList && event.target.classList.contains('tabbable')) {
              setTimeout(initializeCollapse, 100);
              }
              });
              });
              "))
      )
    )
  }
}

#' Server function for the topbar of the different Category pages
#' @param includeSchadeFilters boolean, whether to show the filters on schade
#' @param schadeSources character vector with schade sources
#' @param units character vector with possible units
#' @inheritParams optionsModuleServer
#' @inheritParams reportingGrofwild-common-args
#' @return Shiny module function
#' @import shiny
#' @author sjunius
#' @export
generalSelectionServer <- function(id, subcategory, includeSchadeFilters = FALSE,
  schade_code = NULL, schade_gewas = NULL, schade_voertuig = NULL, schadeSources = NULL, types = reactive(NULL), labelTypes = "Type", typesDefault = types, 
  timeRange = NULL, timeLabel = "Periode", summarizeBy = NULL, data = reactive(NULL),
  multipleTypes = FALSE, allRegionsSelected = FALSE, 
  definedYear = config::get("defaultYear", file = system.file("config.yml", package = "reportingGrofwild")),
  categories = NULL, intervals = NULL, regionLevels = NULL, regionLevelSelected = NULL, units = NULL){
  
  moduleServer(id, function(input, output, session){
      
      ns <- session$ns
      
      results <- reactiveValues()
      
      current <- reactiveValues(
        year = definedYear)
      
      ## Schade Filters
      metaSchade <- loadMetaSchade()
      gewasSubCat <- c("schade-type-gewas")
      
      output$schadeCodeSelection <- renderUI({
          req(includeSchadeFilters)
          req(!subcategory() %in% gewasSubCat)
          
          # Select type schade
          column(4, selectizeInput(
              inputId = ns("schade_code"), 
              label = "Selecteer type(s) schade:",
              choices = metaSchade$types,
              selected = if (!is.null(current$schadeCode))
                  current$schadeCode else if (is.null(schade_code()))
                  metaSchade$types else
                  schade_code(),
              multiple = TRUE,
              width = "100%"
            ))
          
        })
      observe(current$schadeCode <- input$schade_code)
      
      output$schadeGewasSelection <- renderUI({
          req(includeSchadeFilters)
          if (!subcategory() %in% gewasSubCat)
            req("GEWAS" %in% input$schade_code)
          
          # Subselection gewas
          column(4, selectizeInput(
              inputId = ns("schade_gewas"), 
              label = "Filter Gewas Schade",
              choices = metaSchade$codes[["GEWAS"]],
              selected = if (!is.null(current$schadeGewas))
                  current$schadeGewas else if (is.null(schade_gewas()))
                  metaSchade$codes[["GEWAS"]] else
                  schade_gewas(),
              multiple = TRUE,
              width = "100%"
            ))
          
        })
      observe(current$schadeGewas <- input$schade_gewas)
      
      output$schadeVoertuigSelection <- renderUI({
          req(includeSchadeFilters)
          req(!subcategory() %in% gewasSubCat)
          req("VRTG" %in% input$schade_code)
          
          # Subselection voertuig
          column(4, selectizeInput(
              inputId = ns("schade_voertuig"), 
              label = "Filter Voertuig Schade",
              choices = metaSchade$codes[["VRTG"]],
              selected = if (!is.null(current$schadeVoertuig))
                  current$schadeVoertuig else if (is.null(schade_voertuig()))
                  metaSchade$codes[["VRTG"]] else
                  schade_voertuig(),
              multiple = TRUE,
              width = "100%"
            ))
          
        })
      observe(current$schadeVoertuig <- input$schade_voertuig)
      
      
      observe({
          req(includeSchadeFilters)
          updateSelectizeInput(session, inputId = "schade_code", selected = schade_code())
        })
      observe({
          req(includeSchadeFilters)
          updateSelectizeInput(session, inputId = "schade_gewas", selected = schade_gewas())
        })
      observe({
          req(includeSchadeFilters)
          updateSelectizeInput(session, inputId = "schade_voertuig", selected = schade_voertuig())
        })
      
      output$schade_warning <- renderUI({
          req(includeSchadeFilters)
          if (!subcategory() %in% gewasSubCat)
            validate(need(input$schade_code, "Gelieve type(s) schade te selecteren"))
        })
      
      
      ## Time filters
      output$time <- renderUI({
          
          results$minTime <- min(timeRange())
          
          value <- if (is.null(current$time)) {
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
      time_released <- debounce(reactive(input$time), 500)   # waits 500ms after the last change before updating
      observe(current$time <- time_released())
      
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
      year_released <- debounce(reactive(input$year), 500)  # waits 500ms after the last change before updating
      observe(current$year <- year_released())
      
      
      ## Type filters
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
              updateSelectizeInput(session, inputId = "type",
                choices = c("Frisling", "Overloper", "Volwassen", "Onbekend"),
                selected = c("Frisling", "Overloper", "Volwassen", "Onbekend"))
              
            } else {
              
              updateSelectizeInput(session, inputId = "type",
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
              
              selectizeInput(inputId = ns("type"), label = labelTypes,
                choices = types(), 
                selected = selected, 
                multiple = multipleTypes)
              
            })
          
        })
      observe(current$type <- input$type)
      
      ## Interval Filters
      output$interval <- renderUI({
          
          selectizeInput(inputId = ns("interval"), label = "Interval", 
            choices = intervals, selected = current$interval)
          
        })
      observe(current$interval <- input$interval)
      
      
      ## Unit Filters
      output$unit <- renderUI({
          
          selectizeInput(inputId = ns("unit"), label = "Eenheid", 
            choices = units, selected = current$unit)
          
        })
      observe(current$unit <- input$unit)
      
      
      ## SummarizeBy Filters
      output$summarizeBy <- renderUI({
          
          radioButtons(inputId = ns("summarizeBy"), label = "Rapporteer",
              choices = summarizeBy)
          
        })
      observe(current$summarizeBy <- input$summarizeBy)
      
      
      ## Region Filters
      output$regionLevels <- renderUI({
          req(regionLevels)
          
          if (all(regionLevels %in% 1:4)) {
            regionChoices <- c(
              "Vlaanderen" = "flanders", 
              "Provincie" = "provinces", 
              "Gemeente" = "communes", 
              "Faunabeheerzones" = "faunabeheerzones")[regionLevels]
          } else {
            regionChoices <- regionLevels
          }
          
          
          selectizeInput(inputId = ns("regionLevel"), label = "Regio-schaal",
            choices = regionChoices,
            selected = if (is.null(current$regionLevel)) regionLevelSelected else current$regionLevel)
          
        })
      observe({
          current$regionLevel <- input$regionLevel
          current$region <- NULL
        })
      
      
      output$region <- renderUI({
          req(regionLevels)
          validate(need(input$regionLevel, "Selecteer regio-schaal aub"))
          
          isolate(
            if (input$regionLevel == "flanders") {
                
                choices <- c("Vlaams Gewest")
                
              } else if (input$regionLevel == "provinces") {
                
                choices <- c("West-Vlaanderen", "Oost-Vlaanderen", 
                  "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren", "Onbekend")
                
                
              } else if (input$regionLevel == "faunabeheerzones") {
                
                choices <- c(as.character(1:10), "Onbekend")
                
              } else {
                
                if (all(regionLevels %in% 1:4)) {
                  choices <- unique(data()$gemeente_afschot_locatie)
                  choices <- choices[!is.na(choices)]
                  choices <- choices[order(choices)]
                } else {
                  choices <- sort(unique(data()[[input$regionLevel]]$NAAM))
                }
                
              }
          )
          
          if (!is.null(isolate(current$region)) & all(isolate(current$region) %in% choices)) {
            selected <- isolate(current$region)
          } else if (allRegionsSelected && input$regionLevel %in% c("flanders", "provinces", "faunabeheerzones")) {
            selected <- choices
          } else if (input$regionLevel == "flanders") {
            selected <- choices[1]
          } else selected <- NULL
          
          selectizeInput(inputId = ns("region"), label = "Regio('s)",
            choices = choices, 
            selected = selected, 
            multiple = TRUE)
       
          
        })
      region_released <- debounce(reactive(input$region), 1000)   # waits 1000ms after the last change before updating
      observe(current$region <- region_released())
      
      ## Sources Filter
      output$source_schade <- renderUI({
          
          selectizeInput(inputId = ns("dataSource_schade"), 
            label = "Databron(nen)",
            choices = schadeSources, selected = if (is.null(current$sources_schade)) schadeSources else current$sources_schade,
            multiple = TRUE)
        })
      observe(current$sources_schade <- input$dataSource_schade)
      
      output$source_onderkaak <- renderUI({
          
          selectizeInput(inputId = ns("dataSource_onderkaak"), 
              label = "Databron(nen) voor onderkaaklengte",
              choices = c("INBO" = "inbo", 
                "Meldingsformulier" = "meldingsformulier",  
                "INBO en meldingsformulier" = "both"),
              selected = if (is.null(current$dataSource_onderkaak)) "both" else current$dataSource_onderkaak
            )
        })
      observe(current$sources_onderkaak <- input$dataSource_onderkaak)
      
      output$source_embryos <- renderUI({
          
          tagList(
            selectizeInput(inputId = ns("dataSource_embryos"), 
                label = "Databron(nen) voor aantal embryo's", 
                choices = c("INBO" = "inbo", 
                  "Meldingsformulier" = "meldingsformulier",  
                  "INBO en meldingsformulier" = "both"),
                selected = if (is.null(current$sources_embryos)) "both" else current$sources_embryos
              ),
              shinyjs::hidden(tags$div(id = "dataSource_warning", style = "margin-bottom:10px;",
                  helpText("Observaties", HTML("v&#x00F3;&#x00F3;r"), "2014 afkomstig van het meldingsformulier met nul embryo's zijn niet opgenomen in de figuur.")
                ))
            )
        })
      observe(current$sources_embryos <- input$dataSource_embryos)
      
      output$source_leeftijd <- renderUI({
          
          selectizeInput(inputId = ns("dataSource_leeftijd"), 
              label = "Databron(nen) leeftijd", 
              choices = c("INBO" = "inbo", "INBO en meldingsformulier" = "both"),
              selected = if (is.null(current$sources_leeftijd)) "both" else current$sources_leeftijd
            )
        })
      observe(current$sources_leeftijd <- input$dataSource_leeftijd)
      
      output$source_geslacht <- renderUI({
          
          selectizeInput(inputId = ns("dataSource_geslacht"), 
              label = "Databron(nen) geslacht", 
              choices = c("INBO" = "inbo", "INBO en meldingsformulier" = "both"),
              selected = if (is.null(current$sources_geslacht)) "both" else current$sources_geslacht
            )
        })
      observe(current$sources_geslacht <- input$dataSource_geslacht)
      
      
      
      return(list(
          schade_code = reactive({
              if (subcategory() %in% gewasSubCat)
                "GEWAS" else 
                req(input$schade_code)
            }),
          schade_gewas = reactive(req(input$schade_gewas)),
          schade_voertuig = reactive(req(input$schade_voertuig)),
          time = reactive(time_released()),
          year = reactive(year_released()),
          interval = reactive(input$interval),
          type = reactive(input$type),
          regionLevel = reactive(input$regionLevel),
          region = reactive(region_released()),
          unit = reactive(input$unit),
          summarizeBy = reactive(input$summarizeBy),
          dataSource_schade = reactive(input$dataSource_schade),
          dataSource_onderkaak = reactive(input$dataSource_onderkaak),
          dataSource_embryos = reactive(input$dataSource_embryos),
          dataSource_leeftijd = reactive(input$dataSource_leeftijd),
          dataSource_geslacht = reactive(input$dataSource_geslacht)
        ))
      
    })
}
