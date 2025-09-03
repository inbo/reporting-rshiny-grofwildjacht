                    
#' Server function for an output (plot/table) of the 'schade' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export               
schadeOutputServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(), plot = reactiveVal(),
  subcategories = character(),
  schade_code, schade_gewas, schade_voertuig,
  outputs = character(), defaultTabs = NULL,
  schadeData, spatialData, biotoopData, 
  defaultYear, 
  schadeTypes, schadeCodes,
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## input
    results <- reactiveValues(renderedTabs = "Schade")
    
    # Filter data upon user choices
    results$schade_data <- reactive({
          
      # Select species & code & exclude data before 2014
      toRetain <- 
        schadeData$wildsoort %in% req(specie()) &
        schadeData$schadeBasisCode %in% req(schadeSelection$schade_code()) &
        schadeData$afschotjaar >= 2014
          
      # Filter gewas
      if ("GEWAS" %in% schadeSelection$schade_code()) {
        otherCodes <- schadeSelection$schade_code()[schadeSelection$schade_code() != "GEWAS"]
        toRetain <- toRetain &
          (schadeData$schadeBasisCode %in% otherCodes |
            schadeData$schadeCode %in% schadeSelection$schade_gewas())
      }
          
      # Filter voertuig
      if ("VRTG" %in% schadeSelection$schade_code()) {
        otherCodes <- schadeSelection$schade_code()[schadeSelection$schade_code() != "VRTG"]
        toRetain <- toRetain &
          (schadeData$schadeBasisCode %in% otherCodes |
          schadeData$schadeCode %in% schadeSelection$schade_voertuig())
      }
          
      return(schadeData[toRetain, ])
    })

    results$schade_timeRange <- reactive(
      range(results$schade_data()$afschotjaar)
    ) 
    
    ## Selection species
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Selection schade
    
    schadeSelection <- schadeSelectionServer(id = "topbar", specie = specie,
      schade_code = schade_code, schade_gewas = schade_gewas, 
      schade_voertuig = schade_voertuig, plot = plot)
    
    ## Main panel
  
    ## Tab content with selected plot/table

    outputServer <- reactiveVal(NULL)
    
    # Create plot - UI side
    observe({
        req(subcategory())
        req(plot())
        
      if(subcategory() %in% subcategories){
         
        ui <- switch(subcategory(), 
          "schade-vlaanderen" = {
            tagList(
              if ("countYearProvinceUI-schade" %in% outputs)
                wellPanel(class = "well-white", countYearProvinceUI(
                    id = ns("plot"), 
                    uiText = uiText, 
                    plotFunction = "countYearProvinceUI-schade",
                    specie = specie(),
                    showType = TRUE, doHide = !(plot() == defaultTabs$plot || "countYearProvinceUI-schade" %in% plot()),
                    showDataSource = "schade"
                  )),
              if ("tableSchadeSummaryUI" %in% outputs)
                wellPanel(class = "well-white", tableSchadeSummaryUI(
                    id = ns("plot"), 
                    uiText = uiText, specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "tableSchadeSummaryUI" %in% plot())
                  )),
              if ("trendYearFlandersUI-schade" %in% outputs)
                wellPanel(class = "well-white", trendYearFlandersUI(
                    id = ns("plot"),
                    type = "wildschade",
                    uiText = uiText,
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "trendYearFlandersUI-schade" %in% plot())
                  )),
            )
          },
          "schade-regio" = {
            tagList(
              if ("mapFlandersUI-schade" %in% outputs)
                wellPanel(class = "well-white", mapFlandersUI(
                    id = ns("plot"), 
                    type = "schade", plotDetails = "region",
                    showCombine = FALSE, uiText = uiText, 
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "mapFlandersUI-schade" %in% plot())
                  )),
              if ("mapSchadeUI" %in% outputs)
                wellPanel(class = "well-white", mapSchadeUI(
                    id = ns("plot"), 
                    filterVariable = TRUE,
                    variableChoices = 
                      c("Type schade" = "schadeCode",
                        "Seizoen" = "season",
                        "Jaar" = "afschotjaar"
                      ),
                    doHide = !(plot() == defaultTabs$plot || "mapSchadeUI" %in% plot())
                  ))
            )
          },
          "schade-type" = {
            tagList(
              if ("countYearSchadeUI-gewas" %in% outputs)
                wellPanel(class = "well-white", countYearSchadeUI(
                    id = ns("plot2"), 
                    doHide = !(plot() == defaultTabs$plot || "countYearSchadeUI-gewas" %in% plot()),
                    uiText = uiText, context = "description",
                    type = "gewas", specie = specie()
                  )),
              if ("countYearSchadeUI-wildschade" %in% outputs)
                wellPanel(class = "well-white", countYearSchadeUI(
                    id = ns("plot1"), 
                    doHide = !(plot() == defaultTabs$plot || "countYearSchadeUI-wildschade" %in% plot()),
                    uiText = uiText, context = "description",
                    type = "schade", specie = specie()
                  )),
              
              if ("tableGewasUI" %in% outputs)
                wellPanel(class = "well-white", tableGewasUI(
                    id = ns("plot"), 
                    uiText = uiText, context = "description",
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "tableGewasUI" %in% plot())
                  )),
              if ("tableSchadeUI" %in% outputs)
                wellPanel(class = "well-white", tableSchadeUI(
                    id = ns("plot"), 
                    uiText = uiText, context = "description",
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "tableSchadeUI" %in% plot())
                  ))
            )
          },
          "schade-seizoen" = {
            tagList(
              if ("countYearSchadeUI-seizoen" %in% outputs)
                wellPanel(class = "well-white", countYearSchadeUI(
                    id = ns("plot"), 
                    doHide = !(plot() == defaultTabs$plot || "countYearSchadeUI-seizoen" %in% plot()),
                    uiText = uiText, context = "description",
                    type = "seizoen", specie = specie(),
                    regionLevels = c(1:2, 4)
                  ))
            )
          },
          "schade-kosten" = {
            tagList(
              if ("barCostUI" %in% outputs)
                wellPanel(class = "well-white", barCostUI(
                    id = ns("plot"), 
                    uiText = uiText, context = "description",
                    specie = specie(),
                    typeMelding = c("Landbouw" = "landbouw"),
                    regionLevels = c(1:2, 4),
                    doHide = !(plot() == defaultTabs$plot || "barCostUI" %in% plot())
                  ))
            )
          }
        )
       
        # include plot/table in UI
        output[["output"]] <- renderUI(ui)
        
        # activate server-side update
        outputServer(subcategory())
        
      }
      
    })

    # Server
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      
        switch(outputServer(), 
          "schade-vlaanderen" = {
            c(
              if ("tableSchadeSummaryUI" %in% outputs)
                tableSchadeSummaryServer(
                  id = "plot", 
                  data = results$schade_data, 
                  schadeTypes = schadeTypes, schadeCodes = schadeCodes
                ),
              if ("trendYearFlandersUI-schade" %in% outputs)
                trendYearFlandersServer(
                  id = "plot", 
                  geoData = results$schade_data, 
                  allSpatialData = spatialData, 
                  biotoopData = biotoopData, 
                  species = specie,
                  type = "wildschade",
                  uiText = uiText
                ),
              if ("countYearProvinceUI-schade" %in% outputs)
                countYearProvinceServer(
                  id = "plot",
                  data = results$schade_data,
                  types = reactive(c(
                      "Vlaanderen" = "flanders",
                      "Provincie" = "provinces", 
                      "Faunabeheerzones" = "faunabeheerzones"
                    )), 
                  labelTypes = "Regio", 
                  typesDefault = reactive("provinces"), 
                  timeRange = results$schade_timeRange
                )
            )
          },
          "schade-regio" = {
            c(
              if ("mapFlandersUI-schade" %in% outputs)
                mapFlandersServer(
                  id = "plot",
                  uiText = uiText,
                  defaultYear = defaultYear,
                  species = specie,
                  type = "schade",
                  geoData = results$schade_data,
                  biotoopData = biotoopData,
                  allSpatialData = spatialData,
                  sourceChoices = loadMetaSchade()$sources 
                ),
              if ("mapSchadeUI" %in% outputs)
                mapSchadeServer(
                  id = "plot", 
                  schadeData = results$schade_data,
                  allSpatialData = reactive(spatialData),
                  timeRange = results$schade_timeRange,
                  defaultYear = defaultYear,
                  species = specie,
                  borderRegion = "provinces",
                  uiText = uiText,
                  type = "schade"
                )
            )
          },
          "schade-type" = {
            c(
              if ("countYearSchadeUI-wildschade" %in% outputs)
                countYearSchadeServer(
                  id = "plot1",
                  data = results$schade_data,
                  type = "schadeCode", 
                  timeRange = results$schade_timeRange,
                  fullNames = schadeCodes
                ),
              if ("tableSchadeUI" %in% outputs)
                tableSchadeServer(
                  id = "plot",  
                  data = results$schade_data,
                  types = reactive(c(
                      "Vlaanderen" = "flanders",
                      "Provincie" = "provinces", 
                      "Faunabeheerzones" = "faunabeheerzones"
                    )), 
                  labelTypes = "Regio", 
                  typesDefault = reactive("provinces"), 
                  timeRange = results$schade_timeRange,
                  schadeChoices = schadeSelection$schade_code,
                  schadeChoicesVrtg = schadeSelection$schade_voertuig,
                  schadeChoicesGewas = schadeSelection$schade_gewas,
                  datatable = TRUE,
                  fullNames = c(schadeTypes, schadeCodes)
                ),
              if ("countYearSchadeUI-gewas" %in% outputs)
                countYearSchadeServer(
                  id = "plot2",
                  data = results$schade_data,
                  type = "SoortNaam", 
                  timeRange = results$schade_timeRange,
                  fullNames = schadeCodes
                ),
              if ("tableGewasUI" %in% outputs)
                tableGewasServer(
                  id = "plot",
                  data = results$schade_data,
                  types = reactive(c(
                      "Vlaanderen" = "flanders",
                      "Provincie" = "provinces", 
                      "Faunabeheerzones" = "faunabeheerzones"
                    )), 
                  labelTypes = "Regio", 
                  typesDefault = reactive("provinces"),
                  timeRange = results$schade_timeRange,
                  variable = "SoortNaam"
                ) 
            )
          },
          "schade-seizoen" = {
            c(
              if ("countYearSchadeUI-seizoen" %in% outputs)
                countYearSchadeServer(
                  id = "plot",
                  data = results$schade_data,
                  type = "season", 
                  timeRange = results$schade_timeRange
                )
            )
          },
          "schade-kosten" = {
            c(
              if ("barCostUI" %in% outputs)
                barCostServer(
                  id = "plot",
                  data = results$schade_data,
                  yVar = "schadeBedrag"
                )  
            )
          }
        )
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })
    
    
    return(list(
        specie = reactive(specie()),
        schade_code = schadeSelection$schade_code,
        schade_gewas = schadeSelection$schade_gewas,
        schade_voertuig = schadeSelection$schade_voertuig
      ))

  })
  
}

#' UI function for the sidebar of the 'schade' Category page
#' @inheritParams reportingGrofwild-common-args
#' @author lcougnaud
schadeSelectionUI <- function(id){
  
  ns <- NS(namespace = id)
  
  # freeze input parameters choice (same id) for all sub-tabs
  wellPanel(class = "well-white", 
    fluidRow(
      uiOutput(ns("schadeCodeSelection")),
      uiOutput(ns("schadeGewasSelection")),
      uiOutput(ns("schadeVoertuigSelection"))
    ),
    uiOutput(outputId = ns("schade_warning"))
  )

}

#' Server function for the sidebar of the 'schade' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
schadeSelectionServer <- function(id, specie = reactiveVal(), 
  schade_code, schade_gewas, schade_voertuig, plot){
  
  moduleServer(id, function(input, output, session){
  
    metaSchade <- loadMetaSchade()
    ns <- session$ns
      
    specieSidebarServer(id = id, specie = specie)
    
    gewasPlot <- c("countYearSchadeUI-gewas", "tableGewasUI")
    
    output$schadeCodeSelection <- renderUI({
        
        # Select type schade
        column(4, selectInput(
            inputId = ns("schade_code"), 
            label = "Selecteer type(s) schade:",
            choices = metaSchade$types,
            selected = if (is.null(schade_code()))
                metaSchade$types else
                schade_code(),
            multiple = TRUE,
            width = "100%"
          ))
        
      })
    
    output$schadeGewasSelection <- renderUI({
        
        req("GEWAS" %in% input$schade_code)
        
        # Subselection gewas
        column(4, selectInput(
          inputId = ns("schade_gewas"), 
          label = "Filter Gewas Schade",
          choices = metaSchade$codes[["GEWAS"]],
          selected = if (is.null(schade_gewas()))
              metaSchade$codes[["GEWAS"]] else
              schade_gewas(),
          multiple = TRUE,
          width = "100%"
        ))
        
      })
    
    output$schadeVoertuigSelection <- renderUI({
        
        req("VRTG" %in% input$schade_code)
        
        # Subselection voertuig
        column(4, selectInput(
          inputId = ns("schade_voertuig"), 
          label = "Filter Voertuig Schade",
          choices = metaSchade$codes[["VRTG"]],
          selected = if (is.null(schade_voertuig()))
              metaSchade$codes[["VRTG"]] else
              schade_voertuig(),
          multiple = TRUE,
          width = "100%"
        ))
        
    })

    
    observe({
      updateSelectInput(session, inputId = "schade_code", selected = schade_code())
    })
    observe(
      updateSelectInput(session, inputId = "schade_gewas", selected = schade_gewas())
    )
    observe(
      updateSelectInput(session, inputId = "schade_voertuig", selected = schade_voertuig())
    )
    
    output$schade_warning <- renderUI({
          validate(need(input$schade_code, "Gelieve type(s) schade te selecteren"))
    })
    
  return(list(
      schade_code = reactive({
            req(input$schade_code)
      }),
      schade_gewas = reactive(req(input$schade_gewas)),
      schade_voertuig = reactive(req(input$schade_voertuig))
      ))
        
  })
}