#' Server function for the cards of the 'schade' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
schadeCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal()){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
        
    ## input
    results <- reactiveValues(renderedTabs = "Grofwild")
    results$specie <- reactive(specie())
    
    ## Sidebar panel
    
    schadeSidebarServer(id = "sidebar", specie = results$specie)
    
    ## Main panel
        
    observe({    
              
      if(subcategory() %in% subcategories){
        
        categoryCardSchade <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "schade", 
              ...
          )
        }
        
        group <- strsplit(subcategory(), split = "-")[[1]][2]
        
        cards <- switch(group, 
            
          vlaanderen =           
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardSchade(output = "tableSchadeSummaryUI"),
              categoryCardSchade(output = "trendYearFlandersUI"),
              categoryCardSchade(output = "countYearProvinceUI",
                outputFunction = "countYearProvinceUI-schade")
          ),
          regio =         
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardSchade(output = "mapFlandersUI")
           ),
          type =          
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardSchade(
                output = "countYearSchadeUI-wildschade", 
                outputFunction = "countYearSchadeUI",
                type = "wildschade"
              ),
              categoryCardSchade(
                output = "mapSchadeUI-wildschade", 
                outputFunction = "mapSchadeUI",
                type = "wildschade"
              ),
              categoryCardSchade(output = "tableSchadeUI"),
              categoryCardSchade(
                output = "countYearSchadeUI-gewas", 
                outputFunction = "countYearSchadeUI",
                type = "gewas"
              ),
              categoryCardSchade(output = "tableGewasUI")
          ),          
          seizoen =        
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardSchade(
                output = "countYearSchadeUI-seizoen", 
                outputFunction = "countYearSchadeUI",
                type = "seizoen"
              ),
              categoryCardSchade(
                output = "mapSchadeUI-seizoen", 
                outputFunction = "mapSchadeUI",
                type = "seizoen"
              )
            ),
          kosten =         
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardSchade(output = "barCostUI")
            )
        )
        output[["output"]] <- renderUI(cards)
        
      } 
   })
     
  })
     
}
                              
#' Server function for an output (plot/table) of the 'schade' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export               
schadeOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal()){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    schadeOutputs <- getOutputs(category = "schade")
    
    ## input
    results <- reactiveValues(renderedTabs = "Schade")
    
    results$specie <- reactive(specie())
    
    # Filter data upon user choices
    results$schade_data <- reactive({
          
      # Select species & code & exclude data before 2014
      toRetain <- 
        schadeData$wildsoort %in% req(results$specie()) &
        schadeData$schadeBasisCode %in% req(input$schade_code) &
        schadeData$afschotjaar >= 2014
          
      # Filter gewas
      if ("GEWAS" %in% input$schade_code) {
        otherCodes <- input$schade_code[input$schade_code != "GEWAS"]
          toRetain <- toRetain &
            (schadeData$schadeBasisCode %in% otherCodes |
            schadeData$schadeCode %in% input$schade_gewas)
      }
          
      # Filter voertuig
      if ("VRTG" %in% input$schade_code) {
        otherCodes <- input$schade_code[input$schade_code != "VRTG"]
        toRetain <- toRetain &
          (schadeData$schadeBasisCode %in% otherCodes |
          schadeData$schadeCode %in% input$schade_voertuig)
      }
          
      return(schadeData[toRetain, ])
    })

    results$schade_timeRange <- reactive(
      range(results$schade_data()$afschotjaar)
    ) 
    
    ## Sidebar panel
    
    schadeSidebarServer(id = "sidebar", specie = results$specie)
    
    ## Main panel
  
    ## Tab content with selected plot/table

    outputServer <- reactiveVal(NULL)

    # Create plot - UI side
    observe({
          
      if(plot() %in% schadeOutputs){
          
        outputName <- plot()
          
        # create the plot/table
        ui <- switch(outputName, 
          "tableSchadeSummaryUI" = {
            tableSchadeSummaryUI(
              id = ns(outputName), 
              uiText = uiText, specie = results$specie()
            )
          },
          "trendYearFlandersUI" = {
            trendYearFlandersUI(
              id = ns(outputName),
              type = "wildschade",
              includeOptions = TRUE,
              uiText = uiText, specie = results$specie()
            )
          },
          "countYearProvinceUI-schade" = {
            countYearProvinceUI(
              id = ns(outputName), 
              uiText = uiText, type = "schade",
              specie = results$specie(),
              showType = TRUE, doHide = FALSE,
              showDataSource = "schade",
              plotFunction = "countYearProvinceUI-schade"
            )
          },
          "mapFlandersUI-schade" = {
            mapFlandersUI(
              id = ns(outputName), 
              type = "wildschade", plotDetails = "region",
              showCombine = FALSE,
              uiText = uiText, outputFunction = "mapFlandersUI-schade",
              specie = results$specie(), typeTitle = "schade"
            )
          },
          "countYearSchadeUI-wildschade" = {
            countYearSchadeUI(
              id = ns(outputName), 
              doHide = FALSE,
              uiText = uiText, context = "description",
              type = "schade", specie = results$specie()
            )
          },
          "mapSchadeUI-wildschade" = {
            mapSchadeUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              type = "schade", specie = results$specie(),
              filterVariable = FALSE
            )
          },
          "tableSchadeUI" = {
            tableSchadeUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
              doHide = FALSE
            )
          },
          "countYearSchadeUI-gewas" = {
            countYearSchadeUI(
              id = ns(outputName), 
              doHide = FALSE,
              uiText = uiText, context = "description",
              type = "gewas", specie = results$specie()
            )
          },
          "tableGewasUI" = {
            tableGewasUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
              doHide = FALSE
            )
          },
          "countYearSchadeUI-seizoen" = {
            countYearSchadeUI(
              id = ns(outputName), 
              doHide = FALSE,
              uiText = uiText, context = "description",
              type = "seizoen", specie = results$specie(),
              regionLevels = c(1:2, 4)
            )
          },
          "mapSchadeUI-seizoen" = {
            mapSchadeUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              type = "schade", specie = results$specie(),
              filterVariable = FALSE
            )
          },
          # dash plot F09_2
          "barCostUI" = {
            barCostUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
              typeMelding = c("Landbouw" = "landbouw"),
              regionLevels = c(1:2, 4),
              doHide = FALSE
            )
          }
        )
          
        # include plot/table in UI
        output[["output"]] <- renderUI(ui)
        
        # activate server-side update
        outputServer(outputName)
        
      }
      
    })

    # Server
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      outputName <- outputServer()
      
      switch(outputName,
        "tableSchadeSummaryUI" = tableSchadeSummaryServer(
          id = outputName, 
          data = results$schade_data, 
          schadeTypes = schadeTypes, schadeCodes = schadeCodes
        ),
        "trendYearFlandersUI" = trendYearFlandersServer(
          id = outputName, 
          geoData = results$schade_data, 
          allSpatialData = spatialData, 
          biotoopData = biotoopData, 
          species = results$specie,
          type = "wildschade",
          includeOptions = TRUE
        ),
        "countYearProvinceUI-schade" = countYearProvinceServer(
          id = outputName,
          data = results$schade_data,
          types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
          )), 
          labelTypes = "Regio", 
          typesDefault = reactive("provinces"), 
          timeRange = results$schade_timeRange
        ),
        "mapFlandersUI-schade" = mapFlandersServer(
          id = outputName,
          uiText = uiText, outputFunction = "mapFlandersServer-schade",
          defaultYear = defaultYear,
          species = results$specie,
          type = "wildschade",
          geoData = results$schade_data,
          biotoopData = biotoopData,
          allSpatialData = spatialData,
          sourceChoices = loadMetaSchade()$sources 
        ),
        "countYearSchadeUI-wildschade" = countYearSchadeServer(
          id = outputName,
          data = results$schade_data,
          type = "schadeCode", 
          timeRange = results$schade_timeRange,
          fullNames = schadeCodes
        ),
        "mapSchadeUI-wildschade" = mapSchadeServer(
          id = outputName, 
          schadeData = results$schade_data,
          allSpatialData = reactive(spatialData),
          timeRange = results$schade_timeRange,
          defaultYear = defaultYear,
          species = results$specie,
          borderRegion = "provinces",
          variable = "schadeCode"
        ),
        "tableSchadeUI" = tableSchadeServer(
          id = outputName,  
          data = results$schade_data,
          types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
          )), 
          labelTypes = "Regio", 
          typesDefault = reactive("provinces"), 
          timeRange = results$schade_timeRange,
          schadeChoices = reactive(input$schade_code),
          schadeChoicesVrtg = reactive(input$schade_voertuig),
          schadeChoicesGewas = reactive(input$schade_gewas),
          datatable = TRUE,
          fullNames = c(schadeTypes, schadeCodes)
        ),
        "countYearSchadeUI-gewas" = countYearSchadeServer(
          id = outputName,
          data = results$schade_data,
          type = "SoortNaam", 
          timeRange = results$schade_timeRange,
          fullNames = schadeCodes
        ),
        "tableGewasUI" = tableGewasServer(
          id = outputName,
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
        ),
        "countYearSchadeUI-seizoen" = countYearSchadeServer(
          id = outputName,
          data = results$schade_data,
          type = "season", 
          timeRange = results$schade_timeRange
        ),
        "mapSchadeUI-seizoen" = mapSchadeServer(
          id = outputName, 
          schadeData = results$schade_data,
          allSpatialData = reactive(spatialData),
          timeRange = results$schade_timeRange,
          defaultYear = defaultYear,
          species = results$specie,
          borderRegion = "provinces",
          variable = "season"
        ),
        "barCostUI" = barCostServer(
          id = outputName,
          data = results$schade_data,
          yVar = "schadeBedrag"
        )  
      )
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })

  })
  
}

#' UI function for the sidebar of the 'schade' Category page
#' @inheritParams specieSidebarUI
#' @inherit specieSidebarUI return
#' @author lcougnaud
schadeSidebarUI <- function(id, ...){
  
  ns <- NS(namespace = id)
  
  specieSidebarUI(
    id = id, 
    # freeze input parameters choice (same id) for all sub-tabs
    bottomExtra = tagList(
      br(),
      # Select type schade
      selectInput(
        inputId = ns("schade_code"), 
        label = "Selecteer type(s) schade:",
        choices = schadeTypes,
        selected = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      # Select gewas & voertuig
      shinyjs::hidden(
        selectInput(
          inputId = ns("schade_gewas"), 
          label = "Filter Gewas Schade",
          choices = gewasChoices,
          selected = gewasChoices,
          multiple = TRUE,
          width = "100%"
        )
      ),
      shinyjs::hidden(
        selectInput(
          inputId = ns("schade_voertuig"), 
          label = "Filter Voertuig Schade",
          choices = voertuigChoices,
          selected = voertuigChoices,
          multiple = TRUE,
          width = "100%"
        )
      ),
      uiOutput(outputId = ns("schade_warning"))
    ),
    ...
  )
  
}

#' Server function for the sidebar of the 'schade' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
schadeSidebarServer <- function(id, specie = reactiveVal()){
  
  moduleServer(id, function(input, output, session){
        
    specieSidebarServer(id = "sidebar", specie = specie)
        
    # show/hide filters
    observe(
      shinyjs::toggle(
        id = "schade_gewas", 
        condition = "GEWAS" %in% input$schade_code
      )
    )
    observe(
      shinyjs::toggle(
        id = "schade_voertuig", 
        condition = "VRTG" %in% input$schade_code
      )
    )
    
    output$schade_warning <- renderUI({
      validate(need(input$schade_code, "Gelieve type(s) schade te selecteren"))
    })
        
  })
}