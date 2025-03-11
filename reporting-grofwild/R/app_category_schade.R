#' Server function for the cards of the 'schade' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of output plot/table (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
schadeCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText){
  
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
        
        categoryCards <- lapply(outputs, function(output){
              
          args <- list(
            output = output,
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "schade"
          )
          
          if(output == "countYearProvinceUI-schade"){
          		args[["output"]] <- "countYearProvinceUI"
          		args[["outputFunction"]] <- output
          }else if(output == "mapFlandersUI-schade"){
            args[["output"]] <- "mapFlandersUI"
          }else if(output == "countYearSchadeUI-wildschade"){
            args <- c(args, 
              list(outputFunction = "countYearSchadeUI", type = "wildschade")
            )
          }else if(output == "mapSchadeUI-wildschade"){
            args <- c(args, 
              list(outputFunction = "mapSchadeUI", type = "wildschade")
            )
          }else if(output == "countYearSchadeUI-gewas"){
            args <- c(args, 
              list(outputFunction = "countYearSchadeUI", type = "gewas")
            )
          }else if(output == "countYearSchadeUI-seizoen"){
            args <- c(args, 
              list(outputFunction = "countYearSchadeUI", type = "seizoen")
            )
          }else if(output == "mapSchadeUI-seizoen"){
            args <- c(args, 
              list(outputFunction = "mapSchadeUI", type = "seizoen")
            )
          }
          do.call(categoryCard, args)
        })
    
        args <- c(categoryCards, list(width = 1/3, gap = "2em"))
        cards <- do.call(bslib::layout_column_wrap, args)
        
        output[["output"]] <- renderUI(cards)
        
      } 
   })
   
   # if plot is selected based on the category cards
    outputUI <- reactiveVal("Visualisatie/Tabel")
    lapply(outputs, function(output){
      btn <- paste0(output, "-button")
      # exceptions
      if(output %in% c("countYearProvinceUI-schade", "mapFlandersUI-schade"))
        btn <- paste0(sub("(.+)-.+", "\\1", output), "-button")
      observeEvent(
        input[[btn]], 
        outputUI(output), 
        ignoreInit = TRUE
      )
    })

    return(outputUI)
     
  })
     
}
                              
#' Server function for an output (plot/table) of the 'schade' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export               
schadeOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal(),
  outputs = character(),
  schadeData, spatialData, biotoopData, 
  defaultYear, 
  schadeTypes, schadeCodes,
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## input
    results <- reactiveValues(renderedTabs = "Schade")
    
    results$specie <- reactive(specie())
    
    # Filter data upon user choices
    results$schade_data <- reactive({
          
      # Select species & code & exclude data before 2014
      toRetain <- 
        schadeData$wildsoort %in% req(results$specie()) &
        schadeData$schadeBasisCode %in% req(input$`sidebar-schade_code`) &
        schadeData$afschotjaar >= 2014
          
      # Filter gewas
      if ("GEWAS" %in% input$`sidebar-schade_code`) {
        otherCodes <- input$`sidebar-schade_code`[input$`sidebar-schade_code` != "GEWAS"]
          toRetain <- toRetain &
            (schadeData$schadeBasisCode %in% otherCodes |
            schadeData$schadeCode %in% input$`sidebar-schade_gewas`)
      }
          
      # Filter voertuig
      if ("VRTG" %in% input$`sidebar-schade_code`) {
        otherCodes <- input$`sidebar-schade_code`[input$`sidebar-schade_code` != "VRTG"]
        toRetain <- toRetain &
          (schadeData$schadeBasisCode %in% otherCodes |
          schadeData$schadeCode %in% input$`sidebar-schade_voertuig`)
      }
          
      return(schadeData[toRetain, ])
    })

    results$schade_timeRange <- reactive(
      range(results$schade_data()$afschotjaar)
    ) 
    
    ## Sidebar panel
    
    schadeSidebarServer(id = "sidebar", specie = results$specie)
    
    # specie is updated in this page
    observe( 
      updateSelectInput(session, inputId = "sidebar-specie", 
        selected = specie())
    )
    
    observeEvent(input$`sidebar-specie`, 
      results$specie <- reactive(input$`sidebar-specie`))
    
    ## Main panel
  
    ## Tab content with selected plot/table

    outputServer <- reactiveVal(NULL)
    
    # Create plot - UI side
    observe({
          
      if(plot() %in% outputs){
          
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
              filterVariable = FALSE,
              doHide = FALSE
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
              filterVariable = FALSE,
              doHide = FALSE
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
          type = "wildschade"
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
          schadeChoices = reactive(input$`sidebar-schade_code`),
          schadeChoicesVrtg = reactive(input$`sidebar-schade_voertuig`),
          schadeChoicesGewas = reactive(input$`sidebar-schade_gewas`),
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
    
    return(reactive(results$specie()))

  })
  
}

#' UI function for the sidebar of the 'schade' Category page
#' @param ... any extra parameters for \code{\link{specieSidebarUI}}
#' @inheritParams reportingGrofwild-common-args
#' @inherit specieSidebarUI return
#' @author lcougnaud
schadeSidebarUI <- function(id, 
  schadeTypes, gewasChoices, voertuigChoices, ...){
  
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
#' @inheritParams reportingGrofwild-common-args
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