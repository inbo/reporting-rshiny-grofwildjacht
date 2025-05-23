#' Server function for the cards of the 'schade' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return named list with 
#' \code{plot}, reactive with name of output plot/table (if selected);
#' \code{schade_code}, reactive with selected schade code choices;
#' \code{schade_gewas}, reactive with selected schade gewas choices;
#' \code{schade_voertuig}, reactive with selected schade voertuig choices;
#' @import shiny
#' @author lcougnaud
#' @export
schadeCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  schade_code, schade_gewas, schade_voertuig,
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
        
    ## input
    results <- reactiveValues(renderedTabs = "Schade")
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Main panel
        
    observe({    
              
      if(subcategory() %in% subcategories){
        
        categoryCards <- lapply(outputs, function(output){
              
          args <- list(
            output = output,
            id = id, 
            uiText = uiText,
            specie = specie(), 
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
    outputUI <- reactiveVal()
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

  
  return(list(
      plot = reactive(outputUI())
    ))
     
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
  schade_code, schade_gewas, schade_voertuig,
  outputs = character(),
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
      schade_code = schade_code, schade_gewas = schade_gewas, schade_voertuig = schade_voertuig)
    
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
              uiText = uiText, specie = specie()
            )
          },
          "trendYearFlandersUI-schade" = {
            trendYearFlandersUI(
              id = ns(outputName),
              type = "wildschade",
              uiText = uiText, specie = specie()
            )
          },
          "countYearProvinceUI-schade" = {
            countYearProvinceUI(
              id = ns(outputName), 
              uiText = uiText, type = "schade",
              specie = specie(),
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
              specie = specie(), typeTitle = "schade"
            )
          },
          "countYearSchadeUI-wildschade" = {
            countYearSchadeUI(
              id = ns(outputName), 
              doHide = FALSE,
              uiText = uiText, context = "description",
              type = "schade", specie = specie()
            )
          },
          "mapSchadeUI-wildschade" = {
            mapSchadeUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              type = "schade", specie = specie(),
              filterVariable = FALSE,
              doHide = FALSE
            )
          },
          "tableSchadeUI" = {
            tableSchadeUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countYearSchadeUI-gewas" = {
            countYearSchadeUI(
              id = ns(outputName), 
              doHide = FALSE,
              uiText = uiText, context = "description",
              type = "gewas", specie = specie()
            )
          },
          "tableGewasUI" = {
            tableGewasUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countYearSchadeUI-seizoen" = {
            countYearSchadeUI(
              id = ns(outputName), 
              doHide = FALSE,
              uiText = uiText, context = "description",
              type = "seizoen", specie = specie(),
              regionLevels = c(1:2, 4)
            )
          },
          "mapSchadeUI-seizoen" = {
            mapSchadeUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              type = "schade", specie = specie(),
              filterVariable = FALSE,
              doHide = FALSE
            )
          },
          # dash plot F09_2
          "barCostUI" = {
            barCostUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
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
        "trendYearFlandersUI-schade" = trendYearFlandersServer(
          id = outputName, 
          geoData = results$schade_data, 
          allSpatialData = spatialData, 
          biotoopData = biotoopData, 
          species = specie,
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
          species = specie,
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
          species = specie,
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
          schadeChoices = schadeSelection$schade_code,
          schadeChoicesVrtg = schadeSelection$schade_voertuig,
          schadeChoicesGewas = schadeSelection$schade_gewas,
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
          species = specie,
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
      column(4, uiOutput(ns("schadeCodeSelection"))),
      column(4, uiOutput(ns("schadeGewasSelection"))),
      column(4, uiOutput(ns("schadeVoertuigSelection")))
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
  schade_code, schade_gewas, schade_voertuig){
  
  moduleServer(id, function(input, output, session){
  
    metaSchade <- loadMetaSchade()
    ns <- session$ns
      
    specieSidebarServer(id = id, specie = specie)
    
    output$schadeCodeSelection <- renderUI({
        
        # Select type schade
        selectInput(
          inputId = ns("schade_code"), 
          label = "Selecteer type(s) schade:",
          choices = metaSchade$types,
          selected = if (is.null(schade_code()))
              metaSchade$types else
              schade_code(),
          multiple = TRUE,
          width = "100%"
        )
        
      })
    
    output$schadeGewasSelection <- renderUI({
        
        # Select gewas & voertuig
        selectInput(
          inputId = ns("schade_gewas"), 
          label = "Filter Gewas Schade",
          choices = metaSchade$codes[["GEWAS"]],
          selected = if (is.null(schade_gewas()))
              metaSchade$codes[["GEWAS"]] else
              schade_gewas(),
          multiple = TRUE,
          width = "100%"
        )
        
      })
    
    output$schadeVoertuigSelection <- renderUI({
        
        selectInput(
          inputId = ns("schade_voertuig"), 
          label = "Filter Voertuig Schade",
          choices = metaSchade$codes[["VRTG"]],
          selected = if (is.null(schade_voertuig()))
              metaSchade$codes[["VRTG"]] else
              schade_voertuig(),
          multiple = TRUE,
          width = "100%"
        )
        
      })
        
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
      schade_code = reactive(req(input$schade_code)),
      schade_gewas = reactive(req(input$schade_gewas)),
      schade_voertuig = reactive(req(input$schade_voertuig))
      ))
        
  })
}