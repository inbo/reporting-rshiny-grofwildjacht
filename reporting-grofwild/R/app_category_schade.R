#' UI for the 'schade' Category page
#' @param id id character, module id
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @export
schadeUI <- function(id, specie){
  
  ns <- NS(namespace = id)
      
  verticalLayout(
          
    # header
    headerUI(
      path = c("home", "specie", "category", "subcategory", "plot"), 
      id = id, specie = specie, 
      category = "Schade",
      subcategory = getTabTitle(value = "vlaanderen", category = "schade")
    ),
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-schade-header.png", width = "100%")
      )
    ),
    br(),
           
    # navigation page with plots and specie sidebar panel
    navbarPage(
        
      title = "",
      
      id = ns("subcategory"),
            
      tabPanel(
        title = getTabTitle(value = "informatie", category = "schade"), 
        value = "informatie",
        schadePanel(
          id = id, specie = specie,
          tags$div(
              align = "center",
              h1("Welkom op de informatiepagina rond wildschade")
          ),
          welcomeSectionUI(
            id = id, context = "fauna", uiText = uiText, 
            maxDate = max(schadeData$afschot_datum, na.rm = TRUE)
          )
        )
      ),
      tabPanel(
        title = getTabTitle(value = "vlaanderen", category = "schade"), 
        value = "vlaanderen",
        schadePanel(
          id = id, specie = specie,
          uiOutput(outputId = ns("output-vlaanderen"))
        )
      ),
      tabPanel(
        title = getTabTitle(value = "regio", category = "schade"), 
        value = "regio",
        schadePanel(
          id = id, specie = specie,
          uiOutput(outputId = ns("output-regio"))
        )
      ),
      tabPanel(
          title = getTabTitle(value = "type", category = "schade"), 
          value = "type",
          schadePanel(
            id = id, specie = specie,
            uiOutput(outputId = ns("output-type"))
          )
      ),
      tabPanel(
        title = getTabTitle(value = "seizoen", category = "schade"), 
        value = "seizoen",
        schadePanel(
          id = id, specie = specie,
          uiOutput(outputId = ns("output-seizoen"))
        )
      ),
      tabPanel(
        title = getTabTitle(value = "kosten", category = "schade"), 
        value = "kosten",
        schadePanel(
          id = id, specie = specie,
          uiOutput(outputId = ns("output-kosten"))
        )
      )

    )
  )
  
}

#' Server function for the 'afschot' Category page
#' @param id id character, module id/specie
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
schadeServer <- function(id, specie){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    outputCreated <- reactiveVal(value = NULL)
    nextPage <- reactiveVal(value = NULL)
    
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
    
    ## Header
    
    # Update subcategory in path
    output$pathSubcategory <- renderUI(
      actionLink(
        inputId = ns("pathSubcategory-button"), 
        label = getTabTitle(
          value = input$subcategory, 
          category = "schade"
        )
      )
    )
    
    ## Sidebar with input parameters
    
    # show/hide filters
    observe(
      toggle(
        id = ns("schade_gewas"), 
        condition = "GEWAS" %in% input$schade_code
      )
    )
    observe(
      toggle(
        id = ns("schade_voertuig"), 
        condition = "VRTG" %in% input$schade_code
      )
    )
    
    output$schade_warning <- renderUI({
      validate(need(input$schade_code, "Gelieve type(s) schade te selecteren"))
    })
    
    ## Tab with available plots
    
    initTab <- reactiveVal(TRUE)
    # Go back to page if subcategory is clicked on in the path
    observeEvent(input$`pathSubcategory-button`, initTab(TRUE))
    observeEvent(input$subcategory, initTab(TRUE))

    # Create tab
    observe(if(initTab()){

      if(isTruthy(input$subcategory)){
        
        categoryCardSchade <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "schade", 
            ...
          )
        }
        
        switch(input$subcategory, 
            
          vlaanderen = {
            output$`output-vlaanderen` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardSchade(output = "tableSchadeSummaryUI"),
                categoryCardSchade(output = "trendYearFlandersUI"),
                categoryCardSchade(output = "countYearProvinceUI")
              )
            )
          },
          regio = {
            output$`output-regio` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardSchade(output = "mapFlandersUI")
              )
            )
          },
          type = {
            output$`output-type` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardSchade(
                  output = "countYearSchadeUI-schade", 
                  outputFunction = "countYearSchadeUI",
                  type = "schade"
                ),
                categoryCardSchade(
                  output = "mapSchadeUI-type", 
                  outputFunction = "mapSchadeUI",
                  type = "schade"
                ),
                categoryCardSchade(output = "tableSchadeUI"),
                categoryCardSchade(
                  output = "countYearSchadeUI-gewas", 
                  outputFunction = "countYearSchadeUI",
                  type = "gewas"
                ),
                categoryCardSchade(output = "tableGewasUI")
              )
            )      
          },
          seizoen = {
            output$`output-seizoen` <- renderUI(          
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
              )
            )      
          },
          kosten = {
            output$`output-kosten` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardSchade(output = "barCostUI")
              )
            )
          }
        )
        
      }
      
      outputCreated("")# reset path
      initTab(FALSE)
      
    })
  
    ## Tab content with selected plot/table

    # UI
    observeEvent(input$`tableSchadeSummaryUI-button`, {
      output$`output-vlaanderen` <- renderUI(
        tableSchadeSummaryUI(
          id = ns("table-summary"), 
          uiText = uiText, specie = results$specie()
        )
      )
       outputCreated("tableSchadeSummaryUI")
    })
    
    observeEvent(input$`trendYearFlandersUI-button`, {
      output$`output-vlaanderen` <- renderUI(
          trendYearFlandersUI(
            id = ns("schade"),
            type = "wildschade",
            includeOptions = TRUE,
            uiText = uiText, specie = results$specie()
          )
        )
        outputCreated("trendYearFlandersUI")
    })

    observeEvent(input$`countYearProvinceUI-button`, {
      output$`output-vlaanderen` <- renderUI(
        countYearProvinceUI(
          id = ns("schade"), 
          uiText = uiText, type = "schade",
          specie = results$specie(),
          showType = TRUE, doHide = FALSE,
          showDataSource = "schade"
        )
      )
      outputCreated("countYearProvinceUI")
    })

    observeEvent(input$`mapFlandersUI-button`, {
      output$`output-regio` <- renderUI(
        mapFlandersUI(
          id = ns("schade"), 
          type = "wildschade", plotDetails = "region",
          showCombine = FALSE,
          uiText = uiText, specie = results$specie(), typeTitle = "schade"
        )
      )
      outputCreated("mapFlandersUI")
    })

    observeEvent(input$`countYearSchadeUI-schade-button`, {
      output$`output-type` <- renderUI(
        countYearSchadeUI(
          id = ns("schade"), 
          doHide = FALSE,
          uiText = uiText, context = "schade",
          type = "schade", specie = results$specie()
        )
      )
      outputCreated("countYearSchadeUI-schade")
    })

    observeEvent(input$`mapSchadeUI-type-button`, {
      output$`output-type` <- renderUI(
        mapSchadeUI(
          id = ns("schade-type"), 
          uiText = uiText, context = "schade",
          type = "schade", specie = results$specie(),
          filterVariable = FALSE
        )
      )
      outputCreated("mapSchadeUI-type")
    })

    observeEvent(input$`tableSchadeUI-button`, {
      output$`output-type` <- renderUI(
        tableSchadeUI(
          id = ns("schade"), 
          uiText = uiText, context = "schade",
          specie = results$specie(),
          doHide = FALSE
        )
      )
      outputCreated("tableSchadeUI")
    })

    observeEvent(input$`countYearSchadeUI-gewas-button`, {
      output$`output-type` <- renderUI(
        countYearSchadeUI(
          id = ns("schade-gewas"), 
          doHide = FALSE,
          uiText = uiText, context = "schade",
          type = "gewas", specie = results$specie()
        )
      )
      outputCreated("countYearSchadeUI-gewas")
    })

    observeEvent(input$`tableGewasUI-button`, {
      output$`output-type` <- renderUI(
        tableGewasUI(
          id = ns("schade"), 
          uiText = uiText, context = "schade",
          specie = results$specie(),
          doHide = FALSE
        )
      )
      outputCreated("tableGewasUI")
    })

    observeEvent(input$`countYearSchadeUI-seizoen-button`, {
      output$`output-seizoen` <- renderUI(
        countYearSchadeUI(
          id = ns("schade-seizoen"), 
          doHide = FALSE,
          uiText = uiText, context = "schade",
          type = "seizoen", specie = results$specie(),
          regionLevels = c(1:2, 4)
        )
      )
      outputCreated("countYearSchadeUI-seizoen")
    })

    observeEvent(input$`mapSchadeUI-seizoen-button`, {
      output$`output-seizoen` <- renderUI(
        mapSchadeUI(
          id = ns("schade-seizoen"), 
          uiText = uiText, context = "schade",
          type = "schade", specie = results$specie(),
          filterVariable = FALSE
        )
      )
      outputCreated("mapSchadeUI-seizoen")
    })

    # dash plot F09_2
    observeEvent(input$`barCostUI-button`, {
      output$`output-kosten` <- renderUI(
        barCostUI(
          id = ns("schade"), 
          uiText = uiText, context = "schade",
          specie = results$specie(),
          typeMelding = c("Landbouw" = "landbouw"),
          regionLevels = c(1:2, 4),
          doHide = FALSE
        )
      )
      outputCreated("barCostUI")
    })

    observe(print(outputCreated()))
    
    # Update plot in path
    output$pathPlot <- renderText({
      if(isTruthy(outputCreated()))
        getOutputTitle(
          output = sub("(.+)(-[[:alnum:]]{1,})$", "\\1", outputCreated()), 
          uiText = uiText, specie = results$specie(), type = "schade",
          n = 55
        )
      else ""
    })

    # Server
    observe(
      switch(outputCreated(),
        tableSchadeSummaryUI = tableSchadeSummaryServer(
          id = id, 
          data = results$schade_data, 
          schadeTypes = schadeTypes, schadeCodes = schadeCodes
        ),
        trendYearFlandersUI = trendYearFlandersServer(
          id = id, 
          geoData = results$schade_data, 
          allSpatialData = spatialData, 
          biotoopData = biotoopData, 
          species = results$specie,
          type = "wildschade",
          includeOptions = TRUE
        ),
        countYearProvinceUI = countYearProvinceServer(
          id = id,
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
        mapFlandersUI = mapFlandersServer(
          id = id,
          uiText = uiText,
          defaultYear = defaultYear,
          species = results$specie,
          type = "wildschade",
          geoData = results$schade_data,
          biotoopData = biotoopData,
          allSpatialData = spatialData,
          sourceChoices = loadMetaSchade()$sources 
        ),
        `countYearSchadeUI-schade` = countYearSchadeServer(
          id = "schade",
          data = results$schade_data,
          type = "schadeCode", 
          timeRange = results$schade_timeRange,
          fullNames = schadeCodes
        ),
        `mapSchadeUI-type`= mapSchadeServer(
          id = "schade-type", 
          schadeData = results$schade_data,
          allSpatialData = reactive(spatialData),
          timeRange = results$schade_timeRange,
          defaultYear = defaultYear,
          species = results$specie,
          borderRegion = "provinces",
          variable = "schadeCode"
        ),
        tableSchadeUI = tableSchadeServer(
          id = "schade",  
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
        `countYearSchadeUI-gewas` = countYearSchadeServer(
            id = "schade-gewas",
            data = results$schade_data,
            type = "SoortNaam", 
            timeRange = results$schade_timeRange,
            fullNames = schadeCodes
        ),
        tableGewasUI = tableGewasServer(
          id = "schade",
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
        `countYearSchadeUI-seizoen` = countYearSchadeServer(
            id = "schade-seizoen",
            data = results$schade_data,
            type = "season", 
            timeRange = results$schade_timeRange
        ),
        `mapSchadeUI-seizoen`= mapSchadeServer(
          id = "schade-seizoen", 
          schadeData = results$schade_data,
          allSpatialData = reactive(spatialData),
          timeRange = results$schade_timeRange,
          defaultYear = defaultYear,
          species = results$specie,
          borderRegion = "provinces",
          variable = "season"
        ),
        barCostUI = barCostServer(
          id = "schade",
          data = results$schade_data,
          yVar = "schadeBedrag"
        )
        
      )
    )
      
    ## Output
      
    # Redirection:

    observeEvent(input$`pathHome`, {
      print("schade: Go to home page")
      nextPage("home")
    })
  
    observeEvent(input$`pathSpecie-button`, {
      print("schade: Go to specie page")
      nextPage("specie")
    })
      
    return(nextPage)

  })
  
}

#' Wrapper for the sidebar of the 'afschot' Category page
#' @param id id character, module id/specie
#' @param ... Elements for the \code{\link[shiny]{mainPanel}}
#' @inherit shiny::sidebarLayout return
#' @author lcougnaud
schadePanel <- function(id, specie, ...){
  
  ns <- NS(namespace = id)
  
  sidebarLayout(
    position = "left", 
      
    sidebarPanel = sidebarPanel(
      width = 3, 
      id = ns("category-sidebar"), 
      h4(specie, align = "center"),
      img(src = getSpecieImage(specie = specie, relative = TRUE), width = "100%", height = "auto"),
      br(),
      div(strong(paste("Latijn:", getLatinName(specie = specie))), align = "center"),
      br(),
      # Select type schade
      selectInput(
        inputId = ns("schade_code"), 
        label = "Selecteer type(s) schade:",
        choices = schadeTypes,
        selected = schadeTypes,
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
      uiOutput(ns("schade_warning"))
    ),
      
    mainPanel = mainPanel(width = 9, ...)

  )
  
}