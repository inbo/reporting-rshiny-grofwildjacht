schadeOutputs <- c(
  "tableSchadeSummaryUI", "trendYearFlandersUI", 
  "countYearProvinceUI-schade", "mapFlandersUI-schade", 
  "countYearSchadeUI-wildschade", "mapSchadeUI-wildschade", 
  "tableSchadeUI", "countYearSchadeUI-gewas", "tableGewasUI", 
  "countYearSchadeUI-seizoen", "mapSchadeUI-seizoen", "barCostUI"
)

#' UI for the 'schade' Category page
#' @param id id character, module id
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @export
schadeUI <- function(id, specie){
  
  ns <- NS(namespace = id)
  
  # header
  tagList(
        
    headerUI(
      path = c("home", "specie", "category", "subcategory", "plot"), 
      id = id, #specie = specie, 
      category = "Schade",
      subcategory = getTabTitle(value = "vlaanderen", category = "schade")
    ),
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-schade-header.png", width = "100%")
      )
    ),
    
    sidebarLayout(
    
      sidebarPanel = schadePanel(id = id, specie = specie),
    
      mainPanel = mainPanel(
  
        div(class = "navbar-schade", 
        navbarPage(
          
          id = ns("subcategory"),
          
          selected = "informatie",
          
          title = "",
          
          # navigation bar overlays side bar
          position = "static-top", #"fixed-top",
          
          tabPanel(
            title = getTabTitle(value = "vlaanderen", category = "schade"), 
            value = "vlaanderen",
            uiOutput(outputId = ns("output-vlaanderen"))
          ),
          tabPanel(
            title = getTabTitle(value = "regio", category = "schade"), 
            value = "regio",
            uiOutput(outputId = ns("output-regio"))
          ),
          tabPanel(
            title = getTabTitle(value = "type", category = "schade"), 
            value = "type",
            uiOutput(outputId = ns("output-type"))
          ),
          tabPanel(
            title = getTabTitle(value = "seizoen", category = "schade"), 
            value = "seizoen",
            uiOutput(outputId = ns("output-seizoen"))
          ),
          tabPanel(
            title = getTabTitle(value = "kosten", category = "schade"), 
            value = "kosten",
            uiOutput(outputId = ns("output-kosten"))
          ),
          # menu with all plots/tables
          tabPanelAll(
            category = "schade", id = id,
            outputs = schadeOutputs, 
            uiText = uiText
          ),
          tabPanelInformatie(
            category = "schade", id = id, 
            uiText = uiText, 
            maxDate = max(schadeData$afschot_datum, na.rm = TRUE),
            specie = specie
          )
        ))
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
schadeServer <- function(id){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    outputCreated <- reactiveVal(value = NULL)
    nextPage <- reactiveVal(value = NULL)
    
    ## input
    results <- reactiveValues(renderedTabs = "Schade")
    
    results$specie <- reactive(input$specie)
    
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
    
    # Update specie in path
    output$pathSpecie <- renderUI(
      actionLink(
        inputId = ns("pathSpecie-button"), 
        label = results$specie()
      )    
    )
    
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
    
    # Specie image	
    output$`specie-image` <- renderImage(
      list(src = getSpecieImage(specie = results$specie()), width = "100%")
      , deleteFile = FALSE)
    
    # Specie latin name
    output$`specie-name` <- renderText(
      paste("Latijn:", getLatinName(specie = results$specie()))
    )
    
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
    
    ## Tab with available plots
    
    initTab <- reactiveVal(TRUE)
    # Go back to page if subcategory is clicked on in the path
    observeEvent(input$`pathSubcategory-button`, initTab(TRUE))
    observeEvent(input$subcategory, initTab(TRUE))
    
    outputName <- reactiveVal(NULL)

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
                categoryCardSchade(output = "countYearProvinceUI",
                  outputFunction = "countYearProvinceUI-schade")
              )
            )
            outputName("")
          },
          regio = {
            output$`output-regio` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardSchade(output = "mapFlandersUI")
              )
            )
            outputName("")
          },
          type = {
            output$`output-type` <- renderUI(          
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
              )
            )   
            outputName("")
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
            outputName("")
          },
          kosten = {
            output$`output-kosten` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardSchade(output = "barCostUI")
              )
            )
            outputName("")
          }
        )
        
      }
      
      outputCreated("")# reset path
      initTab(FALSE)
      
    })
  
    ## Tab content with selected plot/table

    outputUI <- reactiveVal(NULL)
    outputServer <- reactiveVal(NULL)
    
    # if plot is selected in the 'all' tab
    observeEvent(input$subcategory, 
      if(input$subcategory %in% schadeOutputs)
        outputUI(input$subcategory)
    )
    
    # if plot is selected based on the category cards
    observeEvent(input$`tableSchadeSummaryUI-button`, outputUI("tableSchadeSummaryUI"))
    observeEvent(input$`trendYearFlandersUI-button`, outputUI("trendYearFlandersUI"))
    observeEvent(input$`countYearProvinceUI-button`, outputUI("countYearProvinceUI-schade"))
    observeEvent(input$`mapFlandersUI-button`, outputUI("mapFlandersUI-schade"))
    observeEvent(input$`countYearSchadeUI-wildschade-button`, outputUI("countYearSchadeUI-wildschade"))
    observeEvent(input$`mapSchadeUI-wildschade-button`, outputUI("mapSchadeUI-wildschade"))
    observeEvent(input$`tableSchadeUI-button`, outputUI("tableSchadeUI"))
    observeEvent(input$`countYearSchadeUI-gewas-button`, outputUI("countYearSchadeUI-gewas"))
    observeEvent(input$`tableGewasUI-button`, outputUI("tableGewasUI"))
    observeEvent(input$`countYearSchadeUI-seizoen-button`, outputUI("countYearSchadeUI-seizoen"))
    observeEvent(input$`mapSchadeUI-seizoen-button`, outputUI("mapSchadeUI-seizoen"))
    observeEvent(input$`barCostUI-button`, outputUI("barCostUI"))
    
    # Create plot - UI side
    observeEvent(outputUI(), ignoreNULL = TRUE, {
          
      plotName <- outputUI()
          
      # create the plot/table
      switch(plotName, 
        "tableSchadeSummaryUI" = {
          plot <- tableSchadeSummaryUI(
            id = ns(plotName), 
            uiText = uiText, specie = results$specie()
          )
          card <- "output-vlaanderen"
        },
        "trendYearFlandersUI" = {
          plot <- trendYearFlandersUI(
            id = ns(plotName),
            type = "wildschade",
            includeOptions = TRUE,
            uiText = uiText, specie = results$specie()
          )
          card <- "output-vlaanderen"
        },
        "countYearProvinceUI-schade" = {
          plot <- countYearProvinceUI(
            id = ns(plotName), 
            uiText = uiText, type = "schade",
            specie = results$specie(),
            showType = TRUE, doHide = FALSE,
            showDataSource = "schade",
            plotFunction = "countYearProvinceUI-schade"
          )
          card <- "output-vlaanderen"
        },
        "mapFlandersUI-schade" = {
          plot <- mapFlandersUI(
            id = ns(plotName), 
            type = "wildschade", plotDetails = "region",
            showCombine = FALSE,
            uiText = uiText, outputFunction = "mapFlandersUI-schade",
            specie = results$specie(), typeTitle = "schade"
          )
          card <- "output-regio"
        },
        "countYearSchadeUI-wildschade" = {
          plot <- countYearSchadeUI(
            id = ns(plotName), 
            doHide = FALSE,
            uiText = uiText, context = "description",
            type = "schade", specie = results$specie()
          )
          card <- "output-type"
        },
        "mapSchadeUI-wildschade" = {
          plot <- mapSchadeUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            type = "schade", specie = results$specie(),
            filterVariable = FALSE
          )
          card <- "output-type"
        },
        "tableSchadeUI" = {
          tableSchadeUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-type"
        },
        "countYearSchadeUI-gewas" = {
          plot <- countYearSchadeUI(
            id = ns(plotName), 
            doHide = FALSE,
            uiText = uiText, context = "description",
            type = "gewas", specie = results$specie()
          )
          card <- "output-type"
        },
        "tableGewasUI" = {
          plot <- tableGewasUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-type"
        },
        "countYearSchadeUI-seizoen" = {
          plot <- countYearSchadeUI(
            id = ns(plotName), 
            doHide = FALSE,
            uiText = uiText, context = "description",
            type = "seizoen", specie = results$specie(),
            regionLevels = c(1:2, 4)
          )
          card <- "output-seizoen"
        },
        "mapSchadeUI-seizoen" = {
          plot <- mapSchadeUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            type = "schade", specie = results$specie(),
            filterVariable = FALSE
          )
          card <- "output-seizoen"
        },
        # dash plot F09_2
        "barCostUI" = {
          plot <- barCostUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            typeMelding = c("Landbouw" = "landbouw"),
            regionLevels = c(1:2, 4),
            doHide = FALSE
          )
          card <- "output-kosten"
        }
        
      )
          
      # include plot/table in UI
      cnt <- ifelse(
        input$subcategory %in% schadeOutputs,
        paste0("plots-", plotName),
        card
      )
      output[[cnt]] <- renderUI(plot)
         
      # re-set in case plot selected via tab after/before category card
      outputUI(NULL)
          
      # activate server-side update
      outputServer(plotName)
          
    })

    # Update plot in path
    observeEvent(outputName(), ignoreNULL = TRUE,
      output$pathPlot <- renderText(
        if(outputName() == ""){
          ""
        }else{
          getOutputTitle(
            output = sub("(.+)(-[[:alnum:]]{1,})$", "\\1", outputName()), 
            uiText = uiText, specie = results$specie(), type = "schade",
            n = 55
          )
        }
      )
    )

    # Server
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      plotName <- outputServer()
      
      switch(plotName,
        "tableSchadeSummaryUI" = tableSchadeSummaryServer(
          id = plotName, 
          data = results$schade_data, 
          schadeTypes = schadeTypes, schadeCodes = schadeCodes
        ),
        "trendYearFlandersUI" = trendYearFlandersServer(
          id = plotName, 
          geoData = results$schade_data, 
          allSpatialData = spatialData, 
          biotoopData = biotoopData, 
          species = results$specie,
          type = "wildschade",
          includeOptions = TRUE
        ),
        "countYearProvinceUI-schade" = countYearProvinceServer(
          id = plotName,
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
          id = plotName,
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
          id = plotName,
          data = results$schade_data,
          type = "schadeCode", 
          timeRange = results$schade_timeRange,
          fullNames = schadeCodes
        ),
        "mapSchadeUI-wildschade" = mapSchadeServer(
          id = plotName, 
          schadeData = results$schade_data,
          allSpatialData = reactive(spatialData),
          timeRange = results$schade_timeRange,
          defaultYear = defaultYear,
          species = results$specie,
          borderRegion = "provinces",
          variable = "schadeCode"
        ),
        "tableSchadeUI" = tableSchadeServer(
          id = plotName,  
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
          id = plotName,
          data = results$schade_data,
          type = "SoortNaam", 
          timeRange = results$schade_timeRange,
          fullNames = schadeCodes
        ),
        "tableGewasUI" = tableGewasServer(
          id = plotName,
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
          id = plotName,
          data = results$schade_data,
          type = "season", 
          timeRange = results$schade_timeRange
        ),
        "mapSchadeUI-seizoen" = mapSchadeServer(
          id = plotName, 
          schadeData = results$schade_data,
          allSpatialData = reactive(spatialData),
          timeRange = results$schade_timeRange,
          defaultYear = defaultYear,
          species = results$specie,
          borderRegion = "provinces",
          variable = "season"
        ),
        "barCostUI" = barCostServer(
          id = plotName,
          data = results$schade_data,
          yVar = "schadeBedrag"
        )  
      )
      outputName(plotName)
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })
      
    ## Output
      
    # Redirection:

    observeEvent(input$`pathHome`, {
      print("schade: Go to home page")
      nextPage("home")
    })
  
    observeEvent(input$`pathSpecie-button`, {
      print("schade: Go to specie page")
      nextPage(structure("specie", specie = results$specie()))
    })
      
    return(nextPage)

  })
  
}

#' Wrapper for the sidebar of the 'schade' Category page
#' @inheritParams categorySidebarPanel
#' @inherit categorySidebarPanel return
#' @author lcougnaud
schadePanel <- function(id, specie, ...){
  
  ns <- NS(namespace = id)
  
  categorySidebarPanel(
    id = id, specie = specie,
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
      uiOutput(ns("schade_warning"))
    ),
    ...
  )
  
}