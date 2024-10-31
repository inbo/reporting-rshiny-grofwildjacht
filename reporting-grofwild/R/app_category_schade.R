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

      if(isTruthy(input$subcategory))
        switch(input$subcategory, 
            
          vlaanderen = {
            output$`output-vlaanderen` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCard(
                  id = id, specie = results$specie(), 
                  output = "tableSchadeSummaryUI", uiText = uiText
                ),
                categoryCard(
                  id = id, specie = results$specie(), 
                  output = "trendYearFlandersUI", uiText = uiText
                )
              )
            )
          }
        )
      
      initTab(FALSE)
      
    })
  
    ## Tab content with selected plot/table
    outputCreated <- reactiveVal("")

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
    observe(print(outputCreated()))
    
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
    observe(print(outputCreated()))
    
    # Update plot in path
    output$pathPlot <- renderText({
      if(isTruthy(outputCreated()))
        getOutputTitle(
          output = outputCreated(), 
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