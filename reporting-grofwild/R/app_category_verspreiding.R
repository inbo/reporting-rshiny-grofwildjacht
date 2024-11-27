verspreidingOutputs <- c("mapFlandersUI", "mapSpreadUI")

#' UI for the 'verspreiding' Category page
#' @param id id character, module id
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingUI <- function(id, specie){
  
  ns <- NS(namespace = id)
      
  verticalLayout(
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-verspreiding-header.png", width = "100%")
      )
    ),
           
    # navigation page with plots and specie sidebar panel
    sidebarLayout(
          
      sidebarPanel = specieSidebarUI(id = ns("sidebar"), specie = specie),
      
      mainPanel = mainPanel(
          
        navbarPage(
            
          title = "",
          
          id = ns("subcategory"),
          
          selected = "informatie",
                
          tabPanel(
            title = getTabTitle(
              value = "huidig", category = "verspreiding"
            ), 
            value = "huidig",
            uiOutput(outputId = ns("output-huidig"))
          ),
          
          tabPanel(
            title = getTabTitle(
              value = "toekomstig", category = "verspreiding"
            ), 
            value = "toekomstig",
            uiOutput(outputId = ns("output-toekomstig"))
          ),
          # menu with all plots/tables
          tabPanelAll(
            category = "verspreiding", id = id,
            outputs = verspreidingOutputs, 
            uiText = uiText
          ),
          tabPanelInformatie(
            category = "verspreiding", id = id, 
            uiText = uiText, 
            maxDate = max(ecoData$afschot_datum, na.rm = TRUE),
            specie = specie
          )
        )
      )
    )
  )
  
}

#' Server function for the 'verspreiding' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingServer <- function(id, specie){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    outputName <- reactiveVal(NULL)
    nextPage <- reactiveVal(value = NULL)
    
    ## input
    results <- reactiveValues(renderedTabs = "Verspreiding")
    
    results$specie <- reactive(specie)
    
    # Create data upon user choices
    results$spatialData <- reactive({
      req(spatialData)
      filterSpatial(
        allSpatialData = spatialData, 
        species = results$specie(), 
#        regionLevel = req(input$dash_regionLevel), 
        year = NULL
      )
    })

    # F17_1 plot
    results$geoData <- reactive({
      req(geoData)
      subset(geoData, wildsoort == results$specie())
    })
        
    waarnemingenData <- loadRawData(type = "waarnemingen")
    # Restrict all to same date
    waarnemingenData <- waarnemingenData[
      waarnemingenData$afschotjaar <= 
        format(max(ecoData$afschot_datum, na.rm = TRUE), "%Y"), 
    ]

    # Combine waarnemingen.be & afschot
    results$geoDataAll <- reactive({
      rbind(
        # waarnemingen
        data.table::as.data.table(waarnemingenData),
        # afschot
        results$geoData(),
        fill = TRUE
      )
    })
    
    ## Sidebar with input parameters
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Tab with available plots
    
    initTab <- reactiveVal(TRUE)
    # Go back to page if subcategory is clicked on in the path
    observeEvent(input$`pathSubcategory-button`, initTab(TRUE))
    observeEvent(input$subcategory, initTab(TRUE))

    # Create tab
    observe(if(initTab()){

      if(isTruthy(input$subcategory)){
        
        categoryCardVerspreiding <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "verspreiding", 
            ...
          )
        }
        
        switch(input$subcategory, 
            
          huidig = {
            output$`output-huidig` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardVerspreiding(
                  output = "mapFlandersUI", 
                  outputFunction = "F17_1"
                )
              )
            )
            outputName("")
          },
            
          toekomstig = {
            output$`output-toekomstig` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardVerspreiding(output = "mapSpreadUI")
              )
            )
            outputName("")
          }
        )
      }
      initTab(FALSE)
    })
  
    ## Tab content with selected plot/table

    outputUI <- reactiveVal(NULL)
    outputServer <- reactiveVal(NULL)
    
    # if plot is selected in the 'all' tab
    observeEvent(input$subcategory, 
      if(input$subcategory %in% verspreidingOutputs)
        outputUI(input$subcategory)
    )

    # if plot is selected based on the category cards
    observeEvent(input$`mapFlandersUI-button`, outputUI("mapFlandersUI"))
    observeEvent(input$`mapSpreadUI-button`, outputUI("mapSpreadUI"))
    
    # Create plot - UI side
    observeEvent(outputUI(), ignoreNULL = TRUE, {
      plotName <- outputUI()
          
      # create the plot/table
      switch(plotName, 
        "mapFlandersUI" = {
          plot <- mapFlandersUI(
            id = ns(plotName), 
            uiText = uiText, output = "F17_1", 
            specie = results$specie(),
            showCombine = FALSE, type = "dash",
            mapScaleChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"),
            showRegion = TRUE,
            regionChoices = c(
              "Vlaanderen" = "flanders",
              "Provincie" = "provinces", 
              "Faunabeheerzones" = "faunabeheerzones",
              "Gemeente" = "communes"
            ),
            unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative"),
            plotDetails = ""#, showTitle = FALSE
          )
          card <- "output-huidig"
        },
        "mapSpreadUI" = {
          plot <- mapSpreadUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-toekomstig"
        }
      )
      
      # include plot/table in UI
      cnt <- ifelse(
        input$subcategory %in% verspreidingOutputs,
        paste0("plots-", plotName),
        card
      )
      output[[cnt]] <- renderUI(plot)
      
      # re-set in case plot selected via tab after/before category card
      outputUI(NULL)
      
      # activate server-side update
      outputServer(plotName)
      
    })

    # Create plot - server side
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      plotName <- outputServer()
      
      switch(plotName,
        mapFlandersUI = mapFlandersServer(
          id = plotName,
          defaultYear = defaultYear,
          species = results$specie,
          type = "dash",
          geoData = results$geoDataAll,
          allSpatialData = spatialData,
          hideGlobeDefault = FALSE,
          countVariable = "aantal",
          sourceChoices = c("waarnemingen.be", "afschot"),
          uiText = uiText, outputFunction = "F17_1", context = "description"
        ),
        mapSpreadUI = mapSpreadServer(
          id = plotName,
          allSpatialData = spatialData,
          species = results$specie(),
          type = "F17_4"
        )
      )
      outputName(plotName)
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })
      
    ## Output
      
    # Redirection:

    observeEvent(input$`pathHome`, {
      print("verspreiding: Go to home page")
      nextPage("home")
    })
  
    observeEvent(input$`pathSpecie-button`, {
      print("verspreiding: Go to specie page")
      nextPage(structure("specie", specie = results$specie()))
    })
      
    return(nextPage)

  })
  
}