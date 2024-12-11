#' Server function for the cards of the 'verspreiding' Category page
#' @param id id character, module id
#' @return reactive value with name of output plot/table (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal()){
  
  moduleServer(id, function(input, output, session){  
      
    ns <- session$ns
      
    ## input
    results <- reactiveValues(renderedTabs = "Grofwild")
    results$specie <- reactive(specie())
      
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = results$specie)
      
    ## Main panel
      
    # Create tab
    observe({
          
      if(subcategory() %in% subcategories){
        
        categoryCardVerspreiding <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "verspreiding", 
            ...
          )
        }
        
        group <- strsplit(subcategory(), split = "-")[[1]][2]
          
        cards <- switch(group,
          huidig = 
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardVerspreiding(
                output = "mapFlandersUI", 
                outputFunction = "F17_1"
              )
            ),
          toekomstig =        
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardVerspreiding(output = "mapSpreadUI")
            )
        )
        output[["output"]] <- renderUI(cards)
        
      }
    })

    # if plot is selected based on the category cards
    outputUI <- reactiveVal("Visualisatie/Tabel")
    observeEvent(input$`mapFlandersUI-button`, outputUI("mapFlandersUI"))
    observeEvent(input$`mapSpreadUI-button`, outputUI("mapSpreadUI"))
    
    return(outputUI)

  })
}


#' Server function for an output (plot/table) of the 'verspreiding' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal()){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
        
    ## initialization
    verspreidingOutputs <- getOutputs(category = "verspreiding")
    
    ## input
    results <- reactiveValues(renderedTabs = "Verspreiding")
    
    results$specie <- reactive(specie())
    
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
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Main panel
    
    # Tab content with selected plot/table
    
    outputServer <- reactiveVal(NULL)

    # Create plot - UI side
    observe({
      
      if(plot() %in% verspreidingOutputs){
        
        outputName <- plot()
        
        # create the plot/table
        ui <- switch(outputName, 
          "F17_1" = {
            mapFlandersUI(
              id = ns(outputName), 
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
          },
          "mapSpreadUI" = {
            mapSpreadUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
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

    # Create plot - server side
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      outputName <- outputServer()
      
      switch(outputName,
        `F17_1` = mapFlandersServer(
          id = outputName,
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
          id = outputName,
          allSpatialData = spatialData,
          species = results$specie(),
          type = "F17_4"
        )
      )
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })

  })
  
}