#' Server function for the cards of the 'verspreiding' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return named list with plot, reactive with name of output plot/table (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText){
  
  moduleServer(id, function(input, output, session){  
      
    ns <- session$ns
       
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
      
    ## Main panel
      
    # Create tab
    observe({
          
      if(subcategory() %in% subcategories){
        
        categoryCards <- lapply(outputs, function(output){
              
          args <- list(
            output = output,
            id = id, 
            uiText = uiText,
            specie = specie(), 
            category = "verspreiding"
          )
          
          if(output == "F17_1"){
            args[["output"]] <- "mapFlandersUI"
            args[["outputFunction"]] <- "F17_1"
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
      # exception
      if(output == "F17_1")  btn <- "mapFlandersUI-button"
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


#' Server function for an output (plot/table) of the 'verspreiding' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal(),
  outputs = character(),
  ecoData, geoData, spatialData, waarnemingenData, biotoopData,
  defaultYear,
  uiText){
  
  # For R CMD check
  wildsoort <- NULL
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## input
    results <- reactiveValues()
        
    # F17_1 plot
    results$geoData <- reactive({
      req(geoData)
      geoData[which(geoData$wildsoort == specie()), ]
    })
        
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
      
      if(plot() %in% outputs){
        
        outputName <- plot()
        
        # create the plot/table
        ui <- switch(outputName, 
          "F17_1" = {
            mapFlandersUI(
              id = ns(outputName), 
              uiText = uiText, outputFunction = "F17_1", 
              specie = specie(),
              showCombine = FALSE, type = "dash",
              mapScaleChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"),
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
            if (specie() == "Wild zwijn")
              mapSpreadUI(
                id = ns(outputName), 
                uiText = uiText, context = "description",
                specie = specie(),
                doHide = FALSE
              ) else 
              helpText("Geen visualisatie beschikbaar voor deze diersoort")
          },
          "kencijferUI" = {
            kencijferModuleUI(
              id = ns(outputName), 
              uiText = uiText
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
          species = specie,
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
          species = specie(),
          type = "F17_4"
        ),
        kencijferUI = kencijferModuleServer(
          id = outputName,
          kencijfersData = reactive(results$geoDataAll()[wildsoort == specie()]),
          biotoopData = reactive(biotoopData$communes),
          spatialData = spatialData,
          species = specie
        )
      )
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })

  return(list(
      specie = specie
    ))

  })
  
}