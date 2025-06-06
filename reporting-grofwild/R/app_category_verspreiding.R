
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
        
        # create the plot/table
        ui <- switch(plot(), 
          "F17_1" = {
            mapFlandersUI(
              id = ns("plot"), 
              uiText = uiText,
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
                id = ns("plot"), 
                uiText = uiText, context = "description",
                specie = specie(),
                doHide = FALSE
              ) else 
              helpText("Geen visualisatie beschikbaar voor deze diersoort")
          },
          "kencijferUI" = {
            kencijferModuleUI(
              id = ns("plot"), 
              uiText = uiText
            )
          }
        )
      
        # include plot/table in UI
        output[["output"]] <- renderUI(ui)
        
        # activate server-side update
        outputServer(plot())
      
      }
      
    })

    # Create plot - server side
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      
      switch(outputServer(),
        `F17_1` = mapFlandersServer(
          id = "plot",
          defaultYear = defaultYear,
          species = specie,
          type = "dash",
          geoData = results$geoDataAll,
          allSpatialData = spatialData,
          hideGlobeDefault = FALSE,
          countVariable = "aantal",
          sourceChoices = c("waarnemingen.be", "afschot"),
          uiText = uiText
        ),
        mapSpreadUI = mapSpreadServer(
          id = "plot",
          allSpatialData = spatialData,
          species = specie(),
          type = "F17_4"
        ),
        kencijferUI = kencijferModuleServer(
          id = "plot",
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