#' Server function for the cards of the 'beheer' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return named list with plot, reactive with name of output plot/table (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
beheerCardServer <- function(id, 
  specie, subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
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
            category = "beheer"
          )
          
          if(output == "countYearProvinceUI-afschot"){
            args[["output"]] <- "countYearProvinceUI"
            args[["outputFunction"]] <- output
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
      if(output == "countYearProvinceUI-afschot")
        btn <- "countYearProvinceUI-button"
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

#' Server function for an output (plot/table) of the 'beheer' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export
beheerOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal(),
  outputs = character(),
  ecoData, geoData, openingstijdenData, spatialData, biotoopData,
  defaultYear,
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## input
    results <- reactiveValues()
    
    results$ecoData <- reactive(
      ecoData[which(ecoData$wildsoort == specie()), ]
    )
    results$geoData <- reactive({
      req(geoData)
      geoData[which(geoData$wildsoort == specie()), ]
    })
    
    results$timeRange <- reactive(range(results$ecoData()$afschotjaar))
    results$openingstijdenData <- reactive(
      openingstijdenData[openingstijdenData$Soort == specie(), ]
    )
    results$openingstijd <- reactive({
      # for Ree: openingseason contains more year than in the data
      # for Wildboar: openingseason contains less year than in the data
          
      # so retains the years when data and opening season specified
      # and doesn't retain the last year (because not full)
              
      if (specie() %in% c("Ree", "Wild zwijn")) {
        openingstijd <- c(
          max(
            min(results$ecoData()$afschotjaar), 
            min(results$openingstijdenData()$Jaar)
          ),
          min(
            max(results$ecoData()$afschotjaar), 
            max(results$openingstijdenData()$Jaar)
          )
        )
      }
    })

    # Enrich data with FBZ
    results$combinedData <- reactive(
      merge(
        x = results$ecoData(), 
        y = results$geoData()[, c("ID", "FaunabeheerZone")], 
        by = "ID"
      )
    )

    # Plot: Percentage jaarlijkse afschot
    results$labeltypes <- reactive({
      req(results$openingstijdenData())
      types <- loadMetaEco(species = specie())$labeltype
      if (length(types) == 1 && specie() == types)
        return(c("alle" = "all")) else 
        return(types)
    })
    
    results$leeftijdtypes <- reactive(
      c(loadMetaEco(species = specie())$leeftijd_comp_inbo, "Onbekend")
    )
    
    results$jachttypes <- reactive({
      choices <- unique(results$combinedData()$jachtmethode_comp)
      if (any(is.na(choices)))
        choices[is.na(choices)] <- "onbekend"
      sort(choices)
    })

    results$drukjachtData <- reactive({
      colsGeo <- c("afschotplan_nummer", "postcode_afschot_locatie", 
        "FaunabeheerZone", "gemeente_afschot_locatie"
      )
      drukjachtData <- merge(
        x = results$ecoData()[
          results$ecoData()$jachtmethode_comp %in% "Drukjacht", 
          c("ID", "afschot_datum", "afschotjaar", "provincie", "wildsoort")
        ], 
        y = results$geoData()[, c("ID", colsGeo)], 
        by = "ID", all.x = TRUE
      )
      # Keep unique records per afschotplan_nummer & date
      drukjachtData <- drukjachtData[!duplicated(drukjachtData[, c("afschotplan_nummer", "afschot_datum")]), ]
      validate(need(nrow(drukjachtData) > 0, "Geen data beschikbaar"))
      return(drukjachtData)
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
          "trendYearFlandersUI" = {
            trendYearFlandersUI(
              id = ns(outputName),
              uiText = uiText, specie = specie()
            )
           },
          "countYearProvinceUI-afschot" = {
            countYearProvinceUI(
              id = ns(outputName), 
              uiText = uiText, context = "description", type = "afschot",
              specie = specie(),
              doHide = FALSE,
              plotFunction = "countYearProvinceUI-afschot"
            )
          },
          "yearlyShotAnimalsUI" = {
            yearlyShotAnimalsUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "mapFlandersUI" = {
            mapFlandersUI(
              id = ns(outputName), 
              type = "grofwild", plotDetails = "region",
              uiText = uiText, specie = specie(), 
              typeTitle = "afschot"
            )
          },
          "tableProvinceUI" = {
            tableProvinceUI(
              id = ns(outputName), doHide = FALSE,
              uiText = uiText, context = "description", specie = specie()
            )
          },
          "countYearShotUI-leeftijd_comp" = {
            countYearShotUI(
              id = ns(outputName), groupVariable = "leeftijd_comp",
              regionLevels = c(1:2, 4), 
              uiText = uiText, context = "description", specie = specie(),
              doHide = FALSE
            )
          },
          "countYearShotUI-jachtmethode_comp" = {
            countYearShotUI(
              id = ns(outputName), groupVariable = "jachtmethode_comp",
              regionLevels = c(1:2, 4), 
              uiText = uiText, context = "description", specie = specie(),
              doHide = FALSE
            )
          },
          "F04_3" = {
            countYearProvinceUI(
              id = ns(outputName), 
              uiText = uiText, context = "description", specie = specie(),
              plotFunction = "F04_3", 
              doHide = FALSE,
              showType = TRUE
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
        "trendYearFlandersUI" = trendYearFlandersServer(
          id = outputName, 
          geoData = results$geoData,
          allSpatialData = spatialData, 
          biotoopData = biotoopData, 
          species = specie
        ),
        "countYearProvinceUI-afschot" = countYearProvinceServer(
          id = outputName,
          data = results$ecoData,
          timeRange = if (id == "Edelhert")
            reactive(c(2008, max(results$ecoData()$afschotjaar))) else 
            results$timeRange
         ),
        "yearlyShotAnimalsUI" = yearlyShotAnimalsServer(
          id = outputName, 
          data = results$ecoData, 
          timeRange = results$openingstijd, 
          type = results$labeltypes, 
          openingstijdenData = results$openingstijdenData
        ),
        "mapFlandersUI" = mapFlandersServer(
          id = outputName,
          uiText = uiText,
          defaultYear = defaultYear,
          species = specie,
          type = "grofwild",
          geoData = results$geoData,
          biotoopData = biotoopData,
          allSpatialData = spatialData
        ),
        "tableProvinceUI" = tableProvinceServer(
          id = outputName,
          data = results$ecoData,
          categorie = "leeftijd",
          timeRange = results$timeRange
        ),
        "countYearShotUI-leeftijd_comp" = countYearShotServer(
          id = outputName,
          data = results$combinedData,
          timeRange = results$timeRange,
          groupVariable = "leeftijd_comp",
          types = results$leeftijdtypes
        ),
        "countYearShotUI-jachtmethode_comp" = countYearShotServer(
          id = outputName,
          data = results$combinedData,
          timeRange = reactive(c(2014, results$timeRange()[2])),
          groupVariable = "jachtmethode_comp",
          types = results$jachttypes
        ),
        "F04_3" = countYearProvinceServer(
          id = outputName, 
          data = results$drukjachtData,
          types = reactive(c(
              "Vlaanderen" = "flanders",
              "Provincie" = "provinces", 
              "Faunabeheerzones" = "faunabeheerzones"
            )), 
          labelTypes = "Regio", 
          typesDefault = reactive("provinces"), 
          
          timeRange = reactive(range(results$drukjachtData()$afschotjaar, na.rm = TRUE))
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