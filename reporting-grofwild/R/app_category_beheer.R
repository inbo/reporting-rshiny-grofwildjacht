#' Server function for the cards of the 'beheer' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
beheerCardServer <- function(id, specie, subcategory){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## input
    results <- reactiveValues(renderedTabs = "Grofwild")
    results$specie <- reactive(specie)
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Main panel
    
    observe({    
          
      if(subcategory %in% subcategories){
            
            categoryCardAfschot <- function(...){
              categoryCard(
                  id = id, 
                  uiText = uiText,
                  specie = results$specie(), 
                  category = "beheer",
                  ...
              )
            }
            
            group <- strsplit(subcategory, split = "-")[[1]][2]
            
            cards <- switch(group,
                vlaanderen = 
                    bslib::layout_column_wrap(
                        width = 1/3, gap = "2em",
                        categoryCardAfschot(output = "trendYearRegionUI"),
                        categoryCardAfschot(output = "countYearProvinceUI", outputFunction = "countYearProvinceUI-afschot"),
                        categoryCardAfschot(output = "yearlyShotAnimalsUI")
                    ),
                regio =       
                    bslib::layout_column_wrap(
                        width = 1/3, gap = "2em",
                        categoryCardAfschot(output = "mapFlandersUI")
                    ),
                leeftijdcategorie = 
                    bslib::layout_column_wrap(
                        width = 1/3, gap = "2em",
                        categoryCardAfschot(output = "tableProvinceUI"),
                        categoryCardAfschot(output = "countYearShotUI-leeftijd_comp")
                    ),
                jachtmethode =     
                    bslib::layout_column_wrap(
                        width = 1/3, gap = "2em",
                        categoryCardAfschot(output = "countYearShotUI-jachtmethode_comp"),
                        categoryCardAfschot(output = "F04_3")
                    )
            )
            
            output[["output"]] <- renderUI(cards)
            
          }
          
        })
        
  })
  
}

#' Server function for an output (plot/table) of the 'beheer' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
beheerOutputServer <- function(id, specie, plot){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    beheerOutputs <- getOutputs(category = "beheer")
    
    ## input
    results <- reactiveValues(renderedTabs = "Grofwild")
    
    results$specie <- reactive(specie)
    
    results$ecoData <- reactive(
      ecoData[ecoData$wildsoort == results$specie(), ]
    )
    results$geoData <- reactive({
      req(geoData)
      geoData[geoData$wildsoort == results$specie(), ]
    })
    
    results$timeRange <- reactive(range(results$ecoData()$afschotjaar))
    results$openingstijdenData <- reactive(
      openingstijdenData[openingstijdenData$Soort == results$specie(), ]
    )
    results$openingstijd <- reactive({
      # for Ree: openingseason contains more year than in the data
      # for Wildboar: openingseason contains less year than in the data
          
      # so retains the years when data and opening season specified
      # and doesn't retain the last year (because not full)
              
      if (results$specie() %in% c("Ree", "Wild zwijn")) {
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
      types <- loadMetaEco(species = results$specie())$labeltype
      if (length(types) == 1 && results$specie() == types)
        return(c("alle" = "all")) else 
        return(types)
    })
    
    results$leeftijdtypes <- reactive(
      c(loadMetaEco(species = results$specie())$leeftijd_comp_inbo, "Onbekend")
    )
    
    results$jachttypes <- reactive({
      choices <- unique(results$combinedData()$jachtmethode_comp)
      if (any(is.na(choices)))
        choices[is.na(choices)] <- "onbekend"
      sort(choices)
    })

    results$drukjachtData <- reactive({
      colsGeo <- c("WBE_Naam_Toek", "postcode_afschot_locatie", 
        "FaunabeheerZone", "gemeente_afschot_locatie"
      )
      drukjachtData <- merge(
        x = results$ecoData()[
          results$ecoData()$jachtmethode_comp == "Drukjacht", 
          c("ID", "afschot_datum", "afschotjaar", "provincie", "wildsoort")
        ], 
        y = results$geoData()[, c("ID", colsGeo)], 
        by = "ID", all.x = TRUE
      )
      cols <- c("afschot_datum", "afschotjaar", colsGeo, 
        "provincie", "wildsoort")
      drukjachtData <- drukjachtData[, cols]
      # Keep unique records per WBE & date
      drukjachtData <- drukjachtData[!duplicated(drukjachtData), ]
      validate(need(nrow(drukjachtData) > 0, "Geen data beschikbaar"))
      return(drukjachtData)
    })

    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Main panel

    # Tab content with selected plot/table
    
    outputServer <- reactiveVal(NULL)

#    # if plot is selected based on the category cards
#    observeEvent(input$`trendYearRegionUI-button`, outputUI("trendYearRegionUI"))
#    observeEvent(input$`countYearProvinceUI-button`, outputUI("countYearProvinceUI-afschot"))
#    observeEvent(input$`yearlyShotAnimalsUI-button`, outputUI("yearlyShotAnimalsUI"))
#    observeEvent(input$`mapFlandersUI-button`, outputUI("mapFlandersUI"))
#    observeEvent(input$`tableProvinceUI-button`, outputUI("tableProvinceUI"))
#    observeEvent(input$`countYearShotUI-leeftijd_comp-button`, outputUI("countYearShotUI-leeftijd_comp"))
#    observeEvent(input$`countYearShotUI-jachtmethode_comp-button`, outputUI("countYearShotUI-jachtmethode_comp"))
#    observeEvent(input$`F04_3-button`, outputUI("F04_3"))

    # Create plot - UI side
    observe({
          
      if(plot %in% beheerOutputs){
        
        outputName <- plot
        
        # create the plot/table
        ui <- switch(outputName, 
          "trendYearRegionUI" = {
            trendYearRegionUI(
              id = ns(outputName),
              uiText = uiText, context = "description", specie = results$specie(),
              showCombinatie = TRUE,
              doHide = FALSE
            )
           },
          "countYearProvinceUI-afschot" = {
            countYearProvinceUI(
              id = ns(outputName), 
              uiText = uiText, context = "description", type = "afschot",
              specie = results$specie(),
              doHide = FALSE,
              plotFunction = "countYearProvinceUI-afschot"
            )
          },
          "yearlyShotAnimalsUI" = {
            yearlyShotAnimalsUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
              doHide = FALSE
            )
          },
          "mapFlandersUI" = {
            mapFlandersUI(
              id = ns(outputName), 
              type = "grofwild", plotDetails = "region",
              uiText = uiText, specie = results$specie(), 
              typeTitle = "afschot"
            )
          },
          "tableProvinceUI" = {
            tableProvinceUI(
              id = ns(outputName), doHide = FALSE,
              uiText = uiText, context = "description", specie = results$specie()
            )
          },
          "countYearShotUI-leeftijd_comp" = {
            countYearShotUI(
              id = ns(outputName), groupVariable = "leeftijd_comp",
              regionLevels = c(1:2, 4), 
              uiText = uiText, context = "description", specie = results$specie(),
              doHide = FALSE
            )
          },
          "countYearShotUI-jachtmethode_comp" = {
            countYearShotUI(
              id = ns(outputName), groupVariable = "jachtmethode_comp",
              regionLevels = c(1:2, 4), 
              uiText = uiText, context = "description", specie = results$specie(),
              doHide = FALSE
            )
          },
          "F04_3" = {
            countYearProvinceUI(
              id = ns(outputName), 
              uiText = uiText, context = "description", specie = results$specie(),
              plotFunction = "F04_3", 
              doHide = FALSE,
              regionLevels = 1:4
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
        "trendYearRegionUI" = trendYearRegionServer(
          id = outputName, 
          data = results$ecoData, 
          species = results$specie,
          timeRange = results$timeRange,
          regionLevel = reactive("flanders"),
          locaties = reactive("Vlaams Gewest"),
          geoData = results$geoData,
          allSpatialData = spatialData,
          biotoopData = reactive(biotoopData[["flanders"]])
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
          species = results$specie,
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
          timeRange = reactive(range(results$drukjachtData()$afschotjaar))
        )
      )
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })

  })
  
}