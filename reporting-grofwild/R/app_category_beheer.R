
#' Server function for an output (plot/table) of the 'beheer' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @import data.table
#' @author lcougnaud
#' @export
beheerOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal(),
  outputs = character(),
  ecoData, geoData, openingstijdenData, spatialData, biotoopData,
  defaultYear,
  uiText){
  
  # For R CMD check
  afschotplan_nummer <- afschot_datum <- provincie <- FaunabeheerZone <- NULL
  wildsoort <- afschotjaar <- . <- NULL  
  
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
      drukjachtData <- as.data.table(merge(
          x = results$ecoData()[
            results$ecoData()$jachtmethode_comp %in% "Drukjacht", 
            c("ID", "afschot_datum", "afschotjaar", "provincie", "wildsoort")
          ], 
          y = results$geoData()[, c("ID", colsGeo)], 
          by = "ID", all.x = TRUE
        ))
      # Keep most prevalent province/FBZ per afschotplan_nummer & date
      ## overwrite with most prevalent province/FBZ
      drukjachtData <- drukjachtData[,':='(
          provincie = names(which.max(table(provincie))), 
          FaunabeheerZone = names(which.max(table(FaunabeheerZone)))),
        by = c("afschotplan_nummer", "afschot_datum")]
      drukjachtData <- unique(drukjachtData, by = c("afschotplan_nummer", "afschot_datum"))
      drukjachtData[, .(afschotplan_nummer, afschot_datum, provincie, FaunabeheerZone, wildsoort, afschotjaar)]
      
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
        
        # create the plot/table
        ui <- switch(plot(), 
          "trendYearFlandersUI" = {
            trendYearFlandersUI(
              id = ns("plot")
            )
           },
          "countYearProvinceUI-afschot" = {
            countYearProvinceUI(
              id = ns("plot"), 
              uiText = uiText, 
              plotFunction = "countYearProvinceUI-afschot",
              specie = specie(),
              doHide = FALSE
            )
          },
          "yearlyShotAnimalsUI" = {
            yearlyShotAnimalsUI(
              id = ns("plot"), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "mapFlandersUI" = {
            mapFlandersUI(
              id = ns("plot"), 
              type = "beheer", plotDetails = "region"
            )
          },
          "tableProvinceUI" = {
            tableProvinceUI(
              id = ns("plot"), doHide = FALSE,
              uiText = uiText, context = "description", specie = specie()
            )
          },
          "countYearShotUI-leeftijd_comp" = {
            countYearShotUI(
              id = ns("plot"), groupVariable = "leeftijd_comp",
              regionLevels = c(1:2, 4), 
              uiText = uiText, context = "description", specie = specie(),
              doHide = FALSE
            )
          },
          "countYearShotUI-jachtmethode_comp" = {
            countYearShotUI(
              id = ns("plot"), groupVariable = "jachtmethode_comp",
              regionLevels = c(1:2, 4), 
              uiText = uiText, context = "description", specie = specie(),
              doHide = FALSE
            )
          },
          "F04_3" = {
            countYearProvinceUI(
              id = ns("plot"), 
              uiText = uiText, specie = specie(),
              plotFunction = "F04_3", 
              doHide = FALSE,
              showType = TRUE
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
        "trendYearFlandersUI" = trendYearFlandersServer(
          id = "plot", 
          geoData = results$geoData,
          allSpatialData = spatialData, 
          biotoopData = biotoopData, 
          species = specie,
          uiText = uiText
        ),
        "countYearProvinceUI-afschot" = countYearProvinceServer(
          id = "plot",
          data = results$ecoData,
          timeRange = if (id == "Edelhert")
            reactive(c(2008, max(results$ecoData()$afschotjaar))) else 
            results$timeRange
         ),
        "yearlyShotAnimalsUI" = yearlyShotAnimalsServer(
          id = "plot", 
          data = results$ecoData, 
          timeRange = results$openingstijd, 
          type = results$labeltypes, 
          openingstijdenData = results$openingstijdenData
        ),
        "mapFlandersUI" = mapFlandersServer(
          id = "plot",
          uiText = uiText,
          defaultYear = defaultYear,
          species = specie,
          type = "beheer",
          geoData = results$geoData,
          biotoopData = biotoopData,
          allSpatialData = spatialData
        ),
        "tableProvinceUI" = tableProvinceServer(
          id = "plot",
          data = results$ecoData,
          categorie = "leeftijd",
          timeRange = results$timeRange
        ),
        "countYearShotUI-leeftijd_comp" = countYearShotServer(
          id = "plot",
          data = results$combinedData,
          timeRange = results$timeRange,
          groupVariable = "leeftijd_comp",
          types = results$leeftijdtypes
        ),
        "countYearShotUI-jachtmethode_comp" = countYearShotServer(
          id = "plot",
          data = results$combinedData,
          timeRange = reactive(c(2014, results$timeRange()[2])),
          groupVariable = "jachtmethode_comp",
          types = results$jachttypes
        ),
        "F04_3" = countYearProvinceServer(
          id = "plot", 
          data = results$drukjachtData,
          types = reactive(c(
              "Vlaanderen" = "flanders",
              "Provincie" = "provinces", 
              "Faunabeheerzones" = "faunabeheerzones"
            )), 
          labelTypes = "Regio-schaal", 
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