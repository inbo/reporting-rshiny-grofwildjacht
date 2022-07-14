# Server file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################


everEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
everGeoData <- geoData[geoData$wildsoort == "Wild zwijn", ]

results$dash_species <- reactive("Wild zwijn")

## FILTER ##

results$dash_spatialData <- reactive({
    
    req(spatialData)
    
    filterSpatial(
      allSpatialData = spatialData, 
      species = "Wild zwijn", 
      regionLevel = req(input$dash_regionLevel), 
      year = NULL
    )
    
  })

output$dash_region <- renderUI({
    
    req(input$dash_regionLevel != "flanders")
    
    regionChoices <- sort(unique(results$dash_spatialData()$NAAM))
    
    selectInput(inputId = "dash_locaties", label = "Regio('s)",
      choices = regionChoices,
      selected = if (doDebug) regionChoices[4:5],
      multiple = TRUE)
    
  })

results$dash_ecoData <- reactive({
    
    if (input$dash_regionLevel != "flanders") {
      
      validate(need(input$dash_locaties, "Gelieve regio('s) te selecteren"))
      
      filterVariable <- switch(input$dash_regionLevel,
        "provinces" = "provincie", 
        "faunabeheerzones" = "FaunabeheerZone",
        "communes" = "gemeente_afschot_locatie")
    
      keepIds <- everGeoData$ID[everGeoData[[filterVariable]] %in% input$dash_locaties]
      everEcoData[everEcoData$ID %in% keepIds, ]
      
    } else everEcoData
    
  })


## SUBMIT & DOWNLOAD ##

observeEvent(input$dash_submit, {
    
    
    
  })


## MAP ##

mapFlandersServer(id = "dash",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "dash",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  geoData = reactive(everGeoData),
  biotoopData = biotoopData,
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE)


## PLOTS ##


results$dash_timeRange <- reactive(range(everEcoData$afschotjaar))


# Populatie 

output$dash_populatieTitle <- renderUI({
    
    req(input$dash_populatieIndicatoren)
    
    h2(toupper("Populatie"))
    
  })

countAgeGroupServer(
  id = "dash_reproductie",
  data = reactive({
      plotData <- ecoData[ecoData$geslacht_comp == "Vrouwelijk", ]
      plotData$reproductiestatus <- ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig")
      plotData
    }),
  timeRange = results$dash_timeRange,
  groupVariable = "reproductiestatus"
)


# Jacht

output$dash_jachtTitle <- renderUI({
    
    req(input$dash_jachtIndicatoren)
    
    h2(toupper("Jacht"))
    
  })

trendYearRegionServer(id = "dash",
  data = results$dash_ecoData, 
  species = results$dash_species,
  timeRange = results$dash_timeRange,
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  geoData = reactive(everGeoData),
  allSpatialData = spatialData,
  biotoopData = biotoopData
)

countYearAgeServer(id = "dash",
  data = results$dash_ecoData,
  timeRange = results$dash_timeRange
)

