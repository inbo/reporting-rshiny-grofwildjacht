# Server file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################


everEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
everGeoData <- geoData[geoData$wildsoort == "Wild zwijn", ]
everSchadeData <- schadeData[schadeData$wildsoort == "Wild zwijn", ]

draagkrachtDir <- system.file("extdata", "maatschappelijke_draagkracht", package = "reportingGrofwild")
inschattingData <- fread(file.path(draagkrachtDir, "Data_inschatting.csv"))


results$dash_species <- reactive("Wild zwijn")



## INDICATOR SELECTION ##

output$dash_populatieIndicatoren <- reactive({
    tmp_populatie <- dashboardChoicesServer(
      id = "dash_populatie", 
      choices = populatieChoices,
      uiText = uiText, 
      regionLevel = reactive(input$dash_regionLevel)
    )
    if (is.null(tmp_populatie()))
      "" else
      tmp_populatie()
  })
outputOptions(output, "dash_populatieIndicatoren", suspendWhenHidden = FALSE)


output$dash_jachtIndicatoren <- reactive({
    
    tmp_jacht <- dashboardChoicesServer(
      id = "dash_jacht",
      choices = jachtChoices,
      uiText = uiText,
      regionLevel = reactive(input$dash_regionLevel)
    )
    if (is.null(tmp_jacht()))
      "" else 
      tmp_jacht()
    
  })    
outputOptions(output, "dash_jachtIndicatoren", suspendWhenHidden = FALSE)


output$dash_verkeerIndicatoren <- reactive({
    
    tmp_verkeer <- dashboardChoicesServer(
      id = "dash_verkeer",
      choices = verkeerChoices,
      uiText = uiText,
      regionLevel = reactive(input$dash_regionLevel)
    )
    if (is.null(tmp_verkeer()))
      "" else 
      tmp_verkeer()
    
  })
outputOptions(output, "dash_verkeerIndicatoren", suspendWhenHidden = FALSE)


output$dash_landbouwIndicatoren <- reactive({
    
    tmp_landbouw <- dashboardChoicesServer(
      id = "dash_landbouw",
      choices = landbouwChoices, 
      uiText = uiText,
      regionLevel = reactive(input$dash_regionLevel)
    )
    if (is.null(tmp_landbouw()))
      "" else 
      tmp_landbouw()
    
  })
outputOptions(output, "dash_landbouwIndicatoren", suspendWhenHidden = FALSE)


output$dash_priveIndicatoren <- reactive({
    
    tmp_prive <- dashboardChoicesServer(
      id = "dash_prive",
      choices = priveChoices, 
      uiText = uiText,
      regionLevel = reactive(input$dash_regionLevel)
    )
    if (is.null(tmp_prive()))
      "" else 
      tmp_prive()
    
  })
outputOptions(output, "dash_priveIndicatoren", suspendWhenHidden = FALSE)


output$dash_maatschappijIndicatoren <- reactive({
    
    tmp_maatschappij <- dashboardChoicesServer(
      id = "dash_maatschappij",
      choices = maatschappijChoices, 
      uiText = uiText,
      regionLevel = reactive(req(input$dash_regionLevel))
    )
    if (is.null(tmp_maatschappij()))
      "" else 
      tmp_maatschappij()
    
  })    
outputOptions(output, "dash_maatschappijIndicatoren", suspendWhenHidden = FALSE)


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
    
    if (input$dash_regionLevel == "flanders")
      return(NULL)
    
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

results$dash_schadeData <- reactive({
    
    if (input$dash_regionLevel != "flanders") {
      
      validate(need(input$dash_locaties, "Gelieve regio('s) te selecteren"))
      
      filterVariable <- switch(input$dash_regionLevel,
        "provinces" = "provincie", 
        "faunabeheerzones" = "FaunabeheerZone",
        "communes" = "gemeente_afschot_locatie")
      
      everSchadeData[everSchadeData[[filterVariable]] %in% input$dash_locaties, ]
      
    } else everSchadeData
    
  })



## SUBMIT & DOWNLOAD ##

observeEvent(input$dash_submit, {
    
    
    
  })


## MAP ##

mapFlandersServer(id = "dash_background",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "empty",
  regionLevel = reactive(req(input$dash_regionLevel)),
  locaties = reactive({
      req(input$dash_regionLevel)
      if (input$dash_regionLevel != "flanders")
        req(input$dash_locaties)
      input$dash_locaties
    }),
  geoData = reactive(everGeoData),
  biotoopData = biotoopData,
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE)


## PLOTS ##


results$dash_timeRange <- reactive(range(everEcoData$afschotjaar))


# Populatie 

countAgeGroupServer(
  id = "dash_reproductie",
  data = reactive({
      plotData <- everEcoData[everEcoData$geslacht_comp == "Vrouwelijk", ]
      plotData$reproductiestatus <- ifelse(is.na(plotData$aantal_embryos), "Onbekend",
        ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig"))
      plotData
    }),
  timeRange = results$dash_timeRange,
  groupVariable = "reproductiestatus"
)


mapFlandersServer(id = "F17_1",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "grofwild",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  geoData = reactive(everGeoData),
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE)


mapFlandersServer(id = "F17_2",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "grofwild",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  geoData = reactive({
      df <- fread(file.path(dataDir, "waarnemingen_2018.csv"))
      df$wildsoort <- "Wild zwijn"
      df}),
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE,
  countVariable = "aantal")


mapSpreadServer(id = "dash_F17_4",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  allSpatialData = spatialData,
  type = "F17_4"
) 

barDraagkrachtServer(id = "dash_F18_1",
  data = reactive(inschattingData[Vraag == "populatie_evolutie", ]),
  yVar = "Vraag")


# Jacht

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


# Verkeer

mapSpreadServer(id = "dash_F06_1",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  allSpatialData = spatialData,
  type = "F06"
) 

barCostServer(id = "dash_F07_1",
  data = reactive(subset(results$dash_schadeData()@data, typeMelding %in% "verkeersongeluk")),
  yVar = "count"
)

barDraagkrachtServer(id = "dash_F07_3", 
  data = reactive(inschattingData[Vraag != "populatie_evolutie", ]), 
  yVar = "Vraag"
)


# Landbouw

barCostServer(id = "dash_F09_1",
  data = reactive(subset(results$dash_schadeData()@data, typeMelding %in% "landbouw")),
  yVar = "count"
)

barCostServer(id = "dash_F09_2",
  data = reactive(results$dash_schadeData()@data),
  yVar = "schadeBedrag"
)

barDraagkrachtServer(id = "dash_F09_3", 
  data = reactive(inschattingData[Vraag != "populatie_evolutie", ]), 
  yVar = "Vraag"
)


# Prive/Publiek

barCostServer(id = "dash_F11_1",
  data = reactive(subset(results$dash_schadeData()@data, 
      typeMelding %in% "private en publieke gebieden")),
  yVar = "count"
)

barDraagkrachtServer(id = "dash_F11_3", 
  data = reactive(inschattingData[Vraag != "populatie_evolutie", ]), 
  yVar = "Vraag"
)


# Maatschappelijke draagkracht

barDraagkrachtServer(id = "dash_F12_1",
  data = reactive(fread(file.path(draagkrachtDir, "F12_1_data.csv"))),
  xVar = "Jaar", yVar = "Aantal")

barDraagkrachtServer(id = "dash_F14_1",
  data = reactive(fread(file.path(draagkrachtDir, "F14_1_data.csv"))),
  groupVariable = "Year", yVar = "Sector")

barDraagkrachtServer(id = "dash_F14_2",
  data = reactive(fread(file.path(draagkrachtDir, "F14_2_data.csv"))),
  groupVariable = "Year", yVar = "Sector")

barDraagkrachtServer(id = "dash_F14_3",
  data = reactive(fread(file.path(draagkrachtDir, "F14_3_data.csv"))),
  groupVariable = "Sector", yVar = "Question_label")

barDraagkrachtServer(id = "dash_F14_4",
  data = reactive(fread(file.path(draagkrachtDir, "F14_4_data.csv"))),
  groupVariable = c("Groep", "Year"), yVar = "Question_label")

barDraagkrachtServer(id = "dash_F14_5",
  data = reactive(fread(file.path(draagkrachtDir, "F14_5_data.csv"))),
  groupVariable = "Sector", yVar = "Question_label")
