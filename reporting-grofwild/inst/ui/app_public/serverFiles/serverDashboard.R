# Server file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################


everEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
everGeoData <- geoData[geoData$wildsoort == "Wild zwijn", ]
everSchadeData <- schadeData[schadeData$wildsoort == "Wild zwijn", ]

# Combine waarnemingen.be & afschot
everWaarnemingen <- data.table::fread(file.path(dataDir, "waarnemingen_2018.csv"))[, 
  c("wildsoort", "dataSource") := list("Wild zwijn", "waarnemingen.be")]
everGeoAll <- rbind(everWaarnemingen, cbind(everGeoData, data.frame(dataSource = "afschot")), fill = TRUE)
everGeoAll$aantal[is.na(everGeoAll$aantal)] <- 1


draagkrachtDir <- system.file("extdata", "maatschappelijke_draagkracht", package = "reportingGrofwild")
inschattingData <- data.table::fread(file.path(draagkrachtDir, "Data_inschatting.csv"))


results$dash_species <- reactive("Wild zwijn")



## INDICATOR SELECTION ##

results$dash_showPopulatie <- dashboardChoicesServer(
  id = "dash_populatie", 
  choices = populatieChoices,
  uiText = uiText, 
  regionLevel = reactive(req(input$dash_regionLevel))
)
output$dash_populatieIndicatoren <- reactive({
    if (is.null(results$dash_showPopulatie()))
      "" else
      results$dash_showPopulatie()
  })
outputOptions(output, "dash_populatieIndicatoren", suspendWhenHidden = FALSE)


results$dash_showJacht <- dashboardChoicesServer(
  id = "dash_jacht",
  choices = jachtChoices,
  uiText = uiText,
  regionLevel = reactive(req(input$dash_regionLevel))
)
output$dash_jachtIndicatoren <- reactive({
    if (is.null(results$dash_showJacht()))
      "" else 
      results$dash_showJacht()
  })
outputOptions(output, "dash_jachtIndicatoren", suspendWhenHidden = FALSE)


results$dash_showSchade <- dashboardChoicesServer(
  id = "dash_schade",
  choices = schadeChoices,
  uiText = uiText,
  regionLevel = reactive(req(input$dash_regionLevel))
)
output$dash_schadeIndicatoren <- reactive({
    if (is.null(results$dash_showSchade()))
      "" else 
      results$dash_showSchade()
  })   
outputOptions(output, "dash_schadeIndicatoren", suspendWhenHidden = FALSE)


results$dash_showMaatschappij <- dashboardChoicesServer(
  id = "dash_maatschappij",
  choices = maatschappijChoices, 
  uiText = uiText,
  regionLevel = reactive(req(input$dash_regionLevel))
)
output$dash_maatschappijIndicatoren <- reactive({
    if (is.null(results$dash_showMaatschappij()))
      "" else 
      results$dash_showMaatschappij()
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

dash_reportFile <- reactiveVal()

observeEvent(input$dash_createReport, {
    
    dash_reportFile(NULL)  # reset on each button press
    
    withProgress(message = 'Rapport genereren...\n', value = 0, {
        
        oldDir <- getwd()
        setwd(tempdir())
        on.exit(setwd(oldDir))
        
        fromFile <- system.file("ui/www", "reportDashboard.Rmd", package = "reportingGrofwild")
        file.copy(from = fromFile, to = file.path(tempdir(), basename(fromFile)), overwrite = TRUE)
        
        dash_reportFile(
          rmarkdown::render(
            input = file.path(tempdir(), basename(fromFile)),
            output_file = tempfile(fileext = ".pdf"),
            intermediates_dir = tempdir(),
            output_options = list(bigLogo = getPathLogo())
          )
        )
        
        # report is ready, trigger download
        setProgress(1)
        
        session$sendCustomMessage(type = "imageReady", 
          message = list(id = "dash_downloadReport"))
        
      })
    
  })


output$dash_downloadReport <- downloadHandler(
  filename = 'rapportDashboard.pdf',
  content = function(file) {
    
    file.copy(dash_reportFile(), file, overwrite = TRUE)
    
  })


## MAP ##

results$dash_finalMap <- mapFlandersServer(id = "dash_background",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "empty",
  regionLevel = reactive(req(input$dash_regionLevel)),
  locaties = reactive(input$dash_locaties),
  geoData = reactive(everGeoData),
  biotoopData = biotoopData,
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE,
  uiText = uiText
)



## PLOTS ##


results$dash_timeRange <- reactive(range(everEcoData$afschotjaar))


# Populatie 

results$dash_popTitles <- reactive({
    
    namedChoices(populatieChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

results$dash_F16_1 <- countAgeGroupServer(
  id = "dash_F16_1",
  data = reactive({
      plotData <- everEcoData[everEcoData$geslacht_comp == "Vrouwelijk", ]
      plotData$reproductiestatus <- ifelse(is.na(plotData$aantal_embryos), "Onbekend",
        ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig"))
      plotData
    }),
  timeRange = results$dash_timeRange,
  groupVariable = "reproductiestatus",
  title = reactive(names(results$dash_popTitles()[results$dash_popTitles() == "F16_1"]))
)


observe({
    
    updateActionLink(session = session, inputId = "dash_showF17_1",
      label = paste("FIGUUR:", names(namedChoices("F17_1", uiText = uiText, 
            regionLevel = req(input$dash_regionLevel)))))
    
  })

results$dash_F17_1 <- mapFlandersServer(id = "dash_F17_1",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "dash",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  geoData = reactive(everGeoAll),
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE,
  countVariable = "aantal")


results$dash_F17_4 <- mapSpreadServer(id = "dash_F17_4",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  allSpatialData = spatialData,
  type = "F17_4",
  title = reactive(names(results$dash_popTitles()[results$dash_popTitles() == "F17_4"]))
) 

results$dash_F18_1 <- barDraagkrachtServer(id = "dash_F18_1",
  data = reactive(inschattingData[Vraag == "populatie_evolutie", ]),
  yVar = "Vraag",
  title = reactive(names(results$dash_popTitles()[results$dash_popTitles() == "F18_1"]))
)


# Jacht

results$dash_jachtTitles <- reactive({
    
    namedChoices(jachtChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

results$dash_F05_1 <- trendYearRegionServer(id = "dash",
  data = results$dash_ecoData, 
  species = results$dash_species,
  timeRange = results$dash_timeRange,
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive({
      if (req(input$dash_regionLevel) == "flanders")
        "Vlaams Gewest" else 
        input$dash_locaties
    }),
  geoData = reactive(everGeoData),
  allSpatialData = spatialData,
  biotoopData = biotoopData,
  title = reactive(names(results$dash_jachtTitles()[results$dash_jachtTitles() == "F05_1"]))
)

results$dash_F05_2 <- countYearAgeServer(id = "dash",
  data = results$dash_ecoData,
  timeRange = results$dash_timeRange,
  title = reactive(names(results$dash_popTitles()[results$dash_popTitles() == "F05_2"])) 
)


# Schade

results$dash_schadeTitles <- reactive({
    
    namedChoices(schadeChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

results$dash_F06_1 <- mapSpreadServer(id = "dash_F06_1",
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive(input$dash_locaties),
  allSpatialData = spatialData,
  type = "F06",
  title = reactive(names(results$dash_schadeTitles()[results$dash_schadeTitles() == "F06_1"]))
) 

results$dash_F07_1 <- barCostServer(id = "dash_F07_1",
  data = reactive(results$dash_schadeData()@data),
  yVar = "count",
  title = reactive(names(results$dash_schadeTitles()[results$dash_schadeTitles() == "F07_1"])) 
)

results$dash_F09_2 <- barCostServer(id = "dash_F09_2",
  data = reactive(results$dash_schadeData()@data),
  yVar = "schadeBedrag",
  title = reactive(names(results$dash_schadeTitles()[results$dash_schadeTitles() == "F09_2"]))
)

results$dash_F07_3 <- barDraagkrachtServer(id = "dash_F07_3", 
  data = reactive(inschattingData[Vraag != "populatie_evolutie", ]), 
  yVar = "Vraag",
  title = reactive(names(results$dash_schadeTitles()[results$dash_schadeTitles() == "F07_3"]))
)



# Maatschappelijke draagkracht

results$dash_maatschappijTitles <- reactive({
    
    namedChoices(maatschappijChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

results$dash_F12_1 <- barDraagkrachtServer(id = "dash_F12_1",
  data = reactive(data.table::fread(file.path(draagkrachtDir, "F12_1_data.csv"))),
  xVar = "Jaar", yVar = "Aantal",
  title = reactive(names(results$dash_maatschappijTitles()[results$dash_maatschappijTitles() == "F12_1"]))
)

results$dash_F14_1 <- barDraagkrachtServer(id = "dash_F14_1",
  data = reactive(data.table::fread(file.path(draagkrachtDir, "F14_1_data.csv"))),
  groupVariable = "Sector", yVar = "Year",
  title = reactive(names(results$dash_maatschappijTitles()[results$dash_maatschappijTitles() == "F14_1"]))
)

results$dash_F14_2 <- barDraagkrachtServer(id = "dash_F14_2",
  data = reactive(data.table::fread(file.path(draagkrachtDir, "F14_2_data.csv"))),
  groupVariable = "Sector", yVar = "Year",
  title = reactive(names(results$dash_maatschappijTitles()[results$dash_maatschappijTitles() == "F14_2"]))
)

results$dash_F14_3 <- barDraagkrachtServer(id = "dash_F14_3",
  data = reactive(data.table::fread(file.path(draagkrachtDir, "F14_3_data.csv"))),
  groupVariable = "Sector", yVar = "Question_label",
  title = reactive(names(results$dash_maatschappijTitles()[results$dash_maatschappijTitles() == "F14_3"]))
)

results$dash_F14_4 <- barDraagkrachtServer(id = "dash_F14_4",
  data = reactive(data.table::fread(file.path(draagkrachtDir, "F14_4_data.csv"))),
  groupVariable = c("Groep", "Year"), yVar = "Question_label",
  title = reactive(names(results$dash_maatschappijTitles()[results$dash_maatschappijTitles() == "F14_4"]))
)

results$dash_F14_5 <- barDraagkrachtServer(id = "dash_F14_5",
  data = reactive(data.table::fread(file.path(draagkrachtDir, "F14_5_data.csv"))),
  groupVariable = "Sector", yVar = "Question_label",
  title = reactive(names(results$dash_maatschappijTitles()[results$dash_maatschappijTitles() == "F14_5"]))
)
