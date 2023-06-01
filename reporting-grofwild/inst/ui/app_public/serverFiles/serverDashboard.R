# Server file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################


everEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
everGeoData <- geoData[geoData$wildsoort == "Wild zwijn", ]
everSchadeData <- schadeData[schadeData$wildsoort == "Wild zwijn", ]

# Combine waarnemingen.be & afschot
everGeoAll <- rbind(
  # waarnemingen
  readS3(FUN = data.table::fread, file = "waarnemingen_wild_zwijn_processed.csv"),
  # afschot
  everGeoData,
  fill = TRUE)

inschattingData <- readS3(FUN = data.table::fread, file = "Data_inschatting.csv")


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


output$dash_regionWarning <- renderUI({
    
    if (req(input$dash_regionLevel) != "flanders")
      validate(need(input$dash_locaties, "Gelieve regio('s) te selecteren"))
      
  })


observe({
    
    req(input$dash_regionLevel)
    shinyjs::toggle(id = "dash_results", 
      condition = input$dash_regionLevel == "flanders" || !is.null(input$dash_locaties)) 
       
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
        
        fromFiles <- system.file("ui/www", c("reportDashboard.Rmd", "plotDashboard.Rmd"), package = "reportingGrofwild")
        file.copy(from = fromFiles, to = file.path(tempdir(), basename(fromFiles)), overwrite = TRUE)
        
        dash_reportFile(
          rmarkdown::render(
            input = file.path(tempdir(), basename(fromFiles[1])),
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
  locaties = reactive(if(req(input$dash_regionLevel) == "flanders") "flanders" else input$dash_locaties),
  geoData = reactive(everGeoData),
  biotoopData = biotoopData,
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE,
  uiText = uiText
)



## PLOTS ##


results$dash_timeRange <- reactive(range(everEcoData$afschotjaar))


# Populatie 

results$dash_titlesPopulatie <- reactive({
    
    namedChoices(populatieChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

results$dash_F16_1 <- countAgeGroupServer(
  id = "dash_F16_1",
  data = reactive({
      plotData <- results$dash_ecoData()[results$dash_ecoData()$geslacht_comp == "Vrouwelijk", ]
      validate(need(nrow(plotData) > 0, "Geen data beschikbaar"))
      plotData$reproductiestatus <- ifelse(is.na(plotData$aantal_embryos), "Onbekend",
        ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig"))
      plotData
    }),
  timeRange = results$dash_timeRange,
  groupVariable = "reproductiestatus",
  title = reactive(names(results$dash_titlesPopulatie()[results$dash_titlesPopulatie() == "F16_1"]))
)


output$dash_disclaimerF17_1 <- renderUI({
    
    myTitle <- names(namedChoices("F17_1", uiText = uiText, 
        regionLevel = req(input$dash_regionLevel)))
    
    updateActionLink(session = session, inputId = "dash_showF17_1",
      label = paste("FIGUUR:", myTitle))
    
    if (grepl("\\*", myTitle))
      getDisclaimerLimited() else
      NULL
    
  })


results$dash_F17_1 <- mapFlandersServer(id = "dash_F17_1",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "dash",
  regionLevel = reactive(req(input$dash_regionLevel)),
  locaties = reactive(req(input$dash_locaties)),
  geoData = reactive(everGeoAll),
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE,
  countVariable = "aantal",
  uiText = uiText)


results$dash_F17_4 <- mapSpreadServer(id = "dash_F17_4",
  regionLevel = reactive(req(input$dash_regionLevel)),
  locaties = reactive(req(input$dash_locaties)),
  allSpatialData = spatialData,
  type = "F17_4",
  title = reactive(names(results$dash_titlesPopulatie()[results$dash_titlesPopulatie() == "F17_4"]))
) 

results$dash_F18_1 <- barDraagkrachtServer(id = "dash_F18_1",
  data = reactive(inschattingData[Vraag == "populatie_evolutie", ]),
  yVar = "Vraag",
  title = reactive(names(results$dash_titlesPopulatie()[results$dash_titlesPopulatie() == "F18_1"]))
)


# Jacht

results$dash_titlesJacht <- reactive({
    
    namedChoices(jachtChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

results$dash_drukjachtData <- reactive({      
    
    drukjachtData <- merge(
      results$dash_ecoData()[results$dash_ecoData()$jachtmethode_comp == "Drukjacht", 
        c("ID", "afschot_datum", "afschotjaar", "provincie", "wildsoort")], 
      everGeoData[, c("ID", "WBE_Naam_Toek")], 
      by = "ID", all.x = TRUE)[, c("afschot_datum", "afschotjaar", "WBE_Naam_Toek", "provincie", "wildsoort")]
    # Keep unique records per WBE & date
    drukjachtData <- drukjachtData[!duplicated(drukjachtData), ]
    
    validate(need(nrow(drukjachtData) > 0, "Geen data beschikbaar"))
    
    drukjachtData
    
  })

results$dash_F04_3 <- countYearProvinceServer(id = "dash", 
  data = results$dash_drukjachtData,
  timeRange = reactive(range(results$dash_drukjachtData()$afschotjaar)),
  title = reactive(names(results$dash_titlesJacht()[results$dash_titlesJacht() == "F04_3"]))
  )

results$dash_F05_1 <- trendYearRegionServer(id = "dash",
  data = results$dash_ecoData, 
  species = results$dash_species,
  timeRange = results$dash_timeRange,
  regionLevel = reactive(input$dash_regionLevel),
  locaties = reactive({
      if (req(input$dash_regionLevel) == "flanders")
        "Vlaams Gewest" else 
        req(input$dash_locaties)
    }),
  geoData = reactive(everGeoData),
  allSpatialData = spatialData,
  biotoopData = reactive(biotoopData[[req(input$dash_regionLevel)]]),
  title = reactive(names(results$dash_titlesJacht()[results$dash_titlesJacht() == "F05_1"]))
)

results$dash_F05_2 <- countYearAgeServer(id = "dash",
  data = results$dash_ecoData,
  timeRange = results$dash_timeRange,
  title = reactive(names(results$dash_titlesJacht()[results$dash_titlesJacht() == "F05_2"])) 
)


# Schade

results$dash_titlesSchade <- reactive({
    
    namedChoices(schadeChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

#results$dash_F06_1 <- mapSpreadServer(id = "dash_F06_1",
#  regionLevel = reactive(input$dash_regionLevel),
#  locaties = reactive(input$dash_locaties),
#  allSpatialData = spatialData,
#  type = "F06",
#  title = reactive(names(results$dash_titlesSchade()[results$dash_titlesSchade() == "F06_1"]))
#) 

results$dash_F07_1 <- barCostServer(id = "dash_F07_1",
  data = reactive(results$dash_schadeData()@data),
  yVar = "count",
  title = reactive(names(results$dash_titlesSchade()[results$dash_titlesSchade() == "F07_1"])) 
)

results$dash_F09_2 <- barCostServer(id = "dash_F09_2",
  data = reactive(results$dash_schadeData()@data),
  yVar = "schadeBedrag",
  title = reactive(names(results$dash_titlesSchade()[results$dash_titlesSchade() == "F09_2"]))
)

results$dash_F07_3 <- barDraagkrachtServer(id = "dash_F07_3", 
  data = reactive(inschattingData[Vraag != "populatie_evolutie", ]), 
  yVar = "Vraag",
  title = reactive(names(results$dash_titlesSchade()[results$dash_titlesSchade() == "F07_3"]))
)



# Maatschappelijke draagkracht

results$dash_titlesMaatschappij <- reactive({
    
    namedChoices(maatschappijChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

results$dash_F12_1 <- barDraagkrachtServer(id = "dash_F12_1",
  data = reactive(readS3(FUN = data.table::fread, file = "F12_1_data.csv")),
  yVar = "Jaar", xVar = "Aantal",
  title = reactive(names(results$dash_titlesMaatschappij()[results$dash_titlesMaatschappij() == "F12_1"]))
)

results$dash_F14_1 <- barDraagkrachtServer(id = "dash_F14_1",
  data = reactive(readS3(FUN = data.table::fread, file = "F14_1_data.csv")),
  yVar = "Sector", groupVariable = "Year",
  title = reactive(names(results$dash_titlesMaatschappij()[results$dash_titlesMaatschappij() == "F14_1"]))
)

results$dash_F14_2 <- barDraagkrachtServer(id = "dash_F14_2",
  data = reactive(readS3(FUN = data.table::fread, file = "F14_2_data.csv")),
  yVar = "Sector", groupVariable = "Year",
  title = reactive(names(results$dash_titlesMaatschappij()[results$dash_titlesMaatschappij() == "F14_2"]))
)

results$dash_F14_3 <- barDraagkrachtServer(id = "dash_F14_3",
  data = reactive(readS3(FUN = data.table::fread, file = "F14_3_data.csv")),
  groupVariable = "Question_label", yVar = "Sector",
  groupLabel = "Impacts",
  title = reactive(names(results$dash_titlesMaatschappij()[results$dash_titlesMaatschappij() == "F14_3"]))
)

results$dash_F14_4 <- barDraagkrachtServer(id = "dash_F14_4",
  data = reactive({
      tmpData <- readS3(FUN = data.table::fread, file = "F14_4_data.csv")
      tmpData$Antwoord_reclass <- ifelse(tmpData$Antwoord_reclass == "Belangrijk", "Aanvaardbaar",
        ifelse(tmpData$Antwoord_reclass == "Onbelangrijk", "Niet aanvaardbaar", tmpData$Antwoord_reclass))
      tmpData
    }),
  groupVariable = "Question_label", yVar = "Groep",
  groupLabel = "Maatregelen",
  title = reactive(names(results$dash_titlesMaatschappij()[results$dash_titlesMaatschappij() == "F14_4"]))
)

results$dash_F14_5 <- barDraagkrachtServer(id = "dash_F14_5",
  data = reactive(readS3(FUN = data.table::fread, file = "F14_5_data.csv")),
  groupVariable = "Question_label",
  yVar = "Sector",
  groupLabel = "Belang in beheer", 
  title = reactive(names(results$dash_titlesMaatschappij()[results$dash_titlesMaatschappij() == "F14_5"]))
)
