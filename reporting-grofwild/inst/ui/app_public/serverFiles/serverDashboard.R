# Server file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################


everEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
everGeoData <- geoData[geoData$wildsoort == "Wild zwijn", ]
everSchadeData <- schadeData[schadeData$wildsoort == "Wild zwijn", ]

waarnemingenData <- loadRawData(type = "waarnemingen")
# Restrict all to same date
waarnemingenData <- waarnemingenData[waarnemingenData$afschotjaar <= 
    format(max(ecoData$afschot_datum, na.rm = TRUE), "%Y"), ]


# Combine waarnemingen.be & afschot
everGeoAll <- rbind(
  # waarnemingen
  data.table::as.data.table(waarnemingenData),
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
observe({
    toggle("dash_populatieTitle", condition = !is.null(results$dash_showPopulatie()))
    toggle("dash_populatieF16_1", condition = "F16_1" %in% results$dash_showPopulatie())
    toggle("dash_populatieF17_1", condition = "F17_1" %in% results$dash_showPopulatie())
    toggle("dash_populatieF17_4", condition = "F17_4" %in% results$dash_showPopulatie())
    toggle("dash_populatieF18_1", condition = "F18_1" %in% results$dash_showPopulatie())
    toggle("dash_populatieF18_8", condition = "F18_8" %in% results$dash_showPopulatie())
  })


results$dash_showJacht <- dashboardChoicesServer(
  id = "dash_jacht",
  choices = jachtChoices,
  uiText = uiText,
  regionLevel = reactive(req(input$dash_regionLevel))
)
observe({
    toggle("dash_jachtTitle", condition = !is.null(results$dash_showJacht()))
    toggle("dash_jachtF04_3", condition = "F04_3" %in% results$dash_showJacht())
    toggle("dash_jachtF05_1", condition = "F05_1" %in% results$dash_showJacht())
    toggle("dash_jachtF05_2", condition = "F05_2" %in% results$dash_showJacht())
  })


results$dash_showSchade <- dashboardChoicesServer(
  id = "dash_schade",
  choices = schadeChoices,
  uiText = uiText,
  regionLevel = reactive(req(input$dash_regionLevel))
)
observe({
    toggle("dash_schadeTitle", condition = !is.null(results$dash_showSchade()))
    toggle("dash_schadeF07_1", condition = "F07_1" %in% results$dash_showSchade())
    toggle("dash_schadeF09_2", condition = "F09_2" %in% results$dash_showSchade())
    toggle("dash_schadeF07_3", condition = "F07_3" %in% results$dash_showSchade())
  })   


results$dash_showMaatschappij <- dashboardChoicesServer(
  id = "dash_maatschappij",
  choices = maatschappijChoices, 
  uiText = uiText,
  regionLevel = reactive(req(input$dash_regionLevel))
)
observe({
    toggle("dash_maatschappijTitle", condition = !is.null(results$dash_showMaatschappij()))
    toggle("dash_maatschappijF12_1", condition = "F12_1" %in% results$dash_showMaatschappij())
    toggle("dash_maatschappijF14_1", condition = "F14_1" %in% results$dash_showMaatschappij())
    toggle("dash_maatschappijF14_2", condition = "F14_2" %in% results$dash_showMaatschappij())
    toggle("dash_maatschappijF14_3", condition = "F14_3" %in% results$dash_showMaatschappij())
    toggle("dash_maatschappijF14_4", condition = "F14_4" %in% results$dash_showMaatschappij())
    toggle("dash_maatschappijF14_5", condition = "F14_5" %in% results$dash_showMaatschappij())
  })


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

dash_regionReady <- reactive({
    
    req(input$dash_regionLevel)
    
    if (input$dash_regionLevel != "flanders")
      validate(need(input$dash_locaties, "Gelieve regio('s) te selecteren"))
    
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
    
    if (req(input$dash_regionLevel) != "flanders") {
      
      validate(need(input$dash_locaties, "Gelieve regio('s) te selecteren"))
      
      filterGeo(data = everEcoData, regionLevel = input$dash_regionLevel, locaties = input$dash_locaties)
      
    } else everEcoData
    
  })


results$dash_kencijfersData <- reactive({
    
    dataSingleEntry <- if (req(input$dash_regionLevel) != "flanders") {
        
        validate(need(input$dash_locaties, "Gelieve regio('s) te selecteren"))
        
        filterGeo(
          data = everGeoAll, 
          regionLevel = input$dash_regionLevel, 
          locaties = input$dash_locaties, 
          choseByID = FALSE
        )
        
      } else {  
        
        everGeoAll
        
      }
    
    dataSingleEntry[ ,.(aantal= sum(aantal)), 
      by = .(gemeente_afschot_locatie, provincie, dataSource, afschotjaar)]
    
  })


results$dash_schadeData <- reactive({
    
    if (req(input$dash_regionLevel) != "flanders") {
      
      validate(need(input$dash_locaties, "Gelieve regio('s) te selecteren"))
      
      filterGeo(data =  everSchadeData, regionLevel = input$dash_regionLevel, locaties = input$dash_locaties)
      
    } else everSchadeData
    
  })



## SUBMIT & DOWNLOAD ##

dash_reportFile <- reactiveVal()
dash_results <- reactiveValues()

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

dash_results$dash_finalMap <- mapFlandersServer(id = "dash_background",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "empty",
  regionLevel = reactive(req(input$dash_regionLevel)),
  locaties = reactive({
      if (req(input$dash_regionLevel) == "flanders") 
        "flanders" else 
        input$dash_locaties
    }),
  geoData = reactive(everGeoData),
  biotoopData = biotoopData,
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE,
  uiText = uiText
)



## PLOTS ##


results$dash_timeRange <- reactive(range(everEcoData$afschotjaar))


# Populatie 

dash_titlesPopulatie <- reactive({
    
    namedChoices(populatieChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })


dash_results$dash_F16_1 <- countAgeGroupServer(
  id = "dash_F16_1",
  data = reactive({
      dash_regionReady()
      plotData <- results$dash_ecoData()[results$dash_ecoData()$geslacht_comp == "Vrouwelijk", ]
      validate(need(nrow(plotData) > 0, "Geen data beschikbaar"))
      plotData$reproductiestatus <- ifelse(is.na(plotData$aantal_embryos), "Onbekend",
        ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig"))
      plotData
    }),
  timeRange = results$dash_timeRange,
  groupVariable = "reproductiestatus",
  title = reactive(names(dash_titlesPopulatie()[dash_titlesPopulatie() == "F16_1"]))
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


dash_results$dash_F17_1 <- mapFlandersServer(id = "dash_F17_1",
  defaultYear = defaultYear,
  species = results$dash_species,
  type = "dash",
  regionLevel = reactive(req(input$dash_regionLevel)),
  locaties = reactive({
      if (req(input$dash_regionLevel) == "flanders") 
        "flanders" else 
        req(input$dash_locaties)
    }),
  geoData = reactive(everGeoAll),
  allSpatialData = spatialData,
  hideGlobeDefault = FALSE,
  countVariable = "aantal",
  sourceChoices = c("waarnemingen.be", "afschot"),
  uiText = uiText)


dash_results$dash_F17_4 <- mapSpreadServer(id = "dash_F17_4",
  regionLevel = reactive(req(input$dash_regionLevel)),
  locaties = reactive({
      if (req(input$dash_regionLevel) == "flanders") 
        "flanders" else 
        req(input$dash_locaties)
    }),
  allSpatialData = spatialData,
  type = "F17_4",
  title = reactive(names(dash_titlesPopulatie()[dash_titlesPopulatie() == "F17_4"]))
) 

dash_results$dash_F18_1 <- barDraagkrachtServer(id = "dash_F18_1",
  data = reactive({
      dash_regionReady()
      inschattingData[Vraag == "populatie_evolutie", ]
    }),
  yVar = "Vraag",
  title = reactive(names(dash_titlesPopulatie()[dash_titlesPopulatie() == "F18_1"]))
)


dash_results$dash_F18_8 <- kencijferModuleServer(
  id = "dash_F18_8",
  kencijfersData = results$dash_kencijfersData,
  species = results$dash_species
)


# Jacht

dash_titlesJacht <- reactive({
    
    namedChoices(jachtChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

dash_drukjachtData <- reactive({      
    
    dash_regionReady()
    
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

dash_results$dash_F04_3 <- countYearProvinceServer(id = "dash", 
  data = dash_drukjachtData,
  timeRange = reactive(range(dash_drukjachtData()$afschotjaar)),
  title = reactive(paste("FIGUUR:", names(dash_titlesJacht()[dash_titlesJacht() == "F04_3"])))
)


dash_results$dash_F05_1 <- trendYearRegionServer(id = "dash",
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
  title = reactive(names(dash_titlesJacht()[dash_titlesJacht() == "F05_1"]))
)

dash_results$dash_F05_2 <- countYearAgeServer(id = "dash",
  data = results$dash_ecoData,
  timeRange = results$dash_timeRange,
  title = reactive(names(dash_titlesJacht()[dash_titlesJacht() == "F05_2"])) 
)


# Schade

dash_titlesSchade <- reactive({
    
    namedChoices(schadeChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })

#results$dash_F06_1 <- mapSpreadServer(id = "dash_F06_1",
#  regionLevel = reactive(input$dash_regionLevel),
#  locaties = reactive(input$dash_locaties),
#  allSpatialData = spatialData,
#  type = "F06",
#  title = reactive(names(dash_titlesSchade()[dash_titlesSchade() == "F06_1"]))
#) 

dash_results$dash_F07_1 <- barCostServer(id = "dash_F07_1",
  data = results$dash_schadeData,
  yVar = "count",
  title = reactive(names(dash_titlesSchade()[dash_titlesSchade() == "F07_1"])) 
)

dash_results$dash_F09_2 <- barCostServer(id = "dash_F09_2",
  data = results$dash_schadeData,
  yVar = "schadeBedrag",
  title = reactive(names(dash_titlesSchade()[dash_titlesSchade() == "F09_2"]))
)

dash_results$dash_F07_3 <- barDraagkrachtServer(id = "dash_F07_3", 
  data = reactive({
      dash_regionReady()
      inschattingData[Vraag != "populatie_evolutie", ]
    }), 
  yVar = "Vraag",
  title = reactive(names(dash_titlesSchade()[dash_titlesSchade() == "F07_3"]))
)


# Maatschappelijke draagkracht

dash_titlesMaatschappij <- reactive({
    
    namedChoices(maatschappijChoices, uiText = uiText, regionLevel = req(input$dash_regionLevel))
    
  })


dash_results$dash_F12_1 <- barDraagkrachtServer(id = "dash_F12_1",
  data = reactive({
      dash_regionReady()
      readS3(FUN = data.table::fread, file = "F12_1_data.csv")
    }),
  yVar = "Jaar", xVar = "Aantal",
  title = reactive(names(dash_titlesMaatschappij()[dash_titlesMaatschappij() == "F12_1"]))
)


dash_results$dash_F14_1 <- barDraagkrachtServer(id = "dash_F14_1",
  data = reactive({
      dash_regionReady()
      readS3(FUN = data.table::fread, file = "F14_1_data.csv")
    }),
  yVar = "Sector", groupVariable = "Year",
  title = reactive(names(dash_titlesMaatschappij()[dash_titlesMaatschappij() == "F14_1"]))
)

dash_results$dash_F14_2 <- barDraagkrachtServer(id = "dash_F14_2",
  data = reactive({
      dash_regionReady()
      readS3(FUN = data.table::fread, file = "F14_2_data.csv")
    }),
  yVar = "Sector", groupVariable = "Year",
  title = reactive(names(dash_titlesMaatschappij()[dash_titlesMaatschappij() == "F14_2"]))
)

dash_results$dash_F14_3 <- barDraagkrachtServer(id = "dash_F14_3",
  data = reactive({
      dash_regionReady()
      readS3(FUN = data.table::fread, file = "F14_3_data.csv")
    }),
  groupVariable = "Question_label", yVar = "Sector",
  groupLabel = "Impacts",
  title = reactive(names(dash_titlesMaatschappij()[dash_titlesMaatschappij() == "F14_3"]))
)

dash_results$dash_F14_4 <- barDraagkrachtServer(id = "dash_F14_4",
  data = reactive({
      dash_regionReady()
      tmpData <- readS3(FUN = data.table::fread, file = "F14_4_data.csv")
      tmpData$Antwoord_reclass <- ifelse(tmpData$Antwoord_reclass == "Belangrijk", "Aanvaardbaar",
        ifelse(tmpData$Antwoord_reclass == "Onbelangrijk", "Niet aanvaardbaar", tmpData$Antwoord_reclass))
      tmpData
    }),
  groupVariable = "Question_label", yVar = "Groep",
  groupLabel = "Maatregelen",
  title = reactive(names(dash_titlesMaatschappij()[dash_titlesMaatschappij() == "F14_4"]))
)

dash_results$dash_F14_5 <- barDraagkrachtServer(id = "dash_F14_5",
  data = reactive({
      dash_regionReady()
      readS3(FUN = data.table::fread, file = "F14_5_data.csv")
    }),
  groupVariable = "Question_label",
  yVar = "Sector",
  groupLabel = "Belang in beheer", 
  title = reactive(names(dash_titlesMaatschappij()[dash_titlesMaatschappij() == "F14_5"]))
)
