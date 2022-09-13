# Server file for WBE page
# 
# Author: mvarewyck
###############################################################################


output$wbe_title <- renderUI({
    
    h1("Welkom op de wildbeheereenheid pagina voor",
      paste(unique(geoData$WBE_Naam_Toek), collapse = ","))
    
  })

## Filter Data ##

results$wbe_geoData <- reactive({
    
    subset(geoData, wildsoort == req(input$wbe_species))
    
  })

results$wbe_combinedData <- reactive({
    
    ecoData <- subset(ecoData, wildsoort == req(input$wbe_species))
    
    # Combine data
    commonNames <- names(ecoData)[names(ecoData) %in% names(results$wbe_geoData())]
    combinedData <- merge(results$wbe_geoData(), ecoData, 
      by = commonNames, all.x = TRUE)
    
  })

results$wbe_spatialData <- reactive({
    
    spatialData <- filterSpatial(
      allSpatialData = spatialData, 
      species = req(input$wbe_species), 
      regionLevel = "WBE_binnengrenzen", 
      year = req(input$wbe_year)
    )
    
    req(spatialData)
    validate(need(any(currentWbe %in% spatialData@data$NAAM), "Geen data beschikbaar"))
    
    spatialData
    
  })


results$wbe_timeRange <- reactive({
    
    req(nrow(results$wbe_combinedData()) > 0)
    
    range(results$wbe_combinedData()$afschotjaar)
    
  })  

results$labeltypes <- reactive({
    
    types <- loadMetaEco(species = input$wbe_species)$labeltype
    
    if (length(types) == 1 && input$wbe_species == types)
      return(c("alle" = "all")) else 
      return(types)
    
  })

results$jachttypes <- reactive({
    
    choices <- unique(results$wbe_combinedData()$jachtmethode_comp)
    choices[is.na(choices)] <- "onbekend"
    
    sort(choices)
    
  })


## Disable species without data
# https://stackoverflow.com/a/58310568
observe({
    
    req(input$wbe_species)
    
    for (iSpecies in c("Wild zwijn", "Ree", "Damhert", "Edelhert")) {
      
      subData <- subset(geoData, wildsoort == iSpecies)
      
      if (nrow(subData) == 0) {
        shinyjs::disable(selector = paste0("[type=radio][name=wbe_species][value=", iSpecies, "]"))
        shinyjs::runjs(paste0("$('[type=radio][name=wbe_species][value=", iSpecies, "]').parent().addClass('disabled').css('opacity', 0.4)"))
      } else {
        shinyjs::enable(selector = paste0("[type=radio][name=wbe_species][value=", iSpecies, "]"))
        shinyjs::runjs(paste0("$('[type=radio][name=wbe_species][value=", iSpecies, "]').parent().removeClass('disabled').css('opacity', 1)"))
      }
      
    }
    
  })



### The MAP
### -------------


mapFlandersServer(id = "wbe",
  defaultYear = defaultYear,
  species = reactive(""),
  currentWbe = currentWbe,
  type = "wbe",
  hideGlobeDefault = FALSE,
  geoData = reactive(geoData),  # independent of species
  biotoopData = biotoopData,
  allSpatialData = spatialData)




### Extra Graphs/Tables
### -------------


## Plot1: Trend over time

trendYearRegionServer(id = "wbe",
  species = reactive(input$wbe_species),
  allSpatialData = spatialData,
  biotoopData = biotoopData,
  geoData = results$wbe_geoData, 
  type = "wbe", 
  regionLevelName = reactive(unique(results$wbe_geoData()$WBE_Naam_Toek[
        match(results$wbe_geoData()$PartijNummer, currentWbe)]))
)

## User input for controlling the plots and create plotly
# Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
tableSpeciesServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  species = reactive(input$wbe_species))


# Plot2: Verdeling afschot over de jaren
countYearShotServer(id = "wbe_labeltype",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  groupVariable = "labeltype",
  types = results$labeltypes)


# Plot3: Afschot per jachtmethode
countYearShotServer(id = "wbe_jachtmethode",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  groupVariable = "jachtmethode_comp",
  types = results$jachttypes)


# Plot4: Schademeldingen
mapSchadeServer(id = "wbe",
  schadeData = schadeData, 
  allSpatialData = spatialData, 
  timeRange = reactive(c(max(2018, results$wbe_timeRange()[1]), results$wbe_timeRange()[2])), 
  defaultYear = defaultYear, 
  species = reactive(input$wbe_species))


# Plot 5: Geslachtsverdeling binnen het afschot per leeftijdscategorie
countAgeGenderServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange)

# Plot 6: Leeftijdscategorie op basis van onderkaak & meldingsformulier
countAgeCheekServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange
)


# Plot 7: Onderkaaklengte per jaar
results$wbe_typesGender <- reactive({
    
    types <- levels(droplevels(results$wbe_combinedData()$type_comp))
    types[types != ""]
    
  })

results$wbe_typesDefaultGender <- reactive({
    grep("kits", results$wbe_typesGender(), value = TRUE)
  })

plotBioindicatorServer(id = "wbe_onderkaak",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  types = results$wbe_typesDefaultGender,
  typesDefault = results$wbe_typesDefaultGender,
  bioindicator = "onderkaaklengte")

# Plot 8: Gewicht per jaar
plotBioindicatorServer(id = "wbe_gewicht",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  types = results$wbe_typesGender,
  typesDefault = results$wbe_typesDefaultGender,
  bioindicator = "ontweid_gewicht")


# Plot 9: Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar
countEmbryosServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  types = reactive({
      types <- levels(droplevels(results$wbe_combinedData()$type_comp))
      types[types %in% c("Reegeit", "Smalree")]
    })
)

# Plot 10: Onderkaaklengte per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
ageGenderLowerJawServer(id = "wbe",
  data = results$wbe_combinedData,
  types = reactive(switch(input$wbe_species,
      "Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
      Ree = c("Kits", "Jongvolwassen", "Volwassen")									
    )),
  timeRange = reactive(if (input$wbe_species == "Ree")
        c(2014, max(results$wbe_timeRange())) else 
        results$wbe_timeRange())
)


# Plot 11: Gerealiseerd afschot
percentageRealisedShotServer(id = "wbe",
  data = reactive(toekenningsData),
  types = reactive(unique(toekenningsData$labeltype)),
  timeRange = reactive(range(toekenningsData$labeljaar))
)