# Server file for WBE page
# 
# Author: mvarewyck
###############################################################################



# ------------------ #
# Filter data on KBO #
# ------------------ #


results$wbe_currentKbo <- reactive({
    
    if (length(currentKbo) > 1)
      req(input$wbe_kboChoice) else
      currentKbo
    
  })

results$wbe_geoDataKbo <- reactive({
    
    if (results$wbe_currentKbo() %in% geoData$KboNummer_Toek)
      subset(geoData, KboNummer_Toek %in% results$wbe_currentKbo()) else 
      # when no afschot data, still show map & biotoop
      createEmptyGeo(geoData[1, ], years = 2014:max(geoData$afschotjaar),
        kbo = results$wbe_currentKbo())
  
  })

results$wbe_currentPartij <- reactive({
    
    toReturn <- unique(results$wbe_geoDataKbo()$PartijNummer)
    toReturn[!is.na(toReturn)]
    
  })

results$wbe_schadeData <- reactive({
    
    schadeData[schadeData$KboNummer %in% results$wbe_currentKbo(), ]
    
  })


## Disable species without data
# https://stackoverflow.com/a/58310568
observe({
    
    req(input$wbe_species)
    speciesChoices <- c("Wild zwijn", "Ree", "Damhert", "Edelhert")
    
    isPresent <- sapply(speciesChoices, function(iSpecies) 
        iSpecies %in% results$wbe_geoDataKbo()$wildsoort | 
          iSpecies %in% results$wbe_schadeData()@data$wildsoort,
      simplify  = FALSE)
    
    for (iSpecies in speciesChoices) {
      
      jsSelector <- sprintf('[type=radio][name=wbe_species][value="%s"]', iSpecies)
      
      if (!isPresent[[iSpecies]]) {
        
        if (input$wbe_species == iSpecies)
          updateRadioButtons(session = session, inputId = "wbe_species",
            selected = names(isPresent)[unlist(isPresent)][1])
        
        shinyjs::disable(selector = jsSelector)
        shinyjs::runjs(paste0("$('", jsSelector, "').parent().addClass('disabled').css('opacity', 0.4)"))
      } else {
        shinyjs::enable(selector = jsSelector)
        shinyjs::runjs(paste0("$('", jsSelector, "').parent().removeClass('disabled').css('opacity', 1)"))
      }
      
    }
    
  })

output$wbe_title <- renderUI({
    
    h1("Welkom op de wildbeheereenheid pagina voor",
      paste(unique(results$wbe_geoDataKbo()$WBE_Naam_Toek), collapse = ","))
    
  })


# Message when no data available for 
output$wbe_empty <- renderUI({
    
    errorMessage <- NULL
    
    if (!results$wbe_currentKbo() %in% geoData$KboNummer_Toek)
      errorMessage <- tags$p("Momenteel zijn er voor deze WBE geen afschotgegevens van de grofwildsoorten beschikbaar.", 
        "Hierdoor kunnen er geen figuren/tabellen worden getoond m.b.t. afschot.", 
        "Indien dit niet klopt, kijkt u best eerst na of de gegevens juist in het e-loket van ANB zitten.", 
        "Zijn uw gegevens toch ingegeven in het e-loket en hier worden ze hier niet weergegeven dan laat u best iets weten op", 
        tags$a(id = "wbe_contact", href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", target="_blank", "faunabeheer@inbo.be")) 
    
    if (!results$wbe_currentKbo() %in% schadeData@data$KboNummer)
      errorMessage <- tags$p(errorMessage, 
        "Momenteel zijn er voor deze WBE geen schadegegevens van de grofwildsoorten beschikbaar.", 
        "Hierdoor kunnen er geen figuren/tabellen worden getoond m.b.t. schademeldingen.",
        "Indien u denkt dat dit niet klopt, raden wij u aan gebruik te maken van het", 
        tags$a(href = "https://natuurenbos.vlaanderen.be/e-loketten", target = "_blank", "e-loket"), 
        "van ANB, de Wilderapp (", 
        tags$a(href = "https://apps.apple.com/be/app/wilder/id1478282738", target = "_blank", "ios"), 
        "of", 
        tags$a(href = "https://play.google.com/store/apps/details?id=com.wilderpg.wilder&hl=en&gl=US&pli=1", target = "_blank", "android"), 
        ") van HVV of", 
        tags$a(href = "https://waarnemingen.be/", target = "_blank", "waarnemingen.be"),
        "van Natuurpunt. Bent u zeker dat er toch gegevens bij één van de partners ingevoerd werden die hier niet worden weergegeven, dan laat u best iets weten op",
        tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", target="_blank", "faunabeheer@inbo.be")
      )
    
    tags$em(errorMessage)
    
  })


output$wbe_emptyAfschot <- reactive({
    !input$wbe_species %in% results$wbe_geoDataKbo()$wildsoort
  })
outputOptions(output, "wbe_emptyAfschot", suspendWhenHidden = FALSE)

output$wbe_emptySchade <- reactive({
    !input$wbe_species %in% results$wbe_schadeData()@data$wildsoort
  })
outputOptions(output, "wbe_emptySchade", suspendWhenHidden = FALSE)




# ---------------------- #
# Filter data on species #
# ---------------------- #

results$wbe_geoData <- reactive({
    
    subset(results$wbe_geoDataKbo(), wildsoort == req(input$wbe_species) & 
        KboNummer_Toek %in% results$wbe_currentKbo())
    
  })

results$wbe_combinedData <- reactive({
    
    ecoData <- subset(ecoData, wildsoort == req(input$wbe_species))
    
    # Combine data
    commonNames <- names(ecoData)[names(ecoData) %in% names(results$wbe_geoData())]
    combinedData <- merge(results$wbe_geoData(), ecoData, 
      by = commonNames, all.x = TRUE)
    
    combinedData
    
  })

results$wbe_toekenningsData <- reactive({
    
    toekenningsData[toekenningsData$KboNummer_Toek %in% results$wbe_currentKbo(), ]
    
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






### The MAP
### -------------


mapFlandersServer(id = "wbe",
  uiText = uiText,
  defaultYear = defaultYear,
  species = reactive(""),
  currentWbe = results$wbe_currentPartij,
  type = "wbe",
  hideGlobeDefault = FALSE,
  geoData = results$wbe_geoDataKbo,  # independent of species
  biotoopData = biotoopData,
  allSpatialData = spatialData
)




### Extra Graphs/Tables
### -------------


## Plot1: Trend over time

trendYearRegionServer(id = "wbe",
  species = reactive(input$wbe_species),
  allSpatialData = spatialData,
  biotoopData = reactive(biotoopData),
  geoData = results$wbe_geoData, 
  locaties = reactive(unique(results$wbe_geoData()$WBE_Naam_Toek[
        match(results$wbe_geoData()$PartijNummer, results$wbe_currentPartij())]))
)

## User input for controlling the plots and create plotly
# Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
tableSpeciesServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  species = reactive(input$wbe_species),
  uiText = uiText)


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
  schadeData = results$wbe_schadeData, 
  allSpatialData = reactive(filterSpatialWbe(allSpatialData = spatialData, partijNummer = results$wbe_currentPartij())), 
  timeRange = reactive({
      schadeRange <- range(results$wbe_schadeData()@data$afschotjaar)
      c(max(2018, schadeRange[1]), schadeRange[2])
    }), 
  defaultYear = defaultYear, 
  species = reactive(input$wbe_species),
  borderRegion = "WBE_buitengrenzen"
)


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
  types = results$wbe_typesGender,
  typesDefault = results$wbe_typesDefaultGender,
  bioindicator = "onderkaaklengte")

# Plot 8: Gewicht per jaar
plotBioindicatorServer(id = "wbe_gewicht",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  types = results$wbe_typesGender,
  typesDefault = results$wbe_typesDefaultGender,
  bioindicator = "ontweid_gewicht")


## Bio indicatoren ##

bioindicatorSectionServer(
  id = "wbe", 
  uiText = uiText, 
  wildsoort = reactive(input$wbe_species)
)

# Plot 9: Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar
results$typesFemale <- reactive({
    
    types <- levels(droplevels(results$wbe_combinedData()$type_comp))
    
    types <- if (input$wbe_species == "Ree") {
      types[types %in% c("Reegeit", "Smalree")] 
    } else {
      types[types %in% c("Zeug", "Overloper (v)", "Frisling (v)")]      
    }
    c(types, "Onbekend")
    
  })

countEmbryosServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = reactive(range(results$wbe_combinedData()$afschotjaar[results$wbe_combinedData()$geslacht_comp == "Vrouwelijk"])),
  types = results$typesFemale,
  uiText = uiText,
  wildsoort = reactive(input$wbe_species)
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
  data = results$wbe_toekenningsData,
  types = reactive(unique(results$wbe_toekenningsData()$labeltype)),
  timeRange = reactive(range(results$wbe_toekenningsData()$labeljaar))
)


# Plot 12: Afschot locaties
mapSchadeServer(id = "wbe_afschot",
  schadeData = results$wbe_combinedData, 
  allSpatialData = reactive(filterSpatialWbe(allSpatialData = spatialData, partijNummer = results$wbe_currentPartij())),
  type = "afschot",
  timeRange = results$wbe_timeRange, 
  defaultYear = defaultYear, 
  species = reactive(input$wbe_species),
  borderRegion = "WBE_buitengrenzen"
)