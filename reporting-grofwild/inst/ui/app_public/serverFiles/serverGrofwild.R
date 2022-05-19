# Server file for grofwild summary statistics
# 
# Author: mvarewyck
###############################################################################


### Data
### ---------------



# Create data upon user choices
results$wild_ecoData <- reactive({
      
      subset(ecoData, wildsoort == input$wild_species)
      
    })


results$wild_geoData <- reactive({
      
      req(geoData)
      
      subset(geoData, wildsoort == input$wild_species)
      
    })


results$wild_openingstijdenData <- reactive({
      
      openingstijdenData[openingstijdenData$Soort == input$wild_species, ]
      
    })


results$wild_openingstijd <- reactive({
      
      # for Ree: openingseason contains more year than in the data
      # for Wildboar: openingseason contains less year than in the data
      
      # so retains the years when data and opening season specified
      # and doesn't retain the last year (because not full)
      
      if (input$wild_species %in% c("Ree", "Wild zwijn")) {
        
        openingstijd <- c(
            max(
                min(results$wild_ecoData()$afschotjaar), 
                min(results$wild_openingstijdenData()$Jaar)
            ),
            min(
                max(results$wild_ecoData()$afschotjaar), 
                max(results$wild_openingstijdenData()$Jaar)
            )
        )
        
        openingstijd
        
      } else NULL
      
    })


results$wild_timeRange <- reactive({
      
      range(results$wild_ecoData()$afschotjaar)
      
    })  




### Summary statistics
### ---------------


## User input for controlling the plots and create plotly
tableProvinceServer(id = "wild",
    data = results$wild_ecoData,
    categorie = "leeftijd",
    timeRange = results$wild_timeRange
)
  



# Plot 1: Gerapporteerd aantal per jaar en per regio
countYearProvinceServer(id = "wild",
  data = results$wild_ecoData,
  timeRange = reactive(if (input$wild_species == "Edelhert")
        c(2008, max(results$wild_timeRange())) else 
        results$wild_timeRange())
  )


# Plot 2: Leeftijdscategorie op basis van onderkaak & meldingsformulier
countAgeCheekServer(id = "wild",
  data = results$wild_ecoData,
  timeRange = reactive(if (input$wild_species == "Ree")
        c(2005, max(results$wild_timeRange())) else 
        results$wild_timeRange())
)


# Plot 3: Afschot per jaar en per leeftijdscategorie (o.b.v. onderkaak)
countYearAgeServer(id = "wild",
  data = results$wild_ecoData,
  timeRange = results$wild_timeRange)

# Plot 4: Percentage jaarlijkse afschot
results$labeltypes <- reactive({
      
    req(results$wild_openingstijdenData())
    
    types <- loadMetaEco(species = input$wild_species)$labeltype
    
    if (length(types) == 1 && input$wild_species == types)
      return(c("alle" = "all")) else 
      return(types)
  
    })

yearlyShotAnimalsServer(id = "wild",
  data = results$wild_ecoData,
  timeRange = results$wild_openingstijd,
  type = results$labeltypes,
  openingstijdenData = results$wild_openingstijdenData
)




# Plot 5: Geslachtsverdeling binnen het afschot per leeftijdscategorie
countAgeGenderServer(id = "wild",
  data = results$wild_ecoData,
  timeRange = results$wild_timeRange)


# Plot 6: Leeggewicht per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
results$leeftijdtypes <- reactive({
    toReturn <- switch(input$wild_species,
      "Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
      Ree = c("Kits", "Jongvolwassen", "Volwassen")									
    )
    c(toReturn, "Onbekend")
  })

boxAgeWeightServer(id = "wild",
  data = results$wild_ecoData,
  type = results$leeftijdtypes,
  timeRange = reactive(if (input$wild_species == "Ree")
        c(2014, max(results$wild_timeRange())) else 
        results$wild_timeRange())
)


# Plot 7: Onderkaaklengte per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
ageGenderLowerJawServer(id = "wild",
  data = results$wild_ecoData,
  types = results$leeftijdtypes,
  timeRange = reactive(if (input$wild_species == "Ree")
        c(2014, max(results$wild_timeRange())) else 
        results$wild_timeRange())
)


# Plot 8: Onderkaaklengte per jaar
results$typesGender <- reactive({
    
    loadMetaEco(species = input$wild_species)$type_comp
    
  })

results$typesDefaultGender <- reactive({
    grep("kits", results$typesGender(), value = TRUE)
  })

plotBioindicatorServer(id = "wild_onderkaak",
  data = results$wild_ecoData,
  timeRange = results$wild_timeRange,
  types = results$typesDefaultGender,
  typesDefault = results$typesDefaultGender,
  bioindicator = "onderkaaklengte")


# Plot 9: Gewicht per jaar
plotBioindicatorServer(id = "wild_gewicht",
  data = results$wild_ecoData,
  timeRange = results$wild_timeRange,
  types = results$typesGender,
  typesDefault = results$typesDefaultGender,
  bioindicator = "ontweid_gewicht")



# Plot 10: Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar
results$typesFemale <- reactive({
    
    types <- levels(droplevels(results$wild_ecoData()$type_comp))
    
    if (input$wild_species == "Ree") {
      types[types %in% c("Reegeit", "Smalree")] 
    } else {
      types[types %in% c("Zeug", "Overloper (v)", "Frisling (v)")]      
    }
    
  })

countEmbryosServer(id = "wild",
    data = results$wild_ecoData,
    timeRange = results$wild_timeRange,
    types = results$typesFemale,
    uiText = uiText,
    wildsoort = reactive(input$wild_species))

results$wild_combinedData <- reactive({
    
    merge(
      results$wild_ecoData(), 
      results$wild_geoData()[, c("ID", "FaunabeheerZone")], 
      by = "ID")
    
  }) 

results$jachttypes <- reactive({
    
    choices <- unique(results$wild_combinedData()$jachtmethode_comp)
    if (any(is.na(choices)))
      choices[is.na(choices)] <- "onbekend"
    
    sort(choices)
    
  })
  
# Plot 11: Afschot per jachtmethode
countYearShotServer(id = "wild_jachtmethode",
  data = results$wild_combinedData,
  timeRange = reactive(c(2014, results$wild_timeRange()[2])),
  groupVariable = "jachtmethode_comp",
  types = results$jachttypes)

# Plot 12: Verdeling afschot over de jaren
countYearShotServer(id = "wild_leeftijd",
  data = results$wild_combinedData,
  timeRange = results$wild_timeRange,
  groupVariable = "leeftijd_comp",
  types = results$leeftijdtypes)



# Plot: Gerealiseerd afschot
percentageRealisedShotServer(id = "wild",
  data = reactive(toekenningsData),
  types = reactive(unique(toekenningsData$labeltype)),
  timeRange = reactive(range(toekenningsData$labeljaar))
)

boxRealisedShotServer(id = "wild", 
  data = reactive(toekenningsData),
  types = reactive(unique(toekenningsData$labeltype)),
  timeRange = reactive(range(toekenningsData$labeljaar))
)


### The MAP
### -------------

mapFlandersServer(id = "wild",
  defaultYear = defaultYear,
  species = reactive(input$wild_species),
  type = "grofwild",
  geoData = results$wild_geoData,
  biotoopData = biotoopData,
  allSpatialData = spatialData)



