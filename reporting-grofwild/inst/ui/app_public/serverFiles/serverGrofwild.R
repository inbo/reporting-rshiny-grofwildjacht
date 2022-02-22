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
# Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
callModule(module = optionsModuleServer, id = "wild_table1", 
    data = results$wild_ecoData,
    timeRange = results$wild_timeRange
)
callModule(module = plotModuleServer, id = "wild_table1",
    plotFunction = "tableProvince", 
    data = results$wild_ecoData, 
    categorie = "leeftijd")


#			# Table 2 - input
#			callModule(module = optionsModuleServer, id = "table2", 
#					data = results$wild_ecoData,
#					timeRange = results$wild_timeRange)
#			# Table 3 - input
#			callModule(module = optionsModuleServer, id = "table3", 
#					data = results$wild_ecoData,
#					timeRange = results$wild_timeRange)
#			
#			
#			observe({
#						
#						if (input$wild_species == "Ree") {
#							
#							# Table 2 - output
#							callModule(module = plotModuleServer, id = "table2",
#									plotFunction = "tableProvince", 
#									data = results$wild_ecoData,
#									categorie = "typeAantal")
#							
#							
#							# Table 3 - output
#							callModule(module = plotModuleServer, id = "table3",
#									plotFunction = "tableProvince", 
#									data = results$wild_ecoData,
#									toekenningsData = reactive(toekenningsData),
#									categorie = "typePercent")
#							
#						}
#						
#					})


# Plot 1: Gerapporteerd aantal per jaar en per regio
callModule(module = optionsModuleServer, id = "wild_plot1", 
    data = results$wild_ecoData,
    timeRange = reactive(if (input$wild_species == "Edelhert")
              c(2008, max(results$wild_timeRange())) else 
              results$wild_timeRange()))
callModule(module = plotModuleServer, id = "wild_plot1",
    plotFunction = "countYearProvince", 
    data = results$wild_ecoData)


# Plot 2: Leeftijdscategorie op basis van onderkaak & meldingsformulier
countAgeCheekServer(id = "wild",
  data = results$wild_ecoData,
  timeRange = reactive(if (input$wild_species == "Ree")
        c(2005, max(results$wild_timeRange())) else 
        results$wild_timeRange())
)


# Plot 3: Afschot per jaar en per leeftijdscategorie (o.b.v. onderkaak)
callModule(module = optionsModuleServer, id = "wild_plot3", 
    data = results$wild_ecoData,
    timeRange = results$wild_timeRange)
callModule(module = plotModuleServer, id = "wild_plot3",
    plotFunction = "countYearAge", 
    data = results$wild_ecoData)

# Plot 4: Percentage jaarlijkse afschot
results$types <- reactive({
      
      req(results$wild_openingstijdenData())
      
      types <- unique(results$wild_openingstijdenData()$Type)
      
      if (length(types) == 1 && types == "")
        return(c("alle" = "all")) else 
        return(types)
      
    })


callModule(module = optionsModuleServer, id = "wild_plot4", 
    data = results$wild_ecoData,
    timeRange = results$wild_openingstijd,
    timeLabel = "Referentieperiode",
    types = results$types,
    multipleTypes = FALSE)

callModule(module = plotModuleServer, id = "wild_plot4",
    plotFunction = "percentageYearlyShotAnimals", 
    data = results$wild_ecoData,
    openingstijdenData = results$wild_openingstijdenData)


#			# Plot 4b
#			callModule(module = optionsModuleServer, id = "wild_plot4b", 
#					data = results$wild_ecoData,
#					timeRange = results$wild_timeRange,
#					types = results$types,
#					multipleTypes = TRUE)
#			
#			callModule(module = plotModuleServer, id = "wild_plot4b",
#					plotFunction = "percentageRealisedShotAnimals", 
#					data = results$wild_ecoData,
#					toekenningsData = reactive(toekenningsData))


# Plot 5: Geslachtsverdeling binnen het afschot per leeftijdscategorie
countAgeGenderServer(id = "wild",
  data = results$wild_ecoData,
  timeRange = results$wild_timeRange)


# Plot 6: Leeggewicht per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
callModule(module = optionsModuleServer, id = "wild_plot6", 
    data = results$wild_ecoData,
    types = reactive(switch(input$wild_species,
            "Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
            Ree = c("Kits", "Jongvolwassen", "Volwassen")									
        )),
    labelTypes = "Leeftijdscategorie",
    multipleTypes = TRUE,
    timeRange = reactive(if (input$wild_species == "Ree")
              c(2014, max(results$wild_timeRange())) else 
              results$wild_timeRange())
)
callModule(module = plotModuleServer, id = "wild_plot6",
    plotFunction = "boxAgeWeight", 
    data = results$wild_ecoData)


# Plot 7: Onderkaaklengte per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
ageGenderLowerJawServer(id = "wild",
  data = results$wild_ecoData,
  types = reactive(switch(input$wild_species,
      "Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
      Ree = c("Kits", "Jongvolwassen", "Volwassen")									
    )),
  timeRange = reactive(if (input$wild_species == "Ree")
        c(2014, max(results$wild_timeRange())) else 
        results$wild_timeRange())
)


# Plot 8: Onderkaaklengte per jaar
results$typesGender <- reactive({
    
    types <- levels(droplevels(results$wild_ecoData()$type_comp))
    types[types != ""]
    
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
      types[types %in% c("Reegeit", "Smalree")]
      
    })

countEmbryosServer(id = "wild",
    data = results$wild_ecoData,
    timeRange = results$wild_timeRange,
    types = results$typesFemale)

results$wild_combinedData <- reactive({
    
    merge(
      results$wild_ecoData(), 
      results$wild_geoData()[, c("ID", "FaunabeheerZone")], 
      by = "ID")
    
  })  
  
# Plot 11: Afschot per jachtmethode
countHuntingMethodServer(id = "wild",
  data = results$wild_combinedData,
  timeRange = reactive(c(2014, results$wild_timeRange()[2])))

# Plot 12: Verdeling afschot over de jaren
countYearShotServer(id = "wild",
  data = results$wild_combinedData,
  timeRange = results$wild_timeRange,
  types = results$types)


### The MAP
### -------------

mapFlandersServer(id = "wild",
  defaultYear = defaultYear,
  species = reactive(input$wild_species),
  geoData = results$wild_geoData,
  allSpatialData = spatialData)



