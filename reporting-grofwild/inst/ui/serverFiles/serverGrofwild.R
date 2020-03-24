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


results$wild_spatialData <- reactive({
            
            req(spatialData)
            
            if (req(input$wild_species) == "Wild zwijn" & req(input$map_regionLevel) == "provinces") {
                
                spatialData$provincesVoeren
                
            } else {
                
                spatialData[[input$map_regionLevel]]
                
            }
            
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
                        )-1
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
# Table 1
callModule(module = optionsModuleServer, id = "wild_table1", 
        data = results$wild_ecoData,
        timeRange = results$wild_timeRange)
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


# Plot 1
callModule(module = optionsModuleServer, id = "wild_plot1", 
        data = results$wild_ecoData,
        timeRange = reactive(if (input$wild_species == "Edelhert")
                            c(2008, max(results$wild_timeRange())) else 
                            results$wild_timeRange()))
callModule(module = plotModuleServer, id = "wild_plot1",
        plotFunction = "countYearProvince", 
        data = results$wild_ecoData)


# Plot 2
callModule(module = optionsModuleServer, id = "wild_plot2", 
        data = results$wild_ecoData,
        timeRange = reactive(if (input$wild_species == "Ree")
                            c(2014, max(results$wild_timeRange())) else 
                            results$wild_timeRange()))
callModule(module = plotModuleServer, id = "wild_plot2",
        plotFunction = "countAgeCheek", 
        data = results$wild_ecoData)


# Plot 3
callModule(module = optionsModuleServer, id = "wild_plot3", 
        data = results$wild_ecoData,
        timeRange = results$wild_timeRange)
callModule(module = plotModuleServer, id = "wild_plot3",
        plotFunction = "countYearAge", 
        data = results$wild_ecoData)

# Plot 4
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


# Plot 5
callModule(module = optionsModuleServer, id = "wild_plot5", 
        data = results$wild_ecoData,
        timeRange = results$wild_timeRange)
callModule(module = plotModuleServer, id = "wild_plot5",
        plotFunction = "countAgeGender", 
        data = results$wild_ecoData)


# Plot 6
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
                            results$wild_timeRange()))
callModule(module = plotModuleServer, id = "wild_plot6",
        plotFunction = "boxAgeWeight", 
        data = results$wild_ecoData)


# Plot 7
callModule(module = optionsModuleServer, id = "wild_plot7", 
        data = results$wild_ecoData,
        types = reactive(switch(input$wild_species,
                        "Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
                        Ree = c("Kits", "Jongvolwassen", "Volwassen")									
                )),
        labelTypes = "Leeftijdscategorie",
        multipleTypes = TRUE,
        timeRange = reactive(if (input$wild_species == "Ree")
                            c(2014, max(results$wild_timeRange())) else 
                            results$wild_timeRange()))
callModule(module = plotModuleServer, id = "wild_plot7",
        plotFunction = "boxAgeGenderLowerJaw", 
        data = results$wild_ecoData)


# Plot 8
results$typesGender <- reactive({
            
            types <- levels(droplevels(results$wild_ecoData()$type_comp))
            types[types != ""]
            
        })

results$typesDefaultGender <- reactive({
            grep("kits", results$typesGender(), value = TRUE)
        })

callModule(module = optionsModuleServer, id = "wild_plot8", 
        data = results$wild_ecoData,
        timeRange = results$wild_timeRange,
        types = results$typesDefaultGender,
        typesDefault = results$typesDefaultGender,
        multipleTypes = TRUE)
callModule(module = plotModuleServer, id = "wild_plot8",
        plotFunction = "plotBioindicator", 
        bioindicator = "onderkaaklengte",
        data = results$wild_ecoData)


# Plot 9
callModule(module = optionsModuleServer, id = "wild_plot9", 
        data = results$wild_ecoData,
        timeRange = results$wild_timeRange,
        types = results$typesGender,
        typesDefault = results$typesDefaultGender,
        multipleTypes = TRUE)
callModule(module = plotModuleServer, id = "wild_plot9",
        plotFunction = "plotBioindicator", 
        bioindicator = "ontweid_gewicht",
        data = results$wild_ecoData)


# Plot 10  -  currently not shown
results$typesFemale <- reactive({
            
            types <- levels(droplevels(results$wild_ecoData()$type_comp))
            types[!types %in% c("", "Bokkits", "Jaarlingbok", "Reebok", "Geitkits", "Reekits")]
            
        })

callModule(module = optionsModuleServer, id = "wild_plot10", 
        data = results$wild_ecoData,
        timeRange = results$wild_timeRange,
        types = results$typesFemale,
        multipleTypes = TRUE)
callModule(module = plotModuleServer, id = "wild_plot10",
        plotFunction = "countEmbryos",
        data = results$wild_ecoData)





### The MAP
### -------------



# Data-dependent input fields
output$map_year <- renderUI({
            
            div(class = "sliderBlank", 
                    sliderInput(inputId = "map_year", label = "Geselecteerd Jaar (kaart)",
                            min = if (input$map_regionLevel %in% c("faunabeheerzones", "fbz_gemeentes"))
                                        2014 else
                                        min(results$wild_geoData()$afschotjaar),
                            max = max(results$wild_geoData()$afschotjaar),
                            value = defaultYear,
                            sep = "", step = 1))
            
        })


output$map_time <- renderUI({
            
            minYear <- if (input$map_regionLevel %in% c("faunabeheerzones", "fbz_gemeentes"))
                        2014 else
                        min(results$wild_geoData()$afschotjaar)
            
            sliderInput(inputId = "map_time", label = "Periode (grafiek)", 
                    value = c(minYear, defaultYear),
                    min = minYear,
                    max = max(results$wild_geoData()$afschotjaar),
                    step = 1,
                    sep = "")
            
        })

# show borders in map if gemeente -> province or fbz
output$map_border <- renderUI({
            
            conditionalPanel("input.map_regionLevel == 'communes'",
                    selectInput(
                            inputId = "map_border",
                            label = "Boorden (kaart)",
                            choices = c(
                                    "Faunabeheerzones" = "faunabeheerzones",
                                    "Provincies" = "provinces",
                                    "")
                    )
            )
            
        })


output$map_region <- renderUI({
            
            if (input$map_regionLevel == "flanders")
                selected <- results$wild_spatialData()$NAAM[1] else
                selected <- NULL
            
            selectInput(inputId = "map_region", label = "Regio('s)",
                    choices = levels(droplevels(results$wild_spatialData()$NAAM)),
                    selected = selected, multiple = TRUE)
            
        })


## Time plot for Flanders (reference) ##

results$map_timeDataFlanders <- reactive({
            
            validate(need(input$map_time, "Gelieve periode te selecteren"))
            
            ## Get data for Flanders
            createTrendData(
                    data = results$wild_geoData(),
                    allSpatialData = spatialData,
                    timeRange = input$map_time,
                    species = input$wild_species,
                    regionLevel = "flanders",
                    unit = input$map_unit
            )
            
        })

callModule(module = optionsModuleServer, id = "map_timePlotFlanders", 
        data = results$map_timeDataFlanders)
callModule(module = plotModuleServer, id = "map_timePlotFlanders",
        plotFunction = "trendYearFlanders", 
        data = results$map_timeDataFlanders,
        timeRange = reactive(input$map_time),
        unit = reactive(input$map_unit)
)




## Time plot for selected region ##

# Create data for map, time plot
results$map_timeData <- reactive({
            
            validate(need(results$wild_geoData(), "Geen data beschikbaar"),
                    need(input$map_time, "Gelieve periode te selecteren"))
            
            createTrendData(
                    data = results$wild_geoData(),
                    allSpatialData = spatialData,
                    timeRange = input$map_time,
                    species = input$wild_species,
                    regionLevel = input$map_regionLevel,
                    unit = input$map_unit
            )
            
        })

# Title for selected region level
output$map_timeTitle <- renderUI({
            
            regionLevel <- switch(input$map_regionLevel,
                    "flanders" = "Vlaanderen",
                    "provinces" = "Provincie",
                    "faunabeheerzones" = "Faunabeheerzones",
                    "communes" = "Gemeente (binnen provincie)",
                    "fbz_gemeentes" = "Gemeente (binnen faunabeheerzone)",
                    "utm5" = "5x5 UTM")
            
            
            h3("Regio-schaal:", regionLevel)
            
        })


callModule(module = optionsModuleServer, id = "map_timePlot", 
        data = results$map_timeData)
callModule(module = plotModuleServer, id = "map_timePlot",
        plotFunction = "trendYearRegion", 
        data = results$map_timeData,
        locaties = reactive(input$map_region),
        timeRange = reactive(input$map_time),
        unit = reactive(input$map_unit)
)


## Map for Flanders ##

# Create data for map, summary of ecological data, given year, species and regionLevel
results$map_summarySpaceData <- reactive({
            
            validate(need(results$wild_geoData(), "Geen data beschikbaar"),
                    need(input$map_year, "Gelieve jaar te selecteren"))
            
            
            createSpaceData(
                    data = geoData, 
                    allSpatialData = spatialData,
                    year = input$map_year,
                    species = input$wild_species,
                    regionLevel = input$map_regionLevel,
                    unit = input$map_unit
            )
            
        })


#      # For checking errors in the data
#      output$table1 <- renderDataTable({results$map_spaceData()})
#      output$table2 <- renderDataTable({results$map_timeData()})


# Which region(s) are selected?
observe({
            
            event <- input$map_spacePlot_shape_click
            
            if (!is.null(event)) {
                
                if (!is.null(event$id)) {
                    
                    currentSelected <- isolate(input$map_region)
                    
                    # Remove from list
                    if (event$id %in% currentSelected) {
                        
                        updateSelectInput(session, "map_region", 
                                selected = currentSelected[ - which(currentSelected == event$id)])
                        
                        # Add to list
                    } else {
                        
                        updateSelectInput(session, "map_region", 
                                selected = c(currentSelected, event$id))
                        
                    }
                    
                }
                
            }
            
        })


# Define text to be shown in the pop-ups
results$map_textPopup <- reactive({
            
            validate(need(results$map_summarySpaceData()$data, "Geen data beschikbaar"))
            
            regionNames <- results$map_summarySpaceData()$data$locatie
            titleText <- paste("Gerapporteerd", 
                    if (input$map_unit == "absolute") "aantal" else "aantal/100ha",
                    "in", input$map_year[1])
            
            textPopup <- paste0("<h4>", regionNames, "</h4>",  
                    "<strong>", titleText, "</strong>: ", 
                    round(results$map_summarySpaceData()$data$freq, 2)
            )
            
            
            return(textPopup)
            
        })


# Define colors for the polygons
results$map_colorScheme <- reactive({
            
            # Might give warnings if n < 3
            suppressWarnings(c("white", RColorBrewer::brewer.pal(
                                    n = nlevels(results$map_summarySpaceData()$data$group) - 1, name = "YlOrBr")))
            
        })			


# Send map to the UI
output$map_spacePlot <- renderLeaflet({
            
            req(spatialData)
            
            validate(need(results$wild_spatialData(), "Geen data beschikbaar"),
                    need(nrow(results$map_summarySpaceData()$data) > 0, "Geen data beschikbaar"))
            
            mapFlanders(
                    regionLevel = input$map_regionLevel,
                    species = input$wild_species, 
                    allSpatialData = spatialData,
                    summaryData = results$map_summarySpaceData()$data,
                    colorScheme = results$map_colorScheme()
            )
            
            
        })


# Plot thick border for selected regions
observe({
            
            if (!is.null(input$map_region)) {
                
                validate(need(results$wild_spatialData(), "Geen data beschikbaar"))
                
                selectedPolygons <- subset(results$wild_spatialData(), 
                        results$wild_spatialData()$NAAM %in% input$map_region)
                
                leafletProxy("map_spacePlot", data = results$wild_spatialData()) %>%
                        
                        clearGroup(group = "regionLines") %>%
                        
                        addPolylines(data = selectedPolygons, color = "gray", weight = 5,
                                group = "regionLines")
                
            }
            
        })


# Add world map
observe({
            
            validate(need(results$wild_spatialData(), "Geen data beschikbaar"))
            
            proxy <- leafletProxy("map_spacePlot", data = results$wild_spatialData())
            
            if (!is.null(input$map_globe) & !is.null(proxy)){
                
                if (input$map_globe %% 2 == 1){
                    
                    proxy %>% addProviderTiles("Hydda.Full")
                    
                } else {
                    
                    proxy %>% clearTiles()
                    
                }
                
            }
            
        })


# Add legend
observe({
            
            validate(need(nrow(results$map_summarySpaceData()$data) > 0, "Geen data beschikbaar"))
            
            req(input$map_legend)
            
            proxy <- leafletProxy("map_spacePlot", data = results$wild_spatialData())
            proxy %>% removeControl(layerId = "legend")
            
            if (input$map_legend != "none") {
                
                palette <- colorFactor(palette = results$map_colorScheme(), 
                        levels = levels(results$map_summarySpaceData()$data$group))
                
                valuesPalette <- results$map_summarySpaceData()$data[
                        match(results$wild_spatialData()$NAAM, results$map_summarySpaceData()$data$locatie),
                        "group"]
                
                
                proxy %>% addLegend(
                        position = input$map_legend,
                        pal = palette, 
                        values = valuesPalette,
                        opacity = 0.8,
                        title = "Legende",
                        layerId = "legend"
                )                      
                
            }
            
        })


# Add popups
observe({
            
            validate(need(results$wild_spatialData(), "Geen data beschikbaar"),
                    need(results$map_textPopup(), "Geen data beschikbaar"))
            
            currentMap <- leafletProxy("map_spacePlot", data = results$wild_spatialData()) 
            currentMap %>% clearPopups()
            
            event <- input$map_spacePlot_shape_click
            
            if (!is.null(event)) {
                
                if (!is.null(event$id)) {
                    
                    if (event$id %in% results$map_summarySpaceData()$data$locatie) {
                        
                        textSelected <- results$map_textPopup()[
                                results$map_summarySpaceData()$data$locatie == event$id]
                        
                        isolate({
                                    
                                    currentMap %>% 
                                            addPopups(event$lng, event$lat, popup = textSelected)
                                    
                                }) 
                        
                    }
                    
                }
                
            }
            
        })


# Title for the map
output$map_title <- renderUI({
            
            
            h3(paste("Gerapporteerd", 
                            if (input$map_unit == "absolute") "aantal" else "aantal/100ha", 
                            "voor", tolower(input$wild_species),
                            "in", input$map_year[1]))
            
            
        })

# Statistics with map
output$map_stats <- renderUI({
      
            if (input$map_regionLevel != "flanders") {
              h5(paste0("Info beschikbaar en weergegeven voor ", results$map_summarySpaceData()$stats$percentage, 
                        "% van de totale gegevens (", results$map_summarySpaceData()$stats$nAvailable, "/", 
                        results$map_summarySpaceData()$stats$nTotal, ")" ))
            }
        })

# Create final map (for download)
results$finalMap <- reactive({
            
            validate(need(results$map_summarySpaceData()$data, "Geen data beschikbaar"))
            
            
            newMap <- mapFlanders(
                    regionLevel = input$map_regionLevel, 
                    species = input$wild_species,
                    allSpatialData = spatialData,
                    summaryData = results$map_summarySpaceData()$data,
                    colorScheme = results$map_colorScheme(),
                    legend = input$map_legend,
                    addGlobe = input$map_globe %% 2 == 1
            )
            
            # save the zoom level and centering to the map object
            newMap %<>% setView(
                    lng = input$map_spacePlot_center$lng,
                    lat = input$map_spacePlot_center$lat,
                    zoom = input$map_spacePlot_zoom
            )
            
            # write map to temp .html file
            htmlwidgets::saveWidget(newMap, file = outTempFileName, selfcontained = FALSE)
            
            # output is path to temp .html file containing map
            outTempFileName
            
            
        }) 

# Download the map
output$map_download <- downloadHandler(
        filename = function()
            nameFile(species = input$wild_species,
                    year = input$map_year[1], 
                    content = "kaart", fileExt = "png"),
        content = function(file) {
                  
             # convert temp .html file into .png for download
             webshot::webshot(url = results$finalMap(), file = file,
                     vwidth = 1000, vheight = 500, cliprect = "viewport")
            
        }
)

output$map_downloadData <- downloadHandler(
        filename = function()
            nameFile(species = input$wild_species,
                    year = input$map_year[1], 
                    content = "kaartData", fileExt = "csv"),
        content = function(file) {
            
            myData <- results$map_summarySpaceData()$data
            # change variable names
            names(myData)[names(myData) == "freq"] <- if (input$map_unit == "absolute")
                        "aantal" else "aantal/100ha"
            names(myData)[names(myData) == "group"] <- "groep"
            
            ## write data to exported file
            write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
                    sep = ";", dec = ",")
            
        })

