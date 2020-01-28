# Server file for wildschade summary statistics
# 
# Author: mvarewyck
###############################################################################



### Filter Data
### ---------------

output$schade_subcode <- renderUI({
            
            gewasChoices <- fullNames(
                    unique(schadeData@data$schadeCode[schadeData@data$schadeBasisCode == "GEWAS"]))
            voertuigChoices <- fullNames(
                    unique(schadeData@data$schadeCode[schadeData@data$schadeBasisCode == "VRTG"]))
            
            
            tagList(
                    if ("GEWAS" %in% input$schade_code)
                        selectInput(inputId = "schade_gewas", label = "Filter Gewas Schade",
                                choices = gewasChoices,
                                selected = gewasChoices,
                                multiple = TRUE,
                                width = "100%"
                        ),
                    if ("VRTG" %in% input$schade_code)
                        selectInput(inputId = "schade_voertuig", label = "Filter Voertuig Schade",
                                choices = voertuigChoices,
                                selected = voertuigChoices,
                                multiple = TRUE,
                                width = "100%"
                        )
            )
            
        })





# Filter data upon user choices
results$schade_data <- reactive({
            
            # Select species & code & exclude data before 2018
            # TODO: keep 2018 hardcoded?
            toRetain <- schadeData@data$wildsoort %in% req(input$schade_species) &
                    schadeData@data$schadeBasisCode %in% req(input$schade_code) &
                    schadeData@data$afschotjaar >= 2018
            
            # Filter gewas
            if ("GEWAS" %in% input$schade_code) {
                otherCodes <- input$schade_code[input$schade_code != "GEWAS"]
                toRetain <- toRetain &
                        (schadeData@data$schadeBasisCode %in% otherCodes |
                            schadeData@data$schadeCode %in% input$schade_gewas)
            }
            
            # Filter voertuig
            if ("VRTG" %in% input$schade_code) {
                otherCodes <- input$schade_code[input$schade_code != "VRTG"]
                toRetain <- toRetain &
                        (schadeData@data$schadeBasisCode %in% otherCodes |
                            schadeData@data$schadeCode %in% input$schade_voertuig)
            }
            
            schadeData[toRetain, ]
            
        })


# Create frequency tables for filtered data
## wildsoort
callModule(dataModuleServer, id = "wildsoort",
        data = results$schade_data,
        variable = "wildsoort")
## schade
callModule(dataModuleServer, id = "schade",
        data = results$schade_data,
        variable = "schadeBasisCode")
## subschade
callModule(dataModuleServer, id = "subschade",
        data = results$schade_data,
        variable = "schadeCode")


# Show frequency tables for filtered data
output$schade_summary <- renderUI({
            
            fixedRow(
                    column(4, tableModuleUI(id = "wildsoort", includeTotal = TRUE)),
                    column(4, tableModuleUI(id = "schade", includeTotal = TRUE)),
                    if (any(c("GEWAS", "VRTG") %in% input$schade_code)) 
                        column(4, tableModuleUI(id = "subschade",
                                        includeTotal = TRUE))
            
            )
            
        })


results$schade_timeRange <- reactive({
            
            range(results$schade_data()$afschotjaar)
            
        })  





### Summary map
### ---------------


results$schade_spatialData <- reactive({
            
            req(spatialData)
            
            spatialData[[req(input$schade_regionLevel)]]
            
            
        })

# For which region to show the summary map
output$schade_region <- renderUI({
            
            selectInput(inputId = "schade_region", label = "Regio('s)",
                    choices = levels(droplevels(results$schade_spatialData()$NAAM)),
                    selected = NULL, multiple = TRUE)
            
        })



# Data-dependent input fields
output$schade_year <- renderUI({
            
            div(class = "sliderBlank", 
                    sliderInput(inputId = "schade_year", label = "Geselecteerd Jaar (kaart)",
                            min = 
                                    if (input$schade_regionLevel %in% c("faunabeheerzones", "fbz_gemeentes"))
                                        2014 else
                                        min(results$schade_data()$afschotjaar),
                            max = max(results$schade_data()$afschotjaar),
                            value = 2018,
                            sep = "", step = 1))
            
        })


output$schade_time <- renderUI({
            
            minYear <- if (input$schade_regionLevel %in% c("faunabeheerzones", "fbz_gemeentes"))
                        2014 else
                        min(results$schade_data()$afschotjaar)
            
            sliderInput(inputId = "schade_time", label = "Periode (grafiek)", 
                    value = c(minYear, 
                            max(results$schade_data()$afschotjaar)),
                    min = minYear,
                    max = max(results$schade_data()$afschotjaar),
                    step = 1,
                    sep = "")
            
        })


# Create data for map, summary of ecological data, given year, species and regionLevel
results$schade_summarySpaceData <- reactive({
            
            validate(need(results$schade_data(), "Geen data beschikbaar"),
                    need(input$schade_year, "Gelieve jaar te selecteren"))
            
            
            createSpaceData(
                    data = results$schade_data()@data, 
                    allSpatialData = spatialData,
                    year = input$schade_year,
                    species = input$schade_species,
                    regionLevel = input$schade_regionLevel,
                    unit = input$schade_unit
            )
            
        })


# Which region(s) are selected?
observe({
            
            event <- input$schade_spacePlot_shape_click
            
            if (!is.null(event)) {
                
                if (!is.null(event$id)) {
                    
                    currentSelected <- isolate(input$schade_region)
                    
                    # Remove from list
                    if (event$id %in% currentSelected) {
                        
                        updateSelectInput(session, "schade_region", 
                                selected = currentSelected[ - which(currentSelected == event$id)])
                        
                        # Add to list
                    } else {
                        
                        updateSelectInput(session, "schade_region", 
                                selected = c(currentSelected, event$id))
                        
                    }
                    
                }
                
            }
            
        })


# Define text to be shown in the pop-ups
results$schade_textPopup <- reactive({
            
            validate(need(results$schade_summarySpaceData(), "Geen data beschikbaar"))
            
            regionNames <- results$schade_summarySpaceData()$locatie
            titleText <- paste("Gerapporteerd aantal schadegevallen", 
                    "in", input$schade_year)
            
            textPopup <- paste0("<h4>", regionNames, "</h4>",  
                    "<strong>", titleText, "</strong>: ", 
                    round(results$schade_summarySpaceData()$freq, 2)
            )
            
            
            return(textPopup)
            
        })


# Define colors for the polygons
results$schade_colorScheme <- reactive({
            
            # Might give warnings if n < 3
            suppressWarnings(c("white", RColorBrewer::brewer.pal(
                                    n = nlevels(results$schade_summarySpaceData()$group) - 1, name = "YlOrBr")))
            
        })			


# Send map to the UI
output$schade_spacePlot <- renderLeaflet({
            
            req(spatialData)
            
            validate(need(results$schade_spatialData(), "Geen data beschikbaar"),
                    need(nrow(results$schade_summarySpaceData()) > 0, "Geen data beschikbaar"))
            
            mapFlanders(
                    regionLevel = input$schade_regionLevel,
                    species = input$schade_species, 
                    allSpatialData = spatialData,
                    summaryData = results$schade_summarySpaceData(),
                    colorScheme = results$schade_colorScheme()
            )
            
        })


# Plot thick border for selected regions
observe({
            
            if (!is.null(input$schade_region)) {
                
                req(results$schade_spatialData())
                
                selectedPolygons <- subset(results$schade_spatialData(), 
                        results$schade_spatialData()$NAAM %in% input$schade_region)
                
                leafletProxy("schade_spacePlot", data = results$schade_spatialData()) %>%
                        
                        clearGroup(group = "regionLines") %>%
                        
                        addPolylines(data = selectedPolygons, color = "gray", weight = 5,
                                group = "regionLines")
                
            }
            
        })


# Add world map
observe({
            
            req(results$schade_spatialData())
            
            proxy <- leafletProxy("schade_spacePlot", data = results$schade_spatialData())
            
            if (!is.null(input$schade_globe) & !is.null(proxy)){
                
                if (input$schade_globe %% 2 == 1){
                    
                    proxy %>% addProviderTiles("Hydda.Full")
                    
                } else {
                    
                    proxy %>% clearTiles()
                    
                }
                
            }
            
        })


# Add legend
observe({
            
            req(nrow(results$schade_summarySpaceData()) > 0)
            
            req(input$schade_legend)
            
            proxy <- leafletProxy("schade_spacePlot", data = results$schade_spatialData())
            proxy %>% removeControl(layerId = "legend")
            
            if (input$schade_legend != "none") {
                
                palette <- colorFactor(palette = results$schade_colorScheme(), 
                        levels = levels(results$schade_summarySpaceData()$group))
                
                valuesPalette <- results$schade_summarySpaceData()[
                        match(results$schade_spatialData()$NAAM, results$schade_summarySpaceData()$locatie),
                        "group"]
                
                
                proxy %>% addLegend(
                        position = input$schade_legend,
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
            
            validate(need(results$schade_spatialData(), "Geen data beschikbaar"),
                    need(results$schade_textPopup(), "Geen data beschikbaar"))
            
            currentMap <- leafletProxy("schade_spacePlot", data = results$schade_spatialData()) 
            currentMap %>% clearPopups()
            
            event <- input$schade_spacePlot_shape_click
            
            if (!is.null(event)) {
                
                if (!is.null(event$id)) {
                    
                    if (event$id %in% results$schade_summarySpaceData()$locatie) {
                        
                        textSelected <- results$schade_textPopup()[
                                results$schade_summarySpaceData()$locatie == event$id]
                        
                        isolate({
                                    
                                    currentMap %>% 
                                            addPopups(event$lng, event$lat, popup = textSelected)
                                    
                                }) 
                        
                    }
                    
                }
                
            }
            
        })



# Title for the map
output$schade_title <- renderUI({
            
            
            h3(paste("Gerapporteerd aantal schadegevallen", 
                            "voor", paste(tolower(input$schade_species), collapse = ", "),
                            "in", input$schade_year))
            
            
        })

# Create final map (for download)
results$schade_finalMap <- reactive({
            
            validate(need(results$schade_summarySpaceData(), "Geen data beschikbaar"))
            
            
            newMap <- mapFlanders(
                    regionLevel = input$schade_regionLevel, 
                    species = input$schade_species,
                    allSpatialData = spatialData,
                    summaryData = results$schade_summarySpaceData(),
                    colorScheme = results$schade_colorScheme(),
                    legend = input$schade_legend,
                    addGlobe = input$schade_globe %% 2 == 1
            )
            
            # save the zoom level and centering
            newMap %>%  setView(
                    lng = input$schade_spacePlot_center$lng,
                    lat = input$schade_spacePlot_center$lat,
                    zoom = input$schade_spacePlot_zoom
            )
            
            
        }) 


# Download the map
output$schade_downloadMap <- downloadHandler(
        filename = function()
            nameFile(species = input$schade_species,
                    year = input$schade_year, 
                    content = "kaartSchade", fileExt = "png"),
        content = function(file) {
            
            mapview::mapshot(x = results$schade_finalMap(), file = file,
                    vwidth = 1000, vheight = 500, cliprect = "viewport")
            
        }
)

output$schade_downloadData <- downloadHandler(
        filename = function()
            nameFile(species = input$schade_species,
                    year = input$schade_year, 
                    content = "kaartData", fileExt = "csv"),
        content = function(file) {
            
            myData <- results$schade_summarySpaceData()
            # change variable names
            names(myData)[names(myData) == "freq"] <- "aantal schadegevallen"
            names(myData)[names(myData) == "group"] <- "groep"
            
            ## write data to exported file
            write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
                    sep = ";", dec = ",")
            
        })




## Time line plots per region
## -----------------


## Time plot for Flanders (reference) ##

results$schade_timeDataFlanders <- reactive({
            
            validate(need(input$schade_time, "Gelieve periode te selecteren"))
            
            ## Get data for Flanders
            createTrendData(
                    data = results$schade_data()@data,
                    allSpatialData = spatialData,
                    timeRange = input$schade_time,
                    species = input$schade_species,
                    regionLevel = "flanders",
                    unit = input$schade_unit
            )
            
        })

callModule(module = optionsModuleServer, id = "schade_timePlotFlanders", 
        data = results$schade_timeDataFlanders)
callModule(module = plotModuleServer, id = "schade_timePlotFlanders",
        plotFunction = "trendYearFlanders", 
        data = results$schade_timeDataFlanders,
        timeRange = reactive(input$schade_time),
        unit = reactive(input$schade_unit)
)




## Time plot for selected region ##

# Create data for map, time plot
results$schade_timeData <- reactive({
            
            validate(need(results$schade_data(), "Geen data beschikbaar"),
                    need(input$schade_time, "Gelieve periode te selecteren"))
            
            
            createTrendData(
                    data = results$schade_data()@data,
                    allSpatialData = spatialData,
                    timeRange = input$schade_time,
                    species = input$schade_species,
                    regionLevel = input$schade_regionLevel,
                    unit = input$schade_unit
            )
            
        })

# Title for selected region level
output$schade_timeTitle <- renderUI({
            
            regionLevel <- switch(input$schade_regionLevel,
                    "flanders" = "Vlaanderen",
                    "provinces" = "Provincie",
                    "faunabeheerzones" = "Faunabeheerzones",
                    "communes" = "Gemeente (binnen provincie)",
                    "fbz_gemeentes" = "Gemeente (binnen faunabeheerzone)",
                    "utm5" = "5x5 UTM")
            
            
            h3("Regio-schaal:", regionLevel)
            
        })


callModule(module = optionsModuleServer, id = "schade_timePlot", 
        data = results$schade_timeData)
callModule(module = plotModuleServer, id = "schade_timePlot",
        plotFunction = "trendYearRegion", 
        data = results$schade_timeData,
        locaties = reactive(input$schade_region),
        timeRange = reactive(input$schade_time),
        unit = reactive(input$schade_unit)
)



## Perceel map
## -----------------


# Data-dependent input fields
output$schade_time2 <- renderUI({
            
            sliderInput(inputId = "schade_time2", label = "Periode", 
                    value = results$schade_timeRange(),
                    min = results$schade_timeRange()[1],
                    max = results$schade_timeRange()[2],
                    step = 1,
                    sep = "")
            
        })


output$schade_titlePerceel <- renderUI({
            
            
            h3(paste("Gerapporteerd aantal schadegevallen", 
                            "voor", paste(tolower(input$schade_species), collapse = ", "),
                            #jaartallen
                            paste0("(", 
                                    input$schade_time2[1], 
                                    " tot ", 
                                    input$schade_time2[2],
                                    ")"
                            )
                    ))
            
        })

output$schade_perceelPlot <- renderLeaflet({
            
            validate(need(results$schade_data(), "Geen data beschikbaar"),
                    need(input$schade_time2, "Gelieve periode te selecteren"))
            
            
            
            mapSchade(
                    schadeData = results$schade_data()[
                            results$schade_data()@data$afschotjaar %in% input$schade_time2[1]:input$schade_time2[2], ],
                    regionLevel = "provinces",
                    allSpatialData = spatialData,
                    addGlobe = input$schade_globe2,
                    legend = input$schade_legend2)
            
        })




### Descriptive Plots
### -----------------

# Plot 1: Gerapporteerd aantal schadegevallen per jaar en per regio
callModule(module = optionsModuleServer, id = "schade_plot1", 
        data = reactive(results$schade_data()@data),
        types = reactive(c(
                        "Vlaanderen" = "flanders",
                        "Provincie" = "provinces", 
                        "Faunabeheerzones" = "faunabeheerzones"
                )), 
        labelTypes = "Regio", 
        typesDefault = reactive("provinces"), 
        timeRange = results$schade_timeRange)

callModule(module = plotModuleServer, id = "schade_plot1",
        plotFunction = "countYearProvince", 
        data = reactive(results$schade_data()@data))


# Plot 2: Gerapporteerd aantal schadegevallen per jaar en variabele
callModule(module = optionsModuleServer, id = "schade_plot2", 
        data = reactive(results$schade_data()@data),
        types = reactive(c(
                        "Wildsoort" = "wildsoort",
                        "Gewas" = "SoortNaam", 
                        "Type Schade" = "schadeCode"
                )), 
        labelTypes = "Variabele", 
        typesDefault = reactive("wildsoort"), 
        timeRange = results$schade_timeRange)

callModule(module = plotModuleServer, id = "schade_plot2",
        plotFunction = "countYearSchade", 
        data = reactive(results$schade_data()@data))


# Table 1: Frequency table gewas
callModule(dataModuleServer, id = "gewas",
        data = results$schade_data,
        variable = "SoortNaam")

# Table 2: Frequency table schadeCode
callModule(module = optionsModuleServer, id = "schade_table2", 
    data = reactive(results$schade_data()@data),
    types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
        )), 
    labelTypes = "Regio", 
    typesDefault = reactive("provinces"), 
    timeRange = results$schade_timeRange)
callModule(module = plotModuleServer, id = "schade_table2",
    plotFunction = "tableSchadeCode", 
    data = reactive(results$schade_data()@data),
    schade = TRUE,
    datatable = TRUE)
