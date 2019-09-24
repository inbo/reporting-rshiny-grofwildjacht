# Server file for wildschade summary statistics
# 
# Author: mvarewyck
###############################################################################


### Data
### ---------------



# Create data upon user choices
results$schade_data <- reactive({
            
            # Select species
            subData <- subset(schadeData, wildsoort %in% req(input$schade_species))
            
            
        })



results$schade_spatialData <- reactive({
            
            req(spatialData)
            
            spatialData[[req(input$schade_regionLevel)]]
             
            
        })





### Summary map
### ---------------

# For which region to show the summary map
output$schade_region <- renderUI({
            
            selectInput(inputId = "schade_region", label = "Regio('s)",
                    choices = levels(droplevels(results$schade_spatialData()$NAAM)),
                    selected = NULL, multiple = TRUE)
            
        })



# Data-dependent input fields
output$schade_year <- renderUI({
            
            div(class = "sliderBlank", 
                    sliderInput(inputId = "schade_year", label = "Geselecteerd Jaar",
                            min = 
#                                    if (input$schade_regionLevel %in% c("faunabeheerzones", "fbz_gemeentes"))
#                                        2014 else
                                        min(results$schade_data()$afschotjaar),
                            max = max(results$schade_data()$afschotjaar),
                            value = 2018,
                            sep = "", step = 1))
            
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
            titleText <- paste("Gerapporteerd",
                    if (input$schade_unit == "absolute") "aantal beschadigde percelen" else "aantal schadegevallen", 
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
            
            
            h3(paste("Gerapporteerd", 
                            if (input$schade_unit == "absolute") "aantal beschadigde percelen" else "aantal schadegevallen", 
                            "voor", paste(tolower(input$schade_species), collapse = ", "),
                            "in", input$schade_year))
            
            
        })

# Create final map (for download)
results$schade_finalMap <- reactive({
            
            validate(need(results$schade_summarySpaceData(), "Geen data beschikbaar"))
            
            
            newMap <- mapFlanders(
                    regionLevel = input$schade_regionLevel, 
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
                    content = "kaart", fileExt = "png"),
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
            names(myData)[names(myData) == "freq"] <- if (input$schade_unit == "absolute")
                        "aantal beschadigde percelen" else "aantal schadegevallen"
            names(myData)[names(myData) == "group"] <- "groep"
            
            ## write data to exported file
            write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
                    sep = ";", dec = ",")
            
        })




## Perceel map
## -----------------


output$schade_titlePerceel <- renderUI({
            
            
            h3(paste("Gerapporteerd aantal beschadigde percelen", 
                            "voor", paste(tolower(input$schade_species), collapse = ", "),
                            #jaartallen
                            paste0("(", 
                                    min(results$schade_data()$afschotjaar), 
                                    " tot ", 
                                    max(results$schade_data()$afschotjaar),
                                    ")"
                            )
            ))
            
        })

output$schade_perceelPlot <- renderLeaflet({
            
            req(schadeData)
            
            mapSchade(
                    schadeData = results$schade_data(),
                    regionLevel = "provinces",
                    allSpatialData = spatialData,
                    addGlobe = TRUE)
            
        })