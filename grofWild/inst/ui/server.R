library(leaflet)
library(rCharts)
library(grofWild)

`%then%` <- shiny:::`%OR%`

#timePoints <- 2002:2003 # TODO data until 2014 according to their report

dataDir <- system.file("extdata", package = "grofWild")

# extract the 'countsData' object from the package
load(file.path(dataDir, "countsData.RData"))

timePoints <- sort(unique(countsData$commune$year))

dataDir <- system.file("extdata", package = "grofWild")
provinceData <- readShapeData(zipFile = file.path(dataDir, "provinces.zip"))
communeData <- readShapeData(zipFile = file.path(dataDir, "communes.zip"))


shinyServer(function(input, output, session) {
      
      # For debugging
      observe({
            
            if (is.null(input$debug_console))
              return(NULL)
            
            if (input$debug_console > 0) {
              
              options(browserNLdisabled = TRUE)
              saved_console <- ".RDuetConsole"
              if (file.exists(saved_console)) {load(saved_console)}
              isolate(browser())
              save(file = saved_console, list = ls(environment()))
              
            }
            
          })
      
      
      results <- reactiveValues()
      
      # Read shape data for provinces or communes
      results$shapeData <- reactive({
            
            if (input$spatialLevel == "provinces")
              provinceData else
              communeData
            
          })
      
      
      # TODO Create data for plotting in the maps
      results$plotData <- reactive({
            
            switch(input$spatialLevel,
                'provinces' = countsData$province,
                'communes' = countsData$commune
            )
            
          })
      
      
      
      # Select subset of the data
      results$subsetPlotData <- reactive({
            
            validate(need(results$plotData(), "Geen data beschikbaar"))
            
#            if (is.null(input$showTime))
#              selectedTime <- timePoints[1] else
#              selectedTime <- as.numeric(input$showTime)
#            
#            if (is.null(input$showSpecies))
#              selectedSpecies <- levels(results$plotData()$specie)[1] else
#              selectedSpecies <- input$showSpecies
            
#            entitiesNaam <- results$shapeData()$NAAM
            countsDataSpecieYear <- subset(
                results$plotData(), 
                specie == input$showSpecies & year == input$showTime)
#            counts <- countsDataSpecieYear[
#                match(entitiesNaam, countsDataSpecieYear$NAAM), "counts"]
#            counts[is.na(counts)] <- 0
#            
#            plotData <- data.frame(
#                space = entitiesNaam,
            ##                gemiddelde = round(rnorm(n = nObs, mean = 0.3, sd = 0.2), 2),
#                counts = counts,
#                time = input$showTime
#            )
            
            return(countsDataSpecieYear)
            
          })
      
      
      # User input for controlling the map
      output$controlMap <- renderUI({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$plotData(), "Geen data beschikbaar"))
            
            list(
                fluidRow(
                    column(4, 
                        # TODO allow for multiple species and times
                        selectInput("showSpecies", "Species",
                            choices = levels(results$plotData()$specie), 
                            selected = levels(results$plotData()$specie)[1])
                    ),
                    column(4, 
                        sliderInput("showTime", "Tijdstip", 
                            value = min(timePoints),
                            min = min(timePoints),
                            max =  max(timePoints),
                            step = 1,
#							format="###0",
							sep = "")

                    ),
                    column(4, 
                        selectInput("showRegion", "Regio('s)",
                            choices = results$shapeData()$NAAM,
                            multiple = TRUE)
#                        checkboxInput("selectAll", "Selecteer alles", value = TRUE)
                    )
                
                ),
                
                fluidRow(
                    column(4, selectInput("popupVariables", "Extra variabelen in popup",
                            choices = names(results$plotData()), multiple = TRUE)
                    ),
                    column(4, selectInput("legendPlacement", "Legende plaatsing",
                            choices = c("<none>" = "none", "topright", "bottomright", 
                                "bottomleft", "topleft"))
                    )
                ),
                
                actionLink("providerTiles", label = "Voeg landkaart toe",
                    icon = icon("globe"))
            
            )
            
          })
      
      
      
      # Which region(s) are selected?
      observe({
            
#            validate(need(!is.null(input$selectAll), "Laden..."))
#            
#            if (input$selectAll)
#              updateSelectInput(session = session, inputId = "showRegion",
#                  selected = results$shapeData()$NAAM) else
#              updateSelectInput(session = session, inputId = "showRegion",
#                  selected = NULL)
            
            
            event <- input$showMap_shape_click
            
            if (!is.null(event)) {
              
              currentSelected <- isolate(input$showRegion)
              
              if (event$id %in% currentSelected) {
                
                updateSelectInput(session, "showRegion", 
                    selected = currentSelected[ - which(currentSelected == event$id)])
                
              } else {
                
                updateSelectInput(session, "showRegion", 
                    selected = c(currentSelected, event$id))
                
              }
              
            }
            
          })
      
      
      # Define text to be shown in the pop-ups
      results$textPopup <- reactive({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$plotData(), "Geen data beschikbaar"))
            
            extraVariables <- ""
            
            
            if (!is.null(input$popupVariables)) {
              
              validate(need(input$popupVariables %in% names(results$plotData()), 
                      "Deze variabelen zijn niet beschikbaar in de data"))
              
              for (iName in input$popupVariables) {
                
                extraVariables <- paste(extraVariables,
                    "<br> <b>", iName, "</b>: ", 
                    results$subsetPlotData()[, iName])
                
              }
              
            }
            
            regionNames <- results$shapeData()$NAAM
            titleText <- paste("Geobserveerd aantal in ")            
            
            
            textPopup <- paste0("<h4>", regionNames, "</h4>",  
                "<strong>", titleText, input$showTime, "</strong>: ", 
                round(results$subsetPlotData()$counts, 2),
                "<br>", extraVariables
            )
            
            
            return(textPopup)
            
          })
      
      
      results$colorPalette <- reactive({
            
            validate(need(nrow(results$subsetPlotData()) > 0, "Geen data beschikbaar"))
            
            domain <- range(results$subsetPlotData()$counts, na.rm = TRUE)
            
            palette <- colorNumeric(palette = input$colorPalette, 
                domain = domain)
            
            palette(results$subsetPlotData()$counts)
            
          })
      
      
      # Send map to the UI
      output$showMap <- renderLeaflet({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(nrow(results$subsetPlotData()) > 0, "Geen data beschikbaar"))
            
            if (input$spatialLevel == "provinces")
              provinceBounds <- list(color = "white", opacity = 0) else
              provinceBounds <- list(color = "black", opacity = 0.8)            
            
            
            leaflet(results$shapeData()) %>%
                
                addPolygons(
                    weight = 1, 
                    color = "white",
                    fillColor = ~ results$colorPalette(),
                    fillOpacity = 0.8,
                    layerId = results$shapeData()$NAAM,
                    group = "region"
                ) %>%
                
                addPolylines(
                    data = provinceData, 
                    color = provinceBounds$color, 
                    weight = 3,
                    opacity = provinceBounds$opacity)
            
          })
      
      
      # Plot thick border for selected regions
      observe({
            
            if (!is.null(input$showRegion)) {
              
              validate(need(results$shapeData(), "Geen data beschikbaar"))
              
              selectedPolygons <- subset(results$shapeData(), 
                  results$shapeData()$NAAM %in% input$showRegion)
              
              leafletProxy("showMap", data = results$shapeData()) %>%
                  
                  clearGroup(group = "regionLines") %>%
                  
                  addPolylines(data = selectedPolygons, color = "white", weight = 5,
                      group = "regionLines")
              
            }
            
          })
      
      
      # Plot thick border for province if communes are shown
      observe({
            
            validate(need(input$spatialLevel, "Gelieve 'Regio-schaal' te selecteren"))
            
            if (input$spatialLevel == "communes") {  
              
              leafletProxy("showMap", data = results$shapeData()) %>%
                  
                  addPolylines(data = provinceData, color = "black", weight = 3,
                      opacity = 0.8, group = "provinceLines")
              
            } else {
              
              leafletProxy("showMap", data = results$shapeData()) %>%
                  
                  clearGroup(group = "provinceLines")
              
            }
            
          })
      
      
      # Add world map
      observe({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"))
            
            proxy <- leafletProxy("showMap", data = results$shapeData())
            
            if (!is.null(input$providerTiles) & !is.null(proxy)){
              
              if (input$providerTiles %% 2 == 1){
                
                proxy %>% addProviderTiles("Hydda.Full")
                
              } else {
                
                proxy %>% clearTiles()
                
              }
              
            }
            
          })
      
      
      # Add legend
      observe({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(nrow(results$subsetPlotData()) > 0, "Geen data beschikbaar"))
            
            if (!is.null(input$legendPlacement)) {
              
              domain <- range(results$subsetPlotData()$counts, na.rm = TRUE)
              
              proxy <- leafletProxy("showMap", data = results$shapeData())
              
              proxy %>% removeControl(layerId = "legend")
              
              if (input$legendPlacement != "none"){
                
                validate(need(input$showTime, "Gelieve een tijdstip te selecteren"))
                
                proxy %>% addLegend(position = input$legendPlacement,
                    pal = colorNumeric(palette = input$colorPalette, 
                        domain = domain), 
                    values = results$subsetPlotData()$counts,
                    opacity = 0.8,
                    title = "Legend",
                    layerId = "legend"
                )                      
                
                if (!is.null(input$legendPlacement)) {
                  
                  domain <- range(results$plotData()$count, na.rm = TRUE)	
                  
                  proxy <- leafletProxy("showMap", data = results$shapeData())
                  
                  proxy %>% removeControl(layerId = "legend")
                  
                  if (input$legendPlacement != "none"){
                    
                    validate(need(input$showTime, "Gelieve een tijdstip te selecteren"))
                    
                    proxy %>% addLegend(position = input$legendPlacement,
                        pal = colorNumeric(palette = input$colorPalette, 
                            domain = domain), 
                        values = results$plotData()$count, #results$shapeData()$LENGTE,
                        opacity = 0.8,
                        title = "Legend",
                        layerId = "legend"
                    )                      
                    
                  }
                  
                }
                
              }
              
            }
            
          })
      
      
      # Add popups
      observe({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$textPopup(), "Geen data beschikbaar"))
            
            currentMap <- leafletProxy("showMap", data = results$shapeData()) 
            currentMap %>% clearPopups()
            
            event <- input$showMap_shape_click
            
            if(!is.null(event)){
              
              textSelected <- results$textPopup()[results$shapeData()$NAAM == event$id]
              
              isolate({
                    
                    currentMap %>% 
                        addPopups(event$lng, event$lat, popup = textSelected)
                    
                  }) 
              
            }
            
          })
      
      
      # Interactive time plot
      output$interactiveTime <- renderChart2({
            
            validate(need(results$plotData(), "Geen data beschikbaar"),
                need(input$showRegion, "Gelieve regio('s) te selecteren"))
            
            plotTime <- Highcharts$new()
            
            plotTime$xAxis(categories = timePoints, 
                labels = list(rotation = -45, align = 'right'))
#            plotTime$yAxis(min = 0, max = 1)
            plotTime$chart(width = 600)
            
            plotTime$title(text = paste("Geobserveerd aantal"))
            
            currentData <- subset(results$plotData(),
                specie == input$showSpecies)
            currentData$y <- currentData$counts      
            
            
            plotTime$series(
                lapply(input$showRegion, function(iArea) {
                      
                      list(name = iArea, data = toJSONArray2(currentData[
                                  currentData$NAAM == iArea, ], json = F))
                      
                    })
            )
            
            if (!is.null(input$popupVariables)) {
              
              textTooltip <- paste("<b> {point.spatialUnit} </b> <br> <br>",
                  paste0(input$popupVariables, ": {point.", input$popupVariables, "}",
                      collapse = "<br>")) 
              plotTime$tooltip(useHTML = TRUE, pointFormat = textTooltip)
              
            }
            
            plotTime$save(file.path(tempdir(), "plotTijd.html"),
                standalone = TRUE)
            
            plotTime
            
            return(plotTime)
            
          })
      
      
      output$downloadPlotTime <- downloadHandler("plotTijd.html",
          content = function(file) {
            file.copy(file.path(tempdir(), "plotTijd.html"), file)
          }
      )
      
      
      output$titleInteractivePlot <- renderUI({
            
            if (is.null(input$showTime))
              selectedTime <- timePoints[1] else 
              selectedTime <- input$showTime
            
            
            h4("Geobserveerd aantal in", selectedTime)
            
          })
      
      
      # Create final map
      results$finalMap <- reactive({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$subsetPlotData(), "Geen data beschikbaar"))
            
            domain <- range(results$subsetPlotData()$counts, na.rm = TRUE)
            
            palette <- colorNumeric(palette = input$colorPalette, 
                domain = domain)      
            
            newMap <- leaflet(results$shapeData())
            
            if (input$providerTiles %% 2 == 1) {
              
              newMap <- addProviderTiles(newMap, "Hydda.Full")
              
            } 
            
            if (input$legendPlacement != "none") { 
              
              newMap <- addLegend(newMap,
                  position = input$legendPlacement,
                  pal = colorNumeric(palette = input$colorPalette, 
                      domain = domain), 
                  values = results$subsetPlotData()$counts,
                  opacity = 0.8,
                  title = "Legend",
                  layerId = "legend"
              )
              
            }
            
            newMap <- addPolygons(newMap,
                weight = 1,
                color = "white",
                fillColor = ~ results$colorPalette(),
                fillOpacity = 0.8,
                layerId = results$shapeData()$NAAM,
                group = "region"
            ) 
            
          })
      
      output$downloadPlotSpace <- downloadHandler("plotRuimte.png",
          content = function(file) {
            
            saveWidget(widget = results$finalMap(), 
                file = file.path(tempdir(), "plotRuimte.html"), selfcontained = FALSE)
            webshot(file.path(tempdir(), "plotRuimte.html"), file = file, 
                cliprect = "viewport")
            
          }
      )
      
    })