library(leaflet)
library(rCharts)
library(grofWild)

`%then%` <- shiny:::`%OR%`

timePoints <- 2002:2003 # TODO data until 2014 according to their report

dataDir <- system.file("extdata", package = "grofWild")

# extract the 'countsData' object from the package
load(file.path(dataDir, "countsData.RData"))

timePoints <- sort(unique(countsData$commune$year))

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
              readShapeData(zipFile = file.path(dataDir, "provinces.zip")) else
              readShapeData(zipFile = file.path(dataDir, "communes.zip"))
            
          })
  
 	   results$countsData <- reactive({
			countsData 
			switch(input$spatialLevel,
				'provinces' = countsData$province,
				'communes' = countsData$commune
			)
		})
      
      # TODO Create data for plotting in the maps
      results$plotData <- reactive({
            
            nObs <- length(row.names(results$shapeData()@data))
            
			entitiesNaam <- results$shapeData()$NAAM
			countsDataSpecieYear <- subset(
				results$countsData(), 
				specie == input$showSpecie & year == input$showTime)
			counts <- countsDataSpecieYear[
				match(entitiesNaam, countsDataSpecieYear$NAAM), "counts"]
			counts[is.na(counts)] <- 0
			
            plotData <- data.frame(
				space = entitiesNaam,
#                gemiddelde = round(rnorm(n = nObs, mean = 0.3, sd = 0.2), 2),
				counts = counts,
				time = input$showTime
			)
            
#            plotData <- rbind(plotData, plotData)
#            plotData$time <- c(rep(2002, nObs), rep(2003, nObs))
            
            return(plotData)
            
          })
      
      
      
      # Select subset of the data
      results$subsetPlotData <- reactive({
            
            validate(need(results$plotData(), "Geen data beschikbaar"))
            
            if (is.null(input$showTime))
              selectedTime <- timePoints[1] else
              selectedTime <- as.numeric(input$showTime)
            
#            if (is.null(input$showVariable))
#              selectedVariable <- "gemiddelde" else
#              selectedVariable <- input$showVariable
            
#            subsetData <- results$plotData()[results$plotData()$time == selectedTime, ]
            returnData <- results$plotData()[, c("time", "space", "count")] #selectedVariable
            
            
            return(returnData)
            
          })
      
      
      # User input for controlling the map
      output$controlMap <- renderUI({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$plotData(), "Geen data beschikbaar"))
            
			if (input$doMap > 0) {    
		
				wellPanel(
	                fluidRow(
	#                    column(4, selectInput("showVariable", "Statistiek",
	#                            choices = c("gemiddelde", "variantie"))
	#                    ),
						column(4, 
	#							selectInput("showTime", "op tijdstip", choices = timePoints)
								numericInput("showTime", "op tijdstip", 
										value = min(timePoints),
										min = min(timePoints),
										max =  max(timePoints),
										step = 1)
						),
	                    column(4, selectInput("showRegion", "voor regio('s)",
	                            choices = results$shapeData()$NAAM, 
	                            multiple = TRUE)),
						column(4, selectInput("showSpecie", "voor specie('s)",
								choices = levels(results$countsData()$specie), 
								multiple = TRUE))
	                ),
	                
	                
	                actionLink("providerTiles", label = "Voeg landkaart toe",
	                    icon = icon("globe"))
		
				)
              
            }
            
          })
      
      
      # Which region(s) are selected?
      observe({
            
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
            selectedTime <- input$showTime
            
            
            if (!is.null(input$popupVariables)) {
              
              validate(need(input$popupVariables %in% names(results$plotData()), 
                      "Deze variabelen zijn niet beschikbaar in de data"))
              
              for (iName in input$popupVariables) {
                
                extraVariables <- paste(extraVariables,
                    "<br> <b>", iName, "</b>: ", 
                    results$plotData()[results$plotData()$time == selectedTime, iName])
                
              }
              
            }
            
            regionNames <- results$shapeData()$NAAM
            titleText <- paste("Geobserveerd", input$showVariable, "in ")            
            
            
            textPopup <- paste0("<h4>", regionNames, "</h4>",  
                "<strong>", titleText, input$showTime, "</strong>: ", 
                round(results$subsetPlotData()[, input$showVariable], 2),
                "<br>", extraVariables
            )
            
            
            return(textPopup)
            
          })
      
      
      # Send map to the UI
      output$showMap <- renderLeaflet({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$subsetPlotData(), "Geen data beschikbaar"),
                need(input$showVariable, "Gelieve 'Statistiek' te selecteren"))
            
            domain <- range(results$subsetPlotData()[, input$showVariable], na.rm = TRUE)
            
            palette <- colorNumeric(palette = input$colorPalette, 
                domain = domain)
            
            
            leaflet(results$shapeData()) %>%
                
                addPolygons(
                    weight = 1, 
                    color = "white",
                    fillColor = ~ palette(results$subsetPlotData()[, input$showVariable]),
                    fillOpacity = 0.8,
                    layerId = results$shapeData()$NAAM
                ) 
            
          })
      
      
      # Plot polygons
      observe({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$subsetPlotData(), "Geen data beschikbaar"),
                need(input$showVariable, "Gelieve 'Statistiek' te selecteren"))
            
            domain <- range(results$subsetPlotData()[, input$showVariable], na.rm = TRUE)
            
            palette <- colorNumeric(palette = input$colorPalette, 
                domain = domain)
            
            leafletProxy("showMap", data = results$shapeData()) %>%
                
                addPolygons(
                    weight = 1,
                    color = "white",
                    fillColor = ~ palette(results$subsetPlotData()[, input$showVariable]),
                    fillOpacity = 0.8,
                    layerId = results$shapeData()$NAAM,
                    group = "region"
                ) 
            
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
                need(results$subsetPlotData(), "Geen data beschikbaar"))
            
            if (!is.null(input$legendPlacement)) {
              
              domain <- range(results$subsetPlotData()[, input$showVariable], na.rm = TRUE)
              
              proxy <- leafletProxy("showMap", data = results$shapeData())
              
              proxy %>% removeControl(layerId = "legend")
              
              if (input$legendPlacement != "none"){
                
                validate(need(input$showTime, "Gelieve een tijdstip te selecteren"))
                
                proxy %>% addLegend(position = input$legendPlacement,
                    pal = colorNumeric(palette = input$colorPalette, 
                        domain = domain), 
                    values = results$subsetPlotData()[, input$showVariable],
                    opacity = 0.8,
                    title = "Legend",
                    layerId = "legend"
                )                      
                
                if (!is.null(input$legendPlacement)) {
                  
#                  domain <- range(results$shapeData()$LENGTE, na.rm = TRUE)
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
#            plotPredictions$yAxis(min = 0, max = 1)
#            plotPredictions$chart(width = 600)
            
            plotTime$title(text = paste("Geobserveerd", input$showVariable))
            
            currentData <- results$plotData()
            currentData$y <- currentData[, input$showVariable]      
            
            
            plotTime$series(
                lapply(input$showRegion, function(iArea) {
                      
                      list(name = iArea, data = toJSONArray2(currentData[
                                  currentData$space == iArea, ], json = F))
                      
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
            
            
            if (is.null(input$showVariable))
              selectedVariable <- "gemiddelde" else 
              selectedVariable <- input$showVariable
            
            
            if (is.na(selectedVariable))
              startTitle <- "Geobserveerd gemiddelde" else 
              startTitle <- paste("Geobserveerd", input$showVariable)
            
            h4(startTitle, "in", selectedTime)
            
          })
      
      
      # Create final map
      results$finalMap <- reactive({
            
            validate(need(results$shapeData(), "Geen data beschikbaar"),
                need(results$subsetPlotData(), "Geen data beschikbaar"))
            
            domain <- range(results$subsetPlotData()[, input$showVariable], na.rm = TRUE)
            
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
                  values = results$subsetPlotData()[, input$showVariable],
                  opacity = 0.8,
                  title = "Legend",
                  layerId = "legend"
              )
              
            }
            
            newMap <- addPolygons(newMap,
                weight = 1,
                color = "white",
                fillColor = ~ palette(results$subsetPlotData()[, input$showVariable]),
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