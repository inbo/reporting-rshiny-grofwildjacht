# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



# Center coordinates of Belgium, to crop the amount of gray in image
flandersRange <- list(
		lng = 4.23,
		lat = 51.1
)




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
			
			
			output$print <- renderPrint({
						
						input$showSpecies
						
					})
			
			
			output$debug <- renderUI({
						
						tagList(
								h5(actionLink(inputId = "debug_console", label = "Connect with console"),
										align = "left"),
								verbatimTextOutput("print")
						)
					})
			
			
			
			## Create data upon user choices
			results$wildEcoData <- reactive({
						
						subset(ecoData, wildsoort == input$showSpecies)
						
					})
			
			
			results$wildGeoData <- reactive({
						
						req(geoData)
						
						subset(geoData, wildsoort == input$showSpecies)
						
					})
			
			
			results$afschotData <- reactive({
						
						results$wildEcoData()[results$wildEcoData()$doodsoorzaak == "afschot", ]
						
					})
			
			
			results$spatialData <- reactive({
						
						req(spatialData)
						
						if (input$showSpecies == "Wild zwijn" & input$map_regionLevel == "provinces") {
							
							spatialData$provincesVoeren
							
						} else {
							
							spatialData[[input$map_regionLevel]]
							
						}
						
					})
			
			
			results$openingstijdenData <- reactive({
						
						openingstijdenData[openingstijdenData$Soort == input$showSpecies, ]
						
					})
			
			
			results$openingstijd <- reactive({
						
						# for Ree: openingseason contains more year than in the data
						# for Wildboar: openingseason contains less year than in the data
						
						# so retains the years when data and opening season specified
						# and doesn't retain the last year (because not full)
						
						if (input$showSpecies %in% c("Ree", "Wild zwijn")) {
							
							openingstijd <- c(
									max(
											min(results$wildEcoData()$afschotjaar), 
											min(results$openingstijdenData()$Jaar)
									),
									min(
											max(results$wildEcoData()$afschotjaar), 
											max(results$openingstijdenData()$Jaar)
									)-1
							)
							
							openingstijd
							
						} else NULL
						
					})
			
			
			results$timeRange <- reactive({
						
						range(results$wildEcoData()$afschotjaar)
						
					})  
			
			
			
			## User input for controlling the plots and create plotly
			# Table 1
			callModule(module = optionsModuleServer, id = "table1", 
					data = results$afschotData,
					timeRange = results$timeRange)
			callModule(module = plotModuleServer, id = "table1",
					plotFunction = "tableProvince", 
					data = results$afschotData, 
					categorie = "leeftijd")
			
			
#			# Table 2 - input
#			callModule(module = optionsModuleServer, id = "table2", 
#					data = results$afschotData,
#					timeRange = results$timeRange)
#			# Table 3 - input
#			callModule(module = optionsModuleServer, id = "table3", 
#					data = results$afschotData,
#					timeRange = results$timeRange)
#			
#			
#			observe({
#						
#						if (input$showSpecies == "Ree") {
#							
#							# Table 2 - output
#							callModule(module = plotModuleServer, id = "table2",
#									plotFunction = "tableProvince", 
#									data = results$afschotData,
#									categorie = "typeAantal")
#							
#							
#							# Table 3 - output
#							callModule(module = plotModuleServer, id = "table3",
#									plotFunction = "tableProvince", 
#									data = results$afschotData,
#									toekenningsData = reactive(toekenningsData),
#									categorie = "typePercent")
#							
#						}
#						
#					})
			
			
			# Plot 1
			callModule(module = optionsModuleServer, id = "plot1", 
					data = results$wildEcoData,
					timeRange = reactive(if (input$showSpecies == "Edelhert")
										c(2008, max(results$timeRange())) else 
										results$timeRange()))
			callModule(module = plotModuleServer, id = "plot1",
					plotFunction = "countYearProvince", 
					data = results$wildEcoData)
			
			
			# Plot 2
			callModule(module = optionsModuleServer, id = "plot2", 
					data = results$wildEcoData,
					timeRange = reactive(if (input$showSpecies == "Ree")
										c(2014, max(results$timeRange())) else 
										results$timeRange()))
			callModule(module = plotModuleServer, id = "plot2",
					plotFunction = "countAgeCheek", 
					data = results$wildEcoData)
			
			
			# Plot 3
			callModule(module = optionsModuleServer, id = "plot3", 
					data = results$wildEcoData,
					timeRange = results$timeRange)
			callModule(module = plotModuleServer, id = "plot3",
					plotFunction = "countYearAge", 
					data = results$wildEcoData)
			
			# Plot 4
			results$types <- reactive({
						
						req(results$openingstijdenData())
						
						types <- unique(results$openingstijdenData()$Type)
						
						if (length(types) == 1 && types == "")
							return(c("alle" = "all")) else 
							return(types)
						
					})
			
			
			callModule(module = optionsModuleServer, id = "plot4", 
					data = results$wildEcoData,
					timeRange = results$openingstijd,
					timeLabel = "Referentieperiode",
					types = results$types,
					multipleTypes = FALSE)
			
			callModule(module = plotModuleServer, id = "plot4",
					plotFunction = "percentageYearlyShotAnimals", 
					data = results$wildEcoData,
					openingstijdenData = results$openingstijdenData)
			
			
#			# Plot 4b
#			callModule(module = optionsModuleServer, id = "plot4b", 
#					data = results$wildEcoData,
#					timeRange = results$timeRange,
#					types = results$types,
#					multipleTypes = TRUE)
#			
#			callModule(module = plotModuleServer, id = "plot4b",
#					plotFunction = "percentageRealisedShotAnimals", 
#					data = results$wildEcoData,
#					toekenningsData = reactive(toekenningsData))
			
			
			# Plot 5
			callModule(module = optionsModuleServer, id = "plot5", 
					data = results$wildEcoData,
					timeRange = results$timeRange)
			callModule(module = plotModuleServer, id = "plot5",
					plotFunction = "countAgeGender", 
					data = results$wildEcoData)
			
			
			# Plot 6
			callModule(module = optionsModuleServer, id = "plot6", 
					data = results$wildEcoData,
					types = reactive(switch(input$showSpecies,
									"Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
									Ree = c("Kits", "Jongvolwassen", "Volwassen")									
							)),
					multipleTypes = TRUE,
					timeRange = reactive(if (input$showSpecies == "Ree")
										c(2014, max(results$timeRange())) else 
										results$timeRange()))
			callModule(module = plotModuleServer, id = "plot6",
					plotFunction = "boxAgeWeight", 
					data = results$wildEcoData)
			
			
			# Plot 7
			callModule(module = optionsModuleServer, id = "plot7", 
					data = results$wildEcoData,
					types = reactive({
								tmpLevels <- levels(results$wildEcoData()$ageGender)
								tmpLevels[tmpLevels != ""]
							}),
					multipleTypes = TRUE,
					timeRange = reactive(if (input$showSpecies == "Ree")
										c(2014, max(results$timeRange())) else 
										results$timeRange()))
			callModule(module = plotModuleServer, id = "plot7",
					plotFunction = "boxAgeGenderLowerJaw", 
					data = results$wildEcoData)
			
			
			# Plot 8
			results$typesGender <- reactive({
						
						types <- levels(ecoData$ageGender)
						types[types != ""]
						
					})
			
			results$typesDefaultGender <- reactive({
						grep("kits", results$typesGender(), value = TRUE)
					})
			
			callModule(module = optionsModuleServer, id = "plot8", 
					data = results$wildEcoData,
					timeRange = results$timeRange,
					types = reactive(c("Geitkits", "Bokkits")),
					typesDefault = results$typesDefaultGender,
					multipleTypes = TRUE)
			callModule(module = plotModuleServer, id = "plot8",
					plotFunction = "plotBioindicator", 
					bioindicator = "onderkaaklengte",
					data = results$wildEcoData)
			
			
			# Plot 9
			callModule(module = optionsModuleServer, id = "plot9", 
					data = results$wildEcoData,
					timeRange = results$timeRange,
					types = results$typesGender,
					typesDefault = results$typesDefaultGender,
					multipleTypes = TRUE)
			callModule(module = plotModuleServer, id = "plot9",
					plotFunction = "plotBioindicator", 
					bioindicator = "ontweid_gewicht",
					data = results$wildEcoData)
			
			
			# Plot 10
			results$typesFemale <- reactive({
						
						types <- levels(ecoData$ageGender)
						types[!types %in% c("", "Bokkits", "Jaarlingbok", "Bok", "Geitkits")]
						
					})
			
			callModule(module = optionsModuleServer, id = "plot10", 
					data = results$wildEcoData,
					timeRange = results$timeRange,
					types = results$typesFemale,
					multipleTypes = TRUE)
			callModule(module = plotModuleServer, id = "plot10",
					plotFunction = "countEmbryos",
					data = results$wildEcoData)
			
			
			
			
			
			### The MAP ###
			
			# Data-dependent input fields
			output$map_year <- renderUI({
						
						div(class = "sliderBlank", 
								sliderInput(inputId = "map_year", label = "Geselecteerd Jaar (kaart)",
										min = min(results$wildGeoData()$afschotjaar),
										max = max(results$wildGeoData()$afschotjaar),
										value = 2016,
										sep = "", step = 1))
						
					})
			
			
			output$map_time <- renderUI({
						
						sliderInput(inputId = "map_time", label = "Periode (grafiek)", 
								value = c(min(results$wildGeoData()$afschotjaar), 
										max(results$wildGeoData()$afschotjaar)),
								min = min(results$wildGeoData()$afschotjaar),
								max = max(results$wildGeoData()$afschotjaar),
								step = 1,
								sep = "")
						
					})
			
			
			output$map_region <- renderUI({
						
						if (input$map_regionLevel == "flanders")
							selected <- results$spatialData()$NAAM[1] else
							selected <- NULL
						
						selectInput(inputId = "map_region", label = "Regio('s)",
								choices = levels(droplevels(results$spatialData()$NAAM)),
								selected = selected, multiple = TRUE)
						
					})
			
			
			## Time plot for Flanders (reference) ##
			
			results$map_timeDataFlanders <- reactive({
						
						validate(need(input$map_time, "Gelieve periode te selecteren"))
						
						## Get data for Flanders
						# Select subset for time
						chosenTimes <- input$map_time[1]:input$map_time[2]
						tmpData <- subset(results$wildGeoData(), afschotjaar %in% chosenTimes)
						
						# Create general plot data names
						plotData <- data.frame(afschotjaar = tmpData$afschotjaar)
						plotData$locatie <- "Vlaams Gewest"
						plotData$wildsoort <- input$showSpecies
						
						# Exclude data with missing time or space
						plotData <- plotData[!is.na(plotData$afschotjaar) & 
										!is.na(plotData$locatie) & plotData$locatie != "",]
						
						# Summarize data over years
						summaryData <- plyr::count(df = plotData, vars = names(plotData))
						summaryData <- plyr::rename(summaryData, replace = c("freq" = "aantal"))
						
						# Add names & times with 0 observations
						fullData <- cbind(expand.grid(afschotjaar = unique(summaryData$afschotjaar),
										locatie = "Vlaams Gewest"))
						allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
						allData$aantal[is.na(allData$aantal)] <- 0
						
						allData$afschotjaar <- as.factor(allData$afschotjaar)
						
						allData
						
					})
			
			callModule(module = optionsModuleServer, id = "map_timePlotFlanders", 
					data = results$map_timeDataFlanders)
			callModule(module = plotModuleServer, id = "map_timePlotFlanders",
					plotFunction = "trendYearFlanders", 
					data = results$map_timeDataFlanders,
					timeRange = reactive(input$map_time)
			)
			
			
			
			
			## Time plot for selected region ##
			
			# Create data for map, time plot
			results$map_timeData <- reactive({
						
						validate(need(results$wildGeoData(), "Geen data beschikbaar"),
								need(input$map_time, "Gelieve periode te selecteren"))
						
						# Select subset for time
						chosenTimes <- input$map_time[1]:input$map_time[2]
						tmpData <- subset(results$wildGeoData(), afschotjaar %in% chosenTimes)
						
						# Create general plot data names
						plotData <- data.frame(
								wildsoort = input$showSpecies,
								afschotjaar = tmpData$afschotjaar)
						if (input$map_regionLevel == "flanders")
							plotData$locatie <- "Vlaams Gewest" else if (input$map_regionLevel == "provinces")
							plotData$locatie <- tmpData$provincie else
							plotData$locatie <- tmpData$gemeente_afschot_locatie
						
						# Exclude data with missing time or space
						plotData <- plotData[!is.na(plotData$afschotjaar) & 
										!is.na(plotData$locatie) & plotData$locatie != "",]
						
						# Summarize data over years
						summaryData <- plyr::count(df = plotData, vars = names(plotData))
						summaryData <- plyr::rename(summaryData, replace = c("freq" = "aantal"))
						
						# Add names & times with 0 observations
						fullData <- cbind(expand.grid(
										wildsoort = input$showSpecies,
										afschotjaar = unique(summaryData$afschotjaar),
										locatie = unique(results$spatialData()$NAAM)))
						allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
						allData$aantal[is.na(allData$aantal)] <- 0
						
						allData$afschotjaar <- as.factor(allData$afschotjaar)
						
						
						return(allData)
						
					})
			
			# Title for selected region level
			output$map_timeTitle <- renderUI({
						
						regionLevel <- switch(input$map_regionLevel,
								"flanders" = "Vlaanderen",
								"provinces" = "Provincie",
								"communes" = "Gemeente")
						
						h3("Regio-schaal:", regionLevel)
						
					})
			
			
			callModule(module = optionsModuleServer, id = "map_timePlot", 
					data = results$map_timeData)
			callModule(module = plotModuleServer, id = "map_timePlot",
					plotFunction = "trendYearRegion", 
					data = results$map_timeData,
					locaties = reactive(input$map_region),
					timeRange = reactive(input$map_time)
			)
			
			
			## Map for Flanders ##
			
			# Create data for map, spatial plot
			results$map_spaceData <- reactive({
						
						validate(need(results$wildGeoData(), "Geen data beschikbaar"),
								need(input$map_year, "Gelieve jaar te selecteren"))
						
						# Select subset for time
						tmpData <- subset(results$wildGeoData(), afschotjaar == input$map_year)
						
						# Create general plot data names
						plotData <- data.frame(afschotjaar = tmpData$afschotjaar)
						if (input$map_regionLevel == "flanders")
							plotData$locatie <- "Vlaams Gewest" else if (input$map_regionLevel == "provinces")
							plotData$locatie <- tmpData$provincie else
							plotData$locatie <- tmpData$gemeente_afschot_locatie
						
						# Exclude data with missing time or space
						plotData <- plotData[!is.na(plotData$afschotjaar) & 
										!is.na(plotData$locatie) & plotData$locatie != "",]
						
						# Summarize data over years
						summaryData <- plyr::count(df = plotData, vars = names(plotData))
						
						# Add names & times with 0 observations
						fullData <- cbind(expand.grid(afschotjaar = unique(summaryData$afschotjaar),
										locatie = unique(results$spatialData()$NAAM)))
						allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
						allData$freq[is.na(allData$freq)] <- 0
						
						allData$afschotjaar <- as.factor(allData$afschotjaar)
						
						
						summaryData2 <- plyr::count(df = allData, vars = "locatie", wt_var = "freq")
						
						# Create group variable
						if (input$map_regionLevel %in% c("flanders", "provinces")) {
							
							otherBreaks <- sort(unique(summaryData2$freq))
							
							summaryData2$group <- cut(x = summaryData2$freq, 
									breaks = c(-Inf, otherBreaks),
									labels = otherBreaks) 
							
						} else {
							
							if (input$showSpecies %in% c("Wild zwijn", "Ree"))
								summaryData2$group <- cut(x = summaryData2$freq, 
										breaks = c(-Inf, 0, 10, 20, 40, 80, Inf),
										labels = c("0", "1-10", "11-20", "21-40", "41-80", ">80")) else
								summaryData2$group <- cut(x = summaryData2$freq, 
										breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),
										labels = c("0", "1-5", "6-10", "11-15", "16-20", ">20"))
							
#              otherBreaks <- quantile(summaryData2$freq, probs = seq(0, 1, by = 0.2))
#              otherBreaks <- unique(otherBreaks[otherBreaks != 0])
#              summaryData2$group <- cut(x = summaryData2$freq, 
#                  breaks = c(-Inf, 0, otherBreaks),
#                  labels = c("0", sapply(seq_along(otherBreaks), function(i) {
#                            if (i == 1 & any(summaryData2$freq == 0))
#                              paste0("1-", otherBreaks[i]) else if (i == 1)
#                              paste0("0-", otherBreaks[i]) else 
#                              paste0(otherBreaks[i-1]+1, "-", otherBreaks[i])
#                          })))
							
						}
						
						return(summaryData2)
						
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
						
						validate(need(results$map_spaceData(), "Geen data beschikbaar"))
						
						regionNames <- results$map_spaceData()$locatie
						titleText <- paste("Gerapporteerd aantal in", input$map_year[1])
						
						textPopup <- paste0("<h4>", regionNames, "</h4>",  
								"<strong>", titleText, "</strong>: ", 
								round(results$map_spaceData()$freq, 2)
						)
						
						
						return(textPopup)
						
					})
			
			
			# Define colors for the polygons
			results$map_colorScheme <- reactive({
						
						# Might give warnings if n < 3
						suppressWarnings(c("white", RColorBrewer::brewer.pal(
												n = nlevels(results$map_spaceData()$group) - 1, name = "YlOrBr")))
						
					})
			
			results$map_colors <- reactive({
						
						validate(need(nrow(results$map_spaceData()) > 0, "Geen data beschikbaar"))
						
						palette <- colorFactor(palette = results$map_colorScheme(), 
								levels = levels(results$map_spaceData()$group))
						
						valuesPalette <- results$map_spaceData()[
								match(results$spatialData()$NAAM, results$map_spaceData()$locatie),
								"group"]
						
						palette(valuesPalette)
						
					})
			
			
			# Send map to the UI
			output$map_spacePlot <- renderLeaflet({
						
						req(spatialData)
						
						validate(need(results$spatialData(), "Geen data beschikbaar"),
								need(nrow(results$map_spaceData()) > 0, "Geen data beschikbaar"))
						
						provinceBounds <- switch(input$map_regionLevel,
								"flanders" = list(opacity = 0), 
								"provinces" = list(opacity = 0),
								"communes" = list(color = "black", opacity = 0.8))            
						
						
						leaflet(results$spatialData()) %>%
								
								addPolygons(
										weight = 1, 
										color = "gray",
										fillColor = ~ results$map_colors(),
										fillOpacity = 0.8,
										layerId = results$spatialData()$NAAM,
										group = "region"
								) %>%
								
								addPolylines(
										data = spatialData$provinces, 
										color = provinceBounds$color, 
										weight = 3,
										opacity = provinceBounds$opacity
								)
						
					})
			
			
			# Plot thick border for selected regions
			observe({
						
						if (!is.null(input$map_region)) {
							
							validate(need(results$spatialData(), "Geen data beschikbaar"))
							
							selectedPolygons <- subset(results$spatialData(), 
									results$spatialData()$NAAM %in% input$map_region)
							
							leafletProxy("map_spacePlot", data = results$spatialData()) %>%
									
									clearGroup(group = "regionLines") %>%
									
									addPolylines(data = selectedPolygons, color = "gray", weight = 5,
											group = "regionLines")
							
						}
						
					})
			
			
			# Add world map
			observe({
						
						validate(need(results$spatialData(), "Geen data beschikbaar"))
						
						proxy <- leafletProxy("map_spacePlot", data = results$spatialData())
						
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
						
						validate(need(nrow(results$map_spaceData()) > 0, "Geen data beschikbaar"))
						
						req(input$map_legend)
						
						proxy <- leafletProxy("map_spacePlot", data = results$spatialData())
						proxy %>% removeControl(layerId = "legend")
						
						if (input$map_legend != "none") {
							
							palette <- colorFactor(palette = results$map_colorScheme(), 
									levels = levels(results$map_spaceData()$group))
							
							valuesPalette <- results$map_spaceData()[
									match(results$spatialData()$NAAM, results$map_spaceData()$locatie),
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
						
						validate(need(results$spatialData(), "Geen data beschikbaar"),
								need(results$map_textPopup(), "Geen data beschikbaar"))
						
						currentMap <- leafletProxy("map_spacePlot", data = results$spatialData()) 
						currentMap %>% clearPopups()
						
						event <- input$map_spacePlot_shape_click
						
						if (!is.null(event)) {
							
							if (!is.null(event$id)) {
								
								if (event$id %in% results$map_spaceData()$locatie) {
									
									textSelected <- results$map_textPopup()[
											results$map_spaceData()$locatie == event$id]
									
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
						
						h3(paste("Gerapporteerd aantal voor", tolower(input$showSpecies),
										"in", input$map_year[1]))
						
						
					})
			
			
			# Create final map (for download)
			results$finalMap <- reactive({
						
						validate(need(results$map_spaceData(), "Geen data beschikbaar"))
						
						
						palette <- colorFactor(palette = results$map_colorScheme(), 
								levels = levels(results$map_spaceData()$group))
						
						valuesPalette <- results$map_spaceData()[
								match(results$spatialData()$NAAM, results$map_spaceData()$locatie),
								"group"]
						
						newMap <- leaflet(results$spatialData()) %>%
								setView(lng = flandersRange$lng, lat = flandersRange$lat,
										zoom = 8.5)
						
						if (input$map_legend != "none") { 
							
							newMap <- addLegend(newMap,
									position = input$map_legend,
									pal = palette, 
									values = valuesPalette,
									opacity = 0.8,
									title = "Legende",
									layerId = "legend"
							)
							
						}
						
						newMap <- addPolygons(newMap,
								weight = 1,
								color = "gray",
								fillColor = ~ results$map_colors(),
								fillOpacity = 0.8,
								layerId = results$spatialData()$NAAM,
								group = "region"
						)
						
						if (input$map_regionLevel == "communes") {  
							
							newMap <- addPolylines(newMap,
									data = spatialData$provinces,
									color = "black", 
									weight = 3,
									opacity = 0.8, 
									group = "provinceLines")
							
						}
						
						if (input$map_globe %% 2 == 1) {
							
							newMap <- addProviderTiles(newMap, "Hydda.Full")
							
						} 
						
						# save the zoom level and centering
						newMap %>%  setView(
								lng = input$map_spacePlot_center$lng,
								lat = input$map_spacePlot_center$lat,
								zoom = input$map_spacePlot_zoom
						)
						
						
					}) 
			
			
			# Download the map
			output$map_download <- downloadHandler(
					filename = function()
						nameFile(species = input$showSpecies,
								year = input$map_year[1], 
								content = "kaart", fileExt = "png"),
					content = function(file) {
						
#						htmlwidgets::saveWidget(widget = results$finalMap(), 
#								file = file.path(tempdir(), "plotRuimte.html"), selfcontained = FALSE)
#						webshot::webshot(file.path(tempdir(), "plotRuimte.html"), file = file, 
#								vwidth = 1000, vheight = 500, cliprect = "viewport", zoom = 3)
						mapview::mapshot(x = results$finalMap(), file = file,
								vwidth = 1000, vheight = 500, cliprect = "viewport")
						
					}
			)
			
			output$map_downloadData <- downloadHandler(
					filename = function()
						nameFile(species = input$showSpecies,
								year = input$map_year[1], 
								content = "kaartData", fileExt = "csv"),
					content = function(file) {
						
						## write data to exported file
						write.table(x = results$map_spaceData(), file = file, quote = FALSE, row.names = FALSE,
								sep = ";", dec = ",")
						
					})
					
			
		})
