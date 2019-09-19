# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



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
						
						if (doDebug)
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
					data = results$wildEcoData,
					timeRange = results$timeRange)
			callModule(module = plotModuleServer, id = "table1",
					plotFunction = "tableProvince", 
					data = results$wildEcoData, 
					categorie = "leeftijd")
			
			
#			# Table 2 - input
#			callModule(module = optionsModuleServer, id = "table2", 
#					data = results$wildEcoData,
#					timeRange = results$timeRange)
#			# Table 3 - input
#			callModule(module = optionsModuleServer, id = "table3", 
#					data = results$wildEcoData,
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
#									data = results$wildEcoData,
#									categorie = "typeAantal")
#							
#							
#							# Table 3 - output
#							callModule(module = plotModuleServer, id = "table3",
#									plotFunction = "tableProvince", 
#									data = results$wildEcoData,
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
					labelTypes = "Leeftijdscategorie",
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
					types = reactive(switch(input$showSpecies,
									"Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
									Ree = c("Kits", "Jongvolwassen", "Volwassen")									
							)),
					labelTypes = "Leeftijdscategorie",
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
										min = if (input$map_regionLevel %in% c("faunabeheerzones", "fbz_gemeentes"))
													2014 else
													min(results$wildGeoData()$afschotjaar),
										max = max(results$wildGeoData()$afschotjaar),
										value = 2016,
										sep = "", step = 1))
						
					})
			
			
			output$map_time <- renderUI({
						
						minYear <- if (input$map_regionLevel %in% c("faunabeheerzones", "fbz_gemeentes"))
									2014 else
									min(results$wildGeoData()$afschotjaar)
						
						sliderInput(inputId = "map_time", label = "Periode (grafiek)", 
								value = c(minYear, 
										max(results$wildGeoData()$afschotjaar)),
								min = minYear,
								max = max(results$wildGeoData()$afschotjaar),
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
						
						# Add names & times with 0 observations
						fullData <- cbind(expand.grid(afschotjaar = unique(summaryData$afschotjaar),
										locatie = "Vlaams Gewest"))
						allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
						allData$freq[is.na(allData$freq)] <- 0
						
						# unit taken into account
						if (input$map_unit == "relative")
							allData$freq <- allData$freq/spatialData[["flanders"]]$AREA 
						
						
						allData$afschotjaar <- as.factor(allData$afschotjaar)
						
						allData
						
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
						
						validate(need(results$wildGeoData(), "Geen data beschikbaar"),
								need(input$map_time, "Gelieve periode te selecteren"))
						
						
						# TODO incorporate this code in createSpaceData()
						
						# Select subset for time
						chosenTimes <- input$map_time[1]:input$map_time[2]
						tmpData <- subset(results$wildGeoData(), afschotjaar %in% chosenTimes)
						
						# Create general plot data names
						plotData <- data.frame(
								wildsoort = input$showSpecies,
								afschotjaar = tmpData$afschotjaar)
						plotData$locatie <- switch(input$map_regionLevel,
								flanders = "Vlaams Gewest",
								provinces = tmpData$provincie,
								communes = tmpData$gemeente_afschot_locatie,
								faunabeheerzones = tmpData$FaunabeheerZone,
								fbz_gemeentes = tmpData$fbz_gemeente
						)
						
						# Exclude data with missing time or space
						plotData <- plotData[!is.na(plotData$afschotjaar) & 
										!is.na(plotData$locatie) & plotData$locatie != "",]
						
						# Summarize data over years
						summaryData <- plyr::count(df = plotData, vars = names(plotData))
						
						# Add names & times with 0 observations
						fullData <- cbind(expand.grid(
										wildsoort = input$showSpecies,
										afschotjaar = unique(summaryData$afschotjaar),
										locatie = unique(results$spatialData()$NAAM)))
						# add Area
						fullData <- merge(fullData, results$spatialData()@data[, c("NAAM", "AREA")],
							by.x = "locatie", by.y = "NAAM")
					
						allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
						allData$freq[is.na(allData$freq)] <- 0
						
						# unit taken into account
						if (input$map_unit == "relative")
							allData$freq <- allData$freq/allData$AREA 
						
						allData$AREA <- NULL
						
						allData$afschotjaar <- as.factor(allData$afschotjaar)
						
						
						return(allData)
						
					})
			
			# Title for selected region level
			output$map_timeTitle <- renderUI({
						
						regionLevel <- switch(input$map_regionLevel,
								"flanders" = "Vlaanderen",
								"provinces" = "Provincie",
								"faunabeheerzones" = "Faunabeheerzones",
								"communes" = "Gemeente (binnen provincie)",
								"fbz_gemeentes" = "Gemeente (binnen faunabeheerzone)")
						
						
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
						
						validate(need(results$wildGeoData(), "Geen data beschikbaar"),
								need(input$map_year, "Gelieve jaar te selecteren"))
						
						
						createSpaceData(
								data = geoData, 
								allSpatialData = spatialData,
								year = input$map_year,
								species = input$showSpecies,
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
						
						validate(need(results$map_summarySpaceData(), "Geen data beschikbaar"))
						
						regionNames <- results$map_summarySpaceData()$locatie
						titleText <- paste("Gerapporteerd", input$map_unit, "in", input$map_year[1])
						
						textPopup <- paste0("<h4>", regionNames, "</h4>",  
								"<strong>", titleText, "</strong>: ", 
								round(results$map_summarySpaceData()$freq, 2)
						)
						
						
						return(textPopup)
						
					})
			
			
			# Define colors for the polygons
			results$map_colorScheme <- reactive({
						
						# Might give warnings if n < 3
						suppressWarnings(c("white", RColorBrewer::brewer.pal(
												n = nlevels(results$map_summarySpaceData()$group) - 1, name = "YlOrBr")))
						
					})			
			
			
			# Send map to the UI
			output$map_spacePlot <- renderLeaflet({
						
						req(spatialData)
						
						validate(need(results$spatialData(), "Geen data beschikbaar"),
								need(nrow(results$map_summarySpaceData()) > 0, "Geen data beschikbaar"))
						
						mapFlanders(
								regionLevel = input$map_regionLevel,
								species = input$showSpecies, 
								allSpatialData = spatialData,
								summaryData = results$map_summarySpaceData(),
								colorScheme = results$map_colorScheme()
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
						
						validate(need(nrow(results$map_summarySpaceData()) > 0, "Geen data beschikbaar"))
						
						req(input$map_legend)
						
						proxy <- leafletProxy("map_spacePlot", data = results$spatialData())
						proxy %>% removeControl(layerId = "legend")
						
						if (input$map_legend != "none") {
							
							palette <- colorFactor(palette = results$map_colorScheme(), 
									levels = levels(results$map_summarySpaceData()$group))
							
							valuesPalette <- results$map_summarySpaceData()[
									match(results$spatialData()$NAAM, results$map_summarySpaceData()$locatie),
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
							
								if (event$id %in% results$map_summarySpaceData()$locatie) {
									
									textSelected <- results$map_textPopup()[
											results$map_summarySpaceData()$locatie == event$id]
									
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
                                        "voor", tolower(input$showSpecies),
										"in", input$map_year[1]))
						
						
					})
			
			
			# Create final map (for download)
			results$finalMap <- reactive({
						
						validate(need(results$map_summarySpaceData(), "Geen data beschikbaar"))
						
						
						newMap <- mapFlanders(
								regionLevel = input$map_regionLevel, 
                                species = input$showSpecies,
								allSpatialData = spatialData,
								summaryData = results$map_summarySpaceData(),
								colorScheme = results$map_colorScheme(),
								legend = input$map_legend,
								addGlobe = input$map_globe %% 2 == 1
						)
						
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
						
						myData <- results$map_summarySpaceData()
						# change variable names
						names(myData)[names(myData) == "freq"] <- if (input$map_unit == "absolute")
									"aantal" else "aantal/100ha"
						names(myData)[names(myData) == "group"] <- "groep"
						
						## write data to exported file
						write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
								sep = ";", dec = ",")
						
					})
			
			
		})
