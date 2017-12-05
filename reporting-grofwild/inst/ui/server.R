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
            
            
            results$showSpecies
            
          })
      
      
      ## For which species should summaries be shown?
      observeEvent(input$showSpecies1, results$showSpecies <- "Wild zwijn")
      observeEvent(input$showSpecies2, results$showSpecies <- "Ree")
      observeEvent(input$showSpecies3, results$showSpecies <- "Damhert")
      observeEvent(input$showSpecies4, results$showSpecies <- "Edelhert")
      
      
      ## Create data upon user choices
      results$wildEcoData <- reactive({
            
            ecoData[ecoData$wildsoort == results$showSpecies, ]
            
          })
      
      
      results$wildGeoData <- reactive({
            
            req(geoData)
            
            geoData[geoData$wildsoort == results$showSpecies, ]
            
          })
      
      
      results$afschotData <- reactive({
            
            results$wildEcoData()[results$wildEcoData()$doodsoorzaak == "afschot", ]
            
          })
      
      
      results$spatialData <- reactive({
            
            req(spatialData)
            
            if (results$showSpecies == "Wild zwijn" & input$map_regionLevel == "provinces") {
              
              spatialData$provincesVoeren
              
            } else {
              
              spatialData[[input$map_regionLevel]]
              
            }
            
          })
      
      
      results$openingstijdenData <- reactive({
            
            openingstijdenData[openingstijdenData$Soort == results$showSpecies, ]
            
          })
      
      
      results$openingstijd <- reactive({
            
            # for Ree: openingseason contains more year than in the data
            # for Wildboar: openingseason contains less year than in the data
            
            # so retains the years when data and opening season specified
            # and doesn't retain the last year (because not full)
            
            if (results$showSpecies %in% c("Ree", "Wild zwijn")) {
              
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
              
#						message("openingstijd is: ", toString(openingstijd))
              
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
          wildNaam = reactive(results$showSpecies),
          categorie = "leeftijd")
      
      
      # Table 2 - input
      callModule(module = optionsModuleServer, id = "table2", 
          data = results$afschotData,
          timeRange = results$timeRange)
      # Table 3 - input
      callModule(module = optionsModuleServer, id = "table3", 
          data = results$wildEcoData,
          timeRange = results$timeRange)
      
      
      observe({
            
            if (results$showSpecies == "Ree") {
              
              # Table 2 - output
              callModule(module = plotModuleServer, id = "table2",
                  plotFunction = "tableProvince", 
                  data = results$afschotData, 
                  wildNaam = reactive(results$showSpecies),
                  categorie = "typeAantal")
              
              
              # Table 3 - output
              callModule(module = plotModuleServer, id = "table3",
                  plotFunction = "tableProvince", 
                  data = results$afschotData,
                  toekenningsData = reactive(toekenningsData), 
                  wildNaam = reactive(results$showSpecies),
                  categorie = "typePercent")
              
            }
            
          })
      
      
      # Plot 1
      callModule(module = optionsModuleServer, id = "plot1", 
          data = results$wildEcoData,
          timeRange = reactive(if (results$showSpecies == "Edelhert")
                    c(2008, max(results$timeRange())) else 
                    results$timeRange()))
      callModule(module = plotModuleServer, id = "plot1",
          plotFunction = "countYearProvince", 
          data = results$wildEcoData, 
          wildNaam = reactive(results$showSpecies))
      
      
      # Plot 2
      callModule(module = optionsModuleServer, id = "plot2", 
          data = results$wildEcoData,
          timeRange = reactive(if (results$showSpecies == "Ree")
                    c(2014, max(results$timeRange())) else 
                    results$timeRange()))
      callModule(module = plotModuleServer, id = "plot2",
          plotFunction = "countAgeCheek", 
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
      
      # Plot 3
      callModule(module = optionsModuleServer, id = "plot3", 
          data = results$wildEcoData,
          timeRange = results$timeRange)
      callModule(module = plotModuleServer, id = "plot3",
          plotFunction = "countYearAge", 
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
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
          # TODO change to true? see GIT issue #32
          multipleTypes = FALSE)
      
      callModule(module = plotModuleServer, id = "plot4",
          plotFunction = "percentageYearlyShotAnimals", 
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies),
          openingstijdenData = results$openingstijdenData)
      
      
      # Plot 5
      callModule(module = optionsModuleServer, id = "plot5", 
          data = results$wildEcoData,
          timeRange = results$timeRange)
      callModule(module = plotModuleServer, id = "plot5",
          plotFunction = "countAgeGender", 
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
      
      # Plot 6
      callModule(module = optionsModuleServer, id = "plot6", 
          data = results$wildEcoData,
          timeRange = reactive(if (results$showSpecies == "Ree")
                    c(2014, max(results$timeRange())) else 
                    results$timeRange()))
      callModule(module = plotModuleServer, id = "plot6",
          plotFunction = "boxAgeWeight", 
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
      
      # Plot 7
      callModule(module = optionsModuleServer, id = "plot7", 
          data = results$wildEcoData,
          timeRange = reactive(if (results$showSpecies == "Ree")
                    c(2014, max(results$timeRange())) else 
                    results$timeRange()))
      callModule(module = plotModuleServer, id = "plot7",
          plotFunction = "boxAgeGenderLowerJaw", 
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
      
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
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
      
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
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
      
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
          data = results$wildEcoData,
          wildNaam = reactive(results$showSpecies))
      
      
      
      
      
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
      
      
      
      
      # Create data for map, time plot
      results$map_timeData <- reactive({
            
            validate(need(results$wildGeoData(), "Geen data beschikbaar"),
                need(input$map_time, "Gelieve periode te selecteren"))
            
            # Select subset for time
            chosenTimes <- input$map_time[1]:input$map_time[2]
            tmpData <- subset(results$wildGeoData(), afschotjaar %in% chosenTimes)
            
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
            
            
            return(allData)
            
          })
      
      
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
            summaryData2$group <- cut(x = summaryData2$freq, 
                breaks = c(-Inf, 0, 10, 20, 40, 70, Inf),
                labels = c("0", "1-10", "11-20", "21-40", "41-70", ">70"))
            
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
            
            withProgress(message = "Kaart laden...", value = 0.5, {                  
                  
                  req(spatialData)
                  
                  validate(need(results$spatialData(), "Geen data beschikbaar"),
                      need(nrow(results$map_spaceData()) > 0, "Geen data beschikbaar"))
                  
                  provinceBounds <- switch(input$map_regionLevel,
                      "flanders" = list(opacity = 0), 
                      "provinces" = list(opacity = 0),
                      "communes" = list(color = "black", opacity = 0.8))            
                  
                  
                  leaflet(results$spatialData()) %>%
                      
                      setView(lng = flandersRange$lng, lat = flandersRange$lat,
                          zoom = 8.5) %>%
                      
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
            
            h3(paste("Gerapporteerd aantal voor", tolower(results$showSpecies),
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
            
            if (input$map_globe %% 2 == 1) {
              
              newMap <- addProviderTiles(newMap, "Hydda.Full")
              
            } 
            
            if (input$map_legend != "none") { 
              
              newMap <- addLegend(newMap,
                  position = input$map_legend,
                  pal = palette, 
                  values = valuesPalette,
                  opacity = 0.8,
                  title = "Legend",
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
            
            newMap
            
          })
      
      
      output$map_download <- downloadHandler(
          filename = function()
            nameFile(species = results$showSpecies,
                year = input$map_year[1], 
                content = "kaart", fileExt = "png"),
          content = function(file) {
            
            htmlwidgets::saveWidget(widget = results$finalMap(), 
                file = file.path(tempdir(), "plotRuimte.html"), selfcontained = FALSE)
            webshot::webshot(file.path(tempdir(), "plotRuimte.html"), file = file, 
                vwidth = 1000, vheight = 500, cliprect = "viewport", zoom = 3)
            
          }
      )
      
      output$map_downloadData <- downloadHandler(
          filename = function()
            nameFile(species = results$showSpecies,
                year = input$map_year[1], 
                content = "kaartData", fileExt = "csv"),
          content = function(file) {
            
            ## write data to exported file
            write.table(x = results$map_spaceData(), file = file, quote = FALSE, row.names = FALSE,
                sep = ";", dec = ",")
            
          })
      
      
      ## Create time plot for Flanders (reference) 
      output$map_timePlotFlanders <- renderPlotly({
            
            validate(need(input$map_time, "Gelieve periode te selecteren"))
            
            ## Get data for Flanders
            # Select subset for time
            chosenTimes <- input$map_time[1]:input$map_time[2]
            tmpData <- subset(results$wildGeoData(), afschotjaar %in% chosenTimes)
            
            # Create general plot data names
            plotData <- data.frame(afschotjaar = tmpData$afschotjaar)
            plotData$locatie <- "Vlaams Gewest"
            
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
            
            allData$afschotjaar <- as.factor(allData$afschotjaar)
            
            
            title <- paste("Gerapporteerd aantal voor", tolower(results$showSpecies),
                ifelse(input$map_time[1] != input$map_time[2],
                    paste("van", input$map_time[1], "tot", input$map_time[2]),
                    paste("in", input$map_time[1])
                )
            )
            
            ## Create plot
            toPlot <- plot_ly(data = allData, x = ~afschotjaar, y = ~freq,
                    color = ~locatie, hoverinfo = "x+y+name",
                    type = "scatter", mode = "lines+markers") %>%
                layout(title = title,
                    xaxis = list(title = "Jaar"), 
                    yaxis = list(title = "Aantal"),
                    showlegend = TRUE,
                    margin = list(b = 80, t = 100))
            
            # To prevent warnings in UI
            toPlot$elementId <- NULL
            
            
            toPlot
            
          })
      
      
      
      ## Create time plot for selected regions
      output$map_timeTitle <- renderUI({
            
            regionLevel <- switch(input$map_regionLevel,
                "flanders" = "Vlaanderen",
                "provinces" = "Provincie",
                "communes" = "Gemeente")
            
            h3("Regio-schaal:", regionLevel)
            
          })
      
      output$map_timePlot <- renderPlotly({
            
            validate(need(results$map_timeData(), "Geen data beschikbaar"),
                need(input$map_region, "Gelieve regio('s) te selecteren"))
            
            
            title <- paste("Gerapporteerd aantal voor", tolower(results$showSpecies),
                ifelse(input$map_time[1] != input$map_time[2],
                    paste("van", input$map_time[1], "tot", input$map_time[2]),
                    paste("in", input$map_time[1])
                )
            )
            
            allData <- subset(results$map_timeData(), locatie %in% input$map_region)
            
            
            # Create plot
            toPlot <- plot_ly(data = allData, x = ~afschotjaar, y = ~freq,
                    color = ~locatie, hoverinfo = "x+y+name",
                    type = "scatter", mode = "lines+markers") %>%
                layout(title = title,
                    xaxis = list(title = "Jaar"), 
                    yaxis = list(title = "Aantal"),
                    showlegend = TRUE,
                    margin = list(b = 80, t = 100))     
            
            # To prevent warnings in UI
            toPlot$elementId <- NULL
            
            
            toPlot
            
          })
      
      
      
      
      
      
      ## CONDITIONAL PANELS FOR UI ##
      ## ------------------------- ##
      
      ## tableProvince for "leeftijd": wild zwijn and ree
      output$showTableProvince <- renderUI({
            
            if (!results$showSpecies %in% c('Wild zwijn', 'Ree'))
              return(NULL)
            
            
            list(
                actionLink(inputId = "linkTable1",
                    label = h3("TABEL: Gerapporteerd afschot per regio en per leeftijdscategorie")),
                conditionalPanel("input.linkTable1 % 2 == 1",
                    
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "table1", showYear = TRUE, exportData = TRUE),
                            tags$p("Het gerapporteerd aantal geschoten of dood gevonden dieren per provincie en per leeftijdscategorie voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren. Voor everzwijn werd de leeftijfscategorie bepaald op basis van de ingezamelde onderkaken. Voor ree is deze afkomstig van het meldingsformulier.")
                        ),
                        column(8, tableModuleUI(id = "table1"))
                    
                    ),
                    tags$hr()
                )
            
            )
            
          })
      
      
      
      ## tableProvince for "type": ree
      output$showTableProvince2 <- renderUI({
            
            if (results$showSpecies != 'Ree')
              return(NULL)
            
            
            list(
                
                actionLink(inputId = "linkTable2",
                    label = h3("TABEL: Gerapporteerd afschot per regio en per type")),
                conditionalPanel("input.linkTable2 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "table2", showYear = TRUE, exportData = TRUE),
                            tags$p("Het gerapporteerd aantal geschoten dieren per provincie en per labeltype voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.")
                        ),
                        column(8, tableModuleUI(id = "table2"))
                    
                    ),
                    tags$hr()
                ),
                
                actionLink(inputId = "linkTable3",
                    label = h3("TABEL: Percentage gerealiseerd afschot per regio en per type")),
                conditionalPanel("input.linkTable3 % 2 == 1",
                    
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "table3", showYear = TRUE, exportData = TRUE),
                            tags$p("Realisatiegraad per provincie en per labeltype voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.")
                        ),
                        column(8, tableModuleUI(id = "table3"))
                    
                    ),
                    tags$hr()
                )
            )
            
          })
      
      
      ## countAgeCheek & countYearAge & percentageYearlyShotAnimals
      ## countAgeGender & boxAgeWeight
      ## - Wild zwijn and Ree
      output$showPlotsWild_Ree <- renderUI({
            
            if (!results$showSpecies %in% c('Wild zwijn', 'Ree'))
              return(NULL)
            
            
            list(
                actionLink(inputId = "linkPlot2", label =
                        h3("FIGUUR: Leeftijdscategorie op basis van onderkaak & meldingsformulieren")),
                conditionalPanel("input.linkPlot2 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot2", showTime = TRUE, exportData = TRUE),
                            tags$p("Vergelijking tussen de leeftijd zoals aangeduid op het meldingsformulier en de leeftijd bepaalt op basis van een ingezamelde onderkaak")
                        ),
                        column(8, plotModuleUI(id = "plot2"))
                    
                    ),
                    tags$hr()
                ),
                
                
                actionLink(inputId = "linkPlot3",
                    label = h3("FIGUUR: Afschot per jaar en per leeftijdscategorie (o.b.v. onderkaak)")),
                conditionalPanel("input.linkPlot3 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot3",
                                showSummarizeBy = TRUE, showTime = TRUE,
                                regionLevels = 1:2, exportData = TRUE),
                            tags$p("Evolutie van de verdeling van het afschot over de verschillende leeftijdscategorieën doorheen de jaren op basis van de ingezamelde onderkaak."),
                            conditionalPanel("input.showSpecies == 'Ree'",
                                "Voor mannelijke reeën zijn de leeftijdscategorieën 'jongvolwassen' en 'volwassen' uitgesloten.")
                        ),
                        column(8, plotModuleUI(id = "plot3"))
                    ),
                    tags$hr()
                ),
                
                
                actionLink(inputId = "linkPlot4",
                    label = h3("FIGUUR: Percentage jaarlijks afschot")),
                conditionalPanel("input.linkPlot4 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot4",
                                showTime = TRUE, showYear = TRUE,
                                showType = TRUE, exportData = TRUE),
                            tags$p("Procentuele verdeling van het afschot (voor ree per labeltype) doorheen het openingsseizoen van het geselecteerde jaar in verhouding tot de verdeling in de geselecteerde referentieperiode.")
                        ),
                        column(8, plotModuleUI(id = "plot4"))
                    
                    ),
                    tags$hr()
                ),
                
                
                actionLink(inputId = "linkPlot5",
                    label = h3("FIGUUR: Geslachtsverdeling binnen het afschot per leeftijdscategorie")),
                conditionalPanel("input.linkPlot5 % 2 == 1",
                    
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot5", showTime = TRUE, exportData = TRUE),
                            tags$p("Geslachtsverdeling per leeftijdscategorie voor de geselecteerde periode. Voor everzwijn werd de leeftijdscategorie bepaald op basis van de ingezamelde onderkaak. Voor ree is dit de leeftijdscategorie vermeld op het meldingsformulier.")
                        ),
                        column(8, plotModuleUI(id = "plot5"))
                    
                    ),
                    tags$hr()
                ),
                
                
                actionLink(inputId = "linkPlot6",
                    label = h3("FIGUUR: Leeggewicht per leeftijdscategorie (o.b.v. onderkaak) en geslacht")),
                conditionalPanel("input.linkPlot6 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot6", showTime = TRUE, regionLevels = 1:2, exportData = TRUE),
                            tags$p("Verdeling van de leeggewichten per leeftijdscategorie en per geslacht voor alle gegevens uit de geselecteerde periode."),
                            conditionalPanel("input.showSpecies == 'Ree'",
                                "Voor ree: geen data beschikbaar voor 2014.")
                        ),
                        column(8, plotModuleUI(id = "plot6"))
                    
                    ),
                    tags$hr()
                )
            
            )
            
            
          })
      
      
      ## boxAgeGenderLowerJaw
      ## - Ree
      output$showPlotsRee <- renderUI({
            
            if (results$showSpecies != "Ree")
              return(NULL)
            
            
            list(
                
                actionLink(inputId = "linkPlot7",
                    label = h3("FIGUUR: Onderkaaklengte per leeftijdscategorie (o.b.v. onderkaak) en geslacht")),
                conditionalPanel("input.linkPlot7 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot7", showTime = TRUE,
                                regionLevels = 1:2, exportData = TRUE),
                            tags$p("Verdeling van de onderkaaklengte per leeftijdscategorie en per geslacht voor alle gegevens uit de geselecteerde periode.")),
                        column(8, plotModuleUI(id = "plot7"))
                    
                    ),
                    tags$hr()
                ),
                
                h3("Bio-indicatoren"),
                
                actionLink(inputId = "linkPlot8",
                    label = h3("FIGUUR: Onderkaaklengte per jaar")),
                conditionalPanel("input.linkPlot8 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot8", showTime = TRUE, showType = TRUE,
                                regionLevels = 1:2, exportData = TRUE,
                                showDataSource = TRUE),
                            tags$p("Evolutie van de gemodelleerde onderkaaklengte (met 95% betrouwbaarheidsinterval) doorheen de geselecteerde jaren voor de gekozen regio en types.")
                        ),
                        column(8, plotModuleUI(id = "plot8"))
                    ),
                    tags$hr()
                ),
                
                actionLink(inputId = "linkPlot9", label = h3("FIGUUR: Gewicht per jaar")),
                conditionalPanel("input.linkPlot9 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot9",
                                showTime = TRUE, showType = TRUE,
                                regionLevels = 1:2, exportData = TRUE),
                            tags$p("Evolutie van de gemodelleerde leeggewichten (met 95% betrouwbaarheidsinterval) doorheen de geselecteerde jaren voor de gekozen regio en types."),
                            tags$p("Observaties met leeggewicht < 5kg of > 25kg zijn niet opgenomen in de figuur.")
                        ),
                        column(8, plotModuleUI(id = "plot9"))
                    
                    ),
                    tags$hr()
                ),
                
                actionLink(inputId = "linkPlot10",
                    label = h3("FIGUUR: Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar")
                ),
                conditionalPanel("input.linkPlot10 % 2 == 1",
                    fluidRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot10", showTime = TRUE, showType = TRUE,
                                regionLevels = 1:2, exportData = TRUE),
                            tags$p("Evolutie van het gerapporteerd aantal embryo's per geschoten dier doorheen de geselecteerde jaren voor de gekozen regio en types. Voor 2013 kon nul embryo's niet ingevuld worden, waardoor er geen onderscheid gemaakt kon worden tussen niet drachtig en niet ingevuld."),
                            tags$p("Observaties met meer dan 3 embryo's zijn niet opgenomen in de figuur.")),
                        column(8, plotModuleUI(id = "plot10"))
                    
                    )
                )
            
            )
            
          })
      
      
      
      
    })
