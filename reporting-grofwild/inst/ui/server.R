# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


library(leaflet)
library(rCharts)
#library(reportingGrofwild)
library(plotly)

library(INBOtheme)

`%then%` <- shiny:::`%OR%`



## Load all data
allSpatialData <- loadShapeData()

ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo", shapeData = allSpatialData)



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
      
      ## Create data upon user choices
      results$wildEcoData <- reactive({
            
            ecoData[ecoData$wildsoort == input$showSpecies, ]
            
          })
      
      
      results$wildGeoData <- reactive({
            
            geoData[geoData$wildsoort == input$showSpecies, ]
            
          })
      
      
      results$spatialData <- reactive({
            
            if (input$showSpecies == "Wild zwijn" & input$map_regionLevel == "provinces") {
              
              allSpatialData$provincesVoeren
              
            } else {
              
              allSpatialData[[input$map_regionLevel]]
              
            }
            
          })
      
      
      
      ## User input for controlling the plots and create plotly
      # Plot 1
      callModule(module = optionsModuleServer, id = "plot1", 
          data = results$wildEcoData)
      callModule(module = plotModuleServer, id = "plot1",
          plotFunction = "countYearProvince", 
          data = results$wildEcoData, 
          wildNaam = input$showSpecies)
      
      
      # Plot 2
      callModule(module = optionsModuleServer, id = "plot2", 
          data = results$wildEcoData)
      callModule(module = plotModuleServer, id = "plot2",
          plotFunction = "countAgeCheek", 
          data = results$wildEcoData,
          wildNaam = input$showSpecies)
      
      
      # Plot 3
      callModule(module = optionsModuleServer, id = "plot3", 
          data = results$wildEcoData)
      callModule(module = plotModuleServer, id = "plot3",
          plotFunction = "countYearAge", 
          data = results$wildEcoData,
          wildNaam = input$showSpecies)
      
      
      ### Plot 3 ###
      
      # Data-dependent input fields
      output$map_time <- renderUI({
            
            sliderInput(inputId = "map_time", label = "Tijdstip(pen)", 
                value = c(min(results$wildGeoData()$afschotjaar), 
                    max(results$wildGeoData()$afschotjaar)),
                min = min(results$wildGeoData()$afschotjaar),
                max = max(results$wildGeoData()$afschotjaar),
                step = 1,
                sep = "")
            
          })
      
      
      observeEvent(input$map_regionLevel, {
            
            output$map_region <- renderUI({
                  
                  selectInput(inputId = "map_region", label = "Regio('s)",
                      choices = results$spatialData()$NAAM, multiple = TRUE)
                  
                })
            
          })
      
      
      
      
      # Create data for map, time plot
      results$map_timeData <- reactive({
            
            validate(need(results$wildGeoData(), "Geen data beschikbaar"),
                need(input$map_time, "Gelieve tijdstip(pen) te selecteren"))
            
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
            newData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
            newData$freq[is.na(newData$freq)] <- 0
            
            
            return(newData)
            
          })
      
      
      # Create data for map, spatial plot
      results$map_spaceData <- reactive({
            
            plotData <- results$map_timeData()
            
            summaryData <- plyr::count(df = plotData, vars = "locatie", wt_var = "freq")
            
            # Create group variable
            summaryData$group <- cut(x = summaryData$freq, 
                breaks = c(-Inf, 0, 10, 20, 40, 70, Inf),
                labels = c("0", "1-10", "11-20", "21-40", "41-70", ">70"))
            
            return(summaryData)
            
          })
      
      
#      # For checking errors in the data
#      output$table1 <- renderDataTable({results$map_spaceData()})
#      output$table2 <- renderDataTable({results$map_timeData()})
      
      # TODO update input fields needs debugging
      
#output$print <- renderPrint({
#      
#      print(input$map_regionLevel)
#      
#            print(length(results$spatialData()))
#            input$map_region
#      
#    })
#
      # Which region(s) are selected?
      observe({
            
            event <- input$map_spacePlot_shape_click
              
              if (!is.null(event)) {
                
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
              
          })
      
      
      # Define text to be shown in the pop-ups
      results$map_textPopup <- reactive({
            
            validate(need(results$map_spaceData(), "Geen data beschikbaar"))
            
            regionNames <- results$map_spaceData()$locatie
            titleText <- paste("Geobserveerd aantal",            
                ifelse(input$map_time[1] != input$map_time[2],
                    paste("van", input$map_time[1], "tot", input$map_time[2]),
                    paste("in", input$map_time[1])
                )
            )
            
            textPopup <- paste0("<h4>", regionNames, "</h4>",  
                "<strong>", titleText, "</strong>: ", 
                round(results$map_spaceData()$freq, 2)
            )
            
            
            return(textPopup)
            
          })
      
      
      # Define colors for the polygons
      results$map_colors <- reactive({
            
            validate(need(nrow(results$map_spaceData()) > 0, "Geen data beschikbaar"))
            
            palette <- colorFactor(palette = inbo.2015.colours(nlevels(results$map_spaceData()$group)), 
                domain = levels(results$map_spaceData()$group))
            
            valuesPalette <- results$map_spaceData()[
                match(results$spatialData()$NAAM, results$map_spaceData()$locatie),
                "group"]
            
            palette(valuesPalette)
            
          })
      
      
      # Send map to the UI
      output$map_spacePlot <- renderLeaflet({
            
            validate(need(results$spatialData(), "Geen data beschikbaar"),
                need(nrow(results$map_spaceData()) > 0, "Geen data beschikbaar"))
            
            provinceBounds <- switch(input$map_regionLevel,
                "flanders" = list(color = NULL, opacity = 0), 
                "provinces" = list(color = "white", opacity = 0),
                "communes" = list(color = "black", opacity = 0.8))            
            
            
            leaflet(results$spatialData()) %>%
                
                addPolygons(
                    weight = 1, 
                    color = "white",
                    fillColor = ~ results$map_colors(),
                    fillOpacity = 0.8,
                    layerId = results$spatialData()$NAAM,
                    group = "region"
                ) %>%
                
                addPolylines(
                    data = allSpatialData$provinces, 
                    color = provinceBounds$color, 
                    weight = 3,
                    opacity = provinceBounds$opacity)
            
          })
      
      
      # Plot thick border for selected regions
      observe({
            
            if (!is.null(input$map_region)) {
              
              validate(need(results$spatialData(), "Geen data beschikbaar"))
              
              selectedPolygons <- subset(results$spatialData(), 
                  results$spatialData()$NAAM %in% input$map_region)
              
              leafletProxy("map_spacePlot", data = results$spatialData()) %>%
                  
                  clearGroup(group = "regionLines") %>%
                  
                  addPolylines(data = selectedPolygons, color = "white", weight = 5,
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
              
              palette <- colorFactor(palette = inbo.2015.colours(nlevels(results$map_spaceData()$group)), 
                  domain = levels(results$map_spaceData()$group))
              
              valuesPalette <- results$map_spaceData()[
                  match(results$spatialData()$NAAM, results$map_spaceData()$locatie),
                  "group"]
              
              
              proxy %>% addLegend(
                  position = input$map_legend,
                  pal = palette, 
                  values = valuesPalette,
                  opacity = 0.8,
                  title = "Legend",
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
              
              textSelected <- results$map_textPopup()[
                  results$map_spaceData()$locatie == event$id]
              
              isolate({
                    
                    currentMap %>% 
                        addPopups(event$lng, event$lat, popup = textSelected)
                    
                  }) 
              
            }
            
          })
      
      
      # Title for the map
      output$map_title <- renderUI({
            
            h4(paste("Geobserveerd aantal voor", input$showSpecies,
                    ifelse(input$map_time[1] != input$map_time[2],
                        paste("van", input$map_time[1], "tot", input$map_time[2]),
                        paste("in", input$map_time[1])
                    )
                ))
            
          })
      
      
      # Create final map (for download)
      results$finalMap <- reactive({
            
            validate(need(results$map_spaceData(), "Geen data beschikbaar"))
            
            
            palette <- colorFactor(palette = inbo.2015.colours(nlevels(results$map_spaceData()$group)), 
                domain = levels(results$map_spaceData()$group))
            
            valuesPalette <- results$map_spaceData()[
                match(results$spatialData()$NAAM, results$map_spaceData()$locatie),
                "group"]
            
            newMap <- leaflet(results$spatialData())
            
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
                color = "white",
                fillColor = ~ results$map_colors(),
                fillOpacity = 0.8,
                layerId = results$spatialData()$NAAM,
                group = "region"
            )
            
            if (input$map_regionLevel == "communes") {  
              
              newMap <- addPolylines(newMap,
                  data = allSpatialData$provinces,
                  color = "black", 
                  weight = 3,
                  opacity = 0.8, 
                  group = "provinceLines")
              
            }
            
            newMap
            
          })
      
      
      output$downloadPlotSpace <- downloadHandler("plotRuimte.png",
          content = function(file) {
            
            htmlwidgets::saveWidget(widget = results$finalMap(), 
                file = file.path(tempdir(), "plotRuimte.html"), selfcontained = FALSE)
            webshot::webshot(file.path(tempdir(), "plotRuimte.html"), file = file, 
                cliprect = "viewport")
            
          }
      )
      
      
      
      # Interactive time plot
      results$map_timePlot <- reactive({
            
            validate(need(results$map_timeData(), "Geen data beschikbaar"),
                need(input$map_region, "Gelieve regio('s) te selecteren"))
            
            currentData <- results$map_timeData()
            
            plotTime <- Highcharts$new()
            
            timePointsSelected <- sort(unique(currentData$afschotjaar))
            
            plotTime$xAxis(categories = timePointsSelected, 
                labels = list(rotation = -45, align = 'right'))
            plotTime$chart(width = 600)
            
            plotTime$title(
                text = paste("Geobserveerd aantal voor", input$showSpecies,
                    ifelse(input$map_time[1] != input$map_time[2],
                        paste("van", input$map_time[1], "tot", input$map_time[2]),
                        paste("in", input$map_time[1])
                    )
                )
            )
            
            
            currentData$y <- currentData$freq      
            
#            Code for adapting colors (if needed)
#            https://stackoverflow.com/questions/26507326/rcharts-change-the-individual-point-colors-of-a-time-series-plot-highcharts
            plotTime$series(
                lapply(input$map_region, function(iArea) {
                      
                      list(name = iArea, data = toJSONArray2(currentData[
                                  currentData$locatie == iArea, ], json = F))
                      
                    })
            )
            
            
            plotTime$save(file.path(tempdir(), "plotTijd.html"),
                standalone = TRUE)
            
            
            plotTime
            
          })
      
      
      output$map_timePlot <- renderChart2({results$map_timePlot()})
      
      
      output$downloadPlotTime <- downloadHandler("plotTijd.html",
          content = function(file) {
            file.copy(file.path(tempdir(), "plotTijd.html"), file)
          }
      )
      
      
      
#      
#      # export the results as an html report
#      output$exportResults <- downloadHandler(
#          
#          filename = 'grofWild_results.html',
#          
#          content = function(file) {
#            
#            # extract parameters
#            params <- list(
#                
#                # input parameters
#                spatialLevel = input$spatialLevel,
#                specie = input$showSpecies,
#                times = input$showTime,
#                regions = input$showRegion,
#                
#                # map
#                map = results$finalMap(),
#                
#                # profile plot
#                interactiveTime = results$interactiveTime()
#            
#            )
#            
      ##			message("params: ", str(params))
#            
#            # get path template report
#            pathReport <- grofWild::getPathReport()
#            pathCss <- grofWild::getPathCss()
#            
#            # get report name
#            reportName <- basename(pathReport)
#            
#            # create temporary files in temp
#            tmpDir <- tempdir()
#            dir.create(tmpDir, recursive = TRUE)
      ##			message("File", pathReport, "copied to", tmpDir)
#            
#            # copy start template in working directory
#            file.copy(from = pathReport, to = tmpDir, overwrite = TRUE)
#            file.copy(from = pathCss, to = tmpDir, overwrite = TRUE)
#            
#            # run report
#            library(rmarkdown)
#            potentialErrorMessage <- try(
#                res <- rmarkdown::render(
#                    file.path(tmpDir, reportName), params = params
#                )
#                , silent = TRUE)
#            
#            # print message
#            if(inherits(potentialErrorMessage, "try-error"))
#              message("Error during exporting results:", potentialErrorMessage)
#            
#            # return the report file
#            pathHtmlReport <- file.path(tmpDir, sub("Rmd", "html", reportName))
#            file.copy(pathHtmlReport, file)
#            
#            message("The html report is available at:", pathHtmlReport)
#            
#            # clean directory
      ##			unlink(tmpDir)
#            
#          }, 
#          
#          contentType = "text/html"
#      
#      )
      
      
    })
