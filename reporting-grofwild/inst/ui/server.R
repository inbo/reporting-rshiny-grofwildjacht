library(leaflet)
library(rCharts)
library(reportingGrofwild)
library(plyr)

`%then%` <- shiny:::`%OR%`


dataDir <- system.file("extdata", package = "reportingGrofwild")

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
      
      
      # User input for controlling the plots
      callModule(module = figureModuleServer, id = "plot1", 
          data = reactive(countsData$province))
      callModule(module = figureModuleServer, id = "plot2", 
          data = reactive(countsData$province))
      
      
      # Create plot output
      output$plot1 <- renderPlot({
            
            plot(1:10, 1:10)
            
          })
      
      
      output$plot2 <- renderPlot({
            
            plot(1:10, 10:1)
            
          })
      
      
#      # Read shape data for provinces or communes
#      results$shapeData <- reactive({
#            
#            if (input$spatialLevel == "provinces")
#              provinceData else
#              communeData
#            
#          })
#      
#      
#      # Read counts data for provinces of communes
#      results$plotData <- reactive({
#            
#            switch(input$spatialLevel,
#                'provinces' = countsData$province,
#                'communes' = countsData$commune
#            )
#            
#          })
#      
#      
#      
#      # Select subset of the data
#      results$subsetPlotData <- reactive({
#            
#            validate(need(results$plotData(), "Geen data beschikbaar"),
#                need(input$showTime, "Gelieve tijdstip(pen) te selecteren"))
#            
#            if (input$showTime[1] != input$showTime[2])
#              chosenTimes <- input$showTime[1]:input$showTime[2] else
#              chosenTimes <- input$showTime[1]
#            
#            countsDataSpecieYear <- subset(
#                results$plotData(), 
#                specie == input$showSpecies & year %in% chosenTimes)
#            
#            return(countsDataSpecieYear)
#            
#          })
#      
#      # aggregate over years for map
#      results$subsetPlotDataAggr <- reactive({
#            
#            countsDataSpecieYearAggr <- if(
#                    length(input$showTime) > 1 && input$showTime[1] != input$showTime[2]) {
#                  
#                  ddply(
#                      results$subsetPlotData(), .(NAAM, specie),
#                      summarize, counts = sum(counts))
#                  
#                }else results$subsetPlotData()
#            
#            return(countsDataSpecieYearAggr)
#            
#          })
#      
#      
#      # Which region(s) are selected?
#      observe({
#            
##            validate(need(!is.null(input$selectAll), "Laden..."))
##            
##            if (input$selectAll)
##              updateSelectInput(session = session, inputId = "showRegion",
##                  selected = results$shapeData()$NAAM) else
##              updateSelectInput(session = session, inputId = "showRegion",
##                  selected = NULL)
#            
#            
#            event <- input$showMap_shape_click
#            
#            if (!is.null(event)) {
#              
#              currentSelected <- isolate(input$showRegion)
#              
#              if (event$id %in% currentSelected) {
#                
#                updateSelectInput(session, "showRegion", 
#                    selected = currentSelected[ - which(currentSelected == event$id)])
#                
#              } else {
#                
#                updateSelectInput(session, "showRegion", 
#                    selected = c(currentSelected, event$id))
#                
#              }
#              
#            }
#            
#          })
#      
#      
#      # Define text to be shown in the pop-ups
#      results$textPopup <- reactive({
#            
#            validate(need(results$shapeData(), "Geen data beschikbaar"),
#                need(results$subsetPlotData(), "Geen data beschikbaar"))
#            
#            extraVariables <- ""
#            
#            
#            if (!is.null(input$popupVariables)) {
#              
#              validate(need(input$popupVariables %in% names(results$subsetPlotDataAggr()), 
#                      "Deze variabelen zijn niet beschikbaar in de data"))
#              
#              for (iName in input$popupVariables) {
#                
#                extraVariables <- paste(extraVariables,
#                    "<br> <b>", iName, "</b>: ", 
#                    results$subsetPlotDataAggr()[, iName])
#                
#              }
#              
#            }
#            
#            regionNames <- results$subsetPlotDataAggr()$NAAM # results$shapeData()$NAAM
#            titleText <- paste("Geobserveerd aantal",            
#                ifelse(input$showTime[1] != input$showTime[2],
#                    paste("van", input$showTime[1], "tot", input$showTime[2]),
#                    paste("in", input$showTime[1])
#                )
#            )
#            
#            textPopup <- paste0("<h4>", regionNames, "</h4>",  
#                "<strong>", titleText, "</strong>: ", 
#                round(results$subsetPlotDataAggr()$counts, 2),
#                "<br>", extraVariables
#            )
#            
#            
#            return(textPopup)
#            
#          })
#      
#      
#      results$colorPalette <- reactive({
#            
#            validate(need(nrow(results$subsetPlotDataAggr()) > 0, "Geen data beschikbaar"))
#            
#            domain <- range(results$subsetPlotDataAggr()$counts, na.rm = TRUE)
#            
#            palette <- colorNumeric(palette = "Reds", 
#                domain = domain)
#            
#            valuesPalette <- results$subsetPlotDataAggr()[
#                match(results$shapeData()$NAAM, results$subsetPlotDataAggr()$NAAM),
#                "counts"]
#            
#            palette(valuesPalette)
#            
#          })
#      
#      
#      # Send map to the UI
#      output$showMap <- renderLeaflet({
#            
#            validate(need(results$shapeData(), "Geen data beschikbaar"),
#                need(nrow(results$subsetPlotDataAggr()) > 0, "Geen data beschikbaar"))
#            
#            if (input$spatialLevel == "provinces")
#              provinceBounds <- list(color = "white", opacity = 0) else
#              provinceBounds <- list(color = "black", opacity = 0.8)            
#            
#            
#            leaflet(results$shapeData()) %>%
#                
#                addPolygons(
#                    weight = 1, 
#                    color = "white",
#                    fillColor = ~ results$colorPalette(),
#                    fillOpacity = 0.8,
#                    layerId = results$shapeData()$NAAM,
#                    group = "region"
#                ) %>%
#                
#                addPolylines(
#                    data = provinceData, 
#                    color = provinceBounds$color, 
#                    weight = 3,
#                    opacity = provinceBounds$opacity)
#            
#          })
#      
#      
#      # Plot thick border for selected regions
#      observe({
#            
#            if (!is.null(input$showRegion)) {
#              
#              validate(need(results$shapeData(), "Geen data beschikbaar"))
#              
#              selectedPolygons <- subset(results$shapeData(), 
#                  results$shapeData()$NAAM %in% input$showRegion)
#              
#              leafletProxy("showMap", data = results$shapeData()) %>%
#                  
#                  clearGroup(group = "regionLines") %>%
#                  
#                  addPolylines(data = selectedPolygons, color = "white", weight = 5,
#                      group = "regionLines")
#              
#            }
#            
#          })
#      
#      
#      # Plot thick border for province if communes are shown
#      observe({
#            
#            validate(need(input$spatialLevel, "Gelieve 'Regio-schaal' te selecteren"))
#            
#            if (input$spatialLevel == "communes") {  
#              
#              leafletProxy("showMap", data = results$shapeData()) %>%
#                  
#                  addPolylines(data = provinceData, color = "black", weight = 3,
#                      opacity = 0.8, group = "provinceLines")
#              
#            } else {
#              
#              leafletProxy("showMap", data = results$shapeData()) %>%
#                  
#                  clearGroup(group = "provinceLines")
#              
#            }
#            
#          })
#      
#      
#      # Add world map
#      observe({
#            
#            validate(need(results$shapeData(), "Geen data beschikbaar"))
#            
#            proxy <- leafletProxy("showMap", data = results$shapeData())
#            
#            if (!is.null(input$providerTiles) & !is.null(proxy)){
#              
#              if (input$providerTiles %% 2 == 1){
#                
#                proxy %>% addProviderTiles("Hydda.Full")
#                
#              } else {
#                
#                proxy %>% clearTiles()
#                
#              }
#              
#            }
#            
#          })
#      
#      
#      # Add legend
#      observe({
#            
#            validate(need(results$shapeData(), "Geen data beschikbaar"),
#                need(nrow(results$subsetPlotDataAggr()) > 0, "Geen data beschikbaar"))
#            
#            if (!is.null(input$legendPlacement)) {
#              
#              domain <- range(results$subsetPlotDataAggr()$counts, na.rm = TRUE)
#              
#              proxy <- leafletProxy("showMap", data = results$shapeData())
#              
#              proxy %>% removeControl(layerId = "legend")
#              
#              if (input$legendPlacement != "none"){
#                
#                validate(need(input$showTime, "Gelieve tijdstip(pen) te selecteren"))
#                
#                colorPalette <- colorNumeric(palette = "Reds", 
#                    domain = domain)
#                valuesPalette <- results$subsetPlotDataAggr()[
#                    match(results$shapeData()$NAAM, results$subsetPlotDataAggr()$NAAM),
#                    "counts"]
#                
#                proxy %>% addLegend(
#                    position = input$legendPlacement,
#                    pal = colorPalette, 
#                    values = valuesPalette,
#                    opacity = 0.8,
#                    title = "Legend",
#                    layerId = "legend"
#                )                      
#                
#                if (!is.null(input$legendPlacement)) {
#                  
#                  proxy <- leafletProxy("showMap", data = results$shapeData())
#                  
#                  proxy %>% removeControl(layerId = "legend")
#                  
#                  if (input$legendPlacement != "none"){
#                    
#                    validate(need(input$showTime, "Gelieve een tijdstip te selecteren"))
#                    
#                    proxy %>% addLegend(
#                        position = input$legendPlacement,
#                        pal = colorPalette, 
#                        values = valuesPalette,
#                        opacity = 0.8,
#                        title = "Legend",
#                        layerId = "legend"
#                    )                      
#                    
#                  }
#                  
#                }
#                
#              }
#              
#            }
#            
#          })
#      
#      
#      # Add popups
#      observe({
#            
#            validate(need(results$shapeData(), "Geen data beschikbaar"),
#                need(results$textPopup(), "Geen data beschikbaar"))
#            
#            currentMap <- leafletProxy("showMap", data = results$shapeData()) 
#            currentMap %>% clearPopups()
#            
#            event <- input$showMap_shape_click
#            
#            if(!is.null(event)){
#              
#              textSelected <- results$textPopup()[
#                  results$subsetPlotDataAggr()$NAAM == event$id] #results$shapeData()$NAAM
#              
#              isolate({
#                    
#                    currentMap %>% 
#                        addPopups(event$lng, event$lat, popup = textSelected)
#                    
#                  }) 
#              
#            }
#            
#          })
#      
#      
#      # Interactive time plot
#      results$interactiveTime <- reactive({
#            
#            validate(need(results$subsetPlotData(), "Geen data beschikbaar"),
#                need(input$showRegion, "Gelieve regio('s) te selecteren"))
#            
#            currentData <- results$subsetPlotData()
#            
#            plotTime <- Highcharts$new()
#            
#            timePointsSelected <- sort(unique(currentData$year))
#            
##			message("timePointsSelected:", toString(timePointsSelected ))
#            
#            plotTime$xAxis(categories = timePointsSelected, 
#                labels = list(rotation = -45, align = 'right'))
##            plotTime$yAxis(min = 0, max = 1)
#            plotTime$chart(width = 600)
#            
#            plotTime$title(
#                text = paste("Geobserveerd aantal voor", input$showSpecies,
#                    ifelse(input$showTime[1] != input$showTime[2],
#                        paste("van", input$showTime[1], "tot", input$showTime[2]),
#                        paste("in", input$showTime[1])
#                    )
#                )
#            )
#            
#            
#            currentData$y <- currentData$counts      
#            
#            
#            plotTime$series(
#                lapply(input$showRegion, function(iArea) {
#                      
#                      list(name = iArea, data = toJSONArray2(currentData[
#                                  currentData$NAAM == iArea, ], json = F))
#                      
#                    })
#            )
#            
#            if (!is.null(input$popupVariables)) {
#              
#              textTooltip <- paste("<b> {point.spatialUnit} </b> <br> <br>",
#                  paste0(input$popupVariables, ": {point.", input$popupVariables, "}",
#                      collapse = "<br>")) 
#              plotTime$tooltip(useHTML = TRUE, pointFormat = textTooltip)
#              
#            }
#            
#            plotTime$save(file.path(tempdir(), "plotTijd.html"),
#                standalone = TRUE)
#            
#            plotTime
#            
##            return(plotTime)
#            
#          })
#      
#      output$interactiveTime <- renderChart2({results$interactiveTime()})
#      
#      output$downloadPlotTime <- downloadHandler("plotTijd.html",
#          content = function(file) {
#            file.copy(file.path(tempdir(), "plotTijd.html"), file)
#          }
#      )
#      
#      
#      output$titleInteractivePlot <- renderUI({
#            
#            h4(paste("Geobserveerd aantal voor", input$showSpecies,
#                    ifelse(input$showTime[1] != input$showTime[2],
#                        paste("van", input$showTime[1], "tot", input$showTime[2]),
#                        paste("in", input$showTime[1])
#                    )
#                ))
#            
#          })
#      
#      
#      # Create final map
#      results$finalMap <- reactive({
#            
#            validate(need(results$shapeData(), "Geen data beschikbaar"),
#                need(results$subsetPlotDataAggr(), "Geen data beschikbaar"),
#                need(input$spatialLevel, "Loading..."))
#            
#            domain <- range(results$subsetPlotDataAggr()$counts, na.rm = TRUE)
#            
#            palette <- colorNumeric(palette = "Reds", 
#                domain = domain)      
#            
#            newMap <- leaflet(results$shapeData())
#            
#            if (input$providerTiles %% 2 == 1) {
#              
#              newMap <- addProviderTiles(newMap, "Hydda.Full")
#              
#            } 
#            
#            if (input$legendPlacement != "none") { 
#              
#              newMap <- addLegend(newMap,
#                  position = input$legendPlacement,
#                  pal = colorNumeric(palette = "Reds", 
#                      domain = domain), 
#                  values = results$subsetPlotDataAggr()$counts,
#                  opacity = 0.8,
#                  title = "Legend",
#                  layerId = "legend"
#              )
#              
#            }
#            
#            newMap <- addPolygons(newMap,
#                weight = 1,
#                color = "white",
#                fillColor = ~ results$colorPalette(),
#                fillOpacity = 0.8,
#                layerId = results$shapeData()$NAAM,
#                group = "region"
#            )
#            
#            if (input$spatialLevel == "communes") {  
#              
#              newMap <- addPolylines(newMap,
#                  data = provinceData, color = "black", weight = 3,
#                  opacity = 0.8, group = "provinceLines")
#              
#            }
#            
#            newMap
#            
#          })
#      
#      output$downloadPlotSpace <- downloadHandler("plotRuimte.png",
#          content = function(file) {
#            
#            htmlwidgets::saveWidget(widget = results$finalMap(), 
#                file = file.path(tempdir(), "plotRuimte.html"), selfcontained = FALSE)
#            webshot::webshot(file.path(tempdir(), "plotRuimte.html"), file = file, 
#                cliprect = "viewport")
#            
#          }
#      )
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