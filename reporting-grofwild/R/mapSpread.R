# Functions to plot the interactive map for verspreiding everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################



#' Necessary info for the color palette of \code{\link{mapCube}}
#' @param groupNames character vector, labels to be shown in the color legend
#' @param groupVariable character, variable for which the \code{groupNames} are defined
#' @return list with colors, character vector and levels, character vector. 
#' Each item has same length as \code{units}
#' 
#' @author mvarewyck
#' @importFrom grDevices palette
#' @export
paletteMap <- function(variable, groupNames) {
  
  myColors <- if (grepl("model", variable))
      c("deepskyblue1", "deepskyblue3", "deepskyblue4") else if (grepl("risk", variable))
      c('red', 'orange', 'green', 'white') else if (grepl("Strt", variable))
      "black"
  
  list(
    colors = myColors,
    levels = groupNames
  )
  
}



#' Create leaflet map for the spread of a species
#' 
#' @param spatialDir path to directory with spatial files
#' @param spatialLevel character, for which spatial level to create the map;
#' should be one of \code{c("pixels", "municipalities")}
#' @param unit character, characteristic that defines the polygons and color coding
#' should be one of \code{c("model_EP", "model_OH", "risk_EP", "risk_OH")}
#' @param startYear integer, starting year; only relevant for pixels & model unit 
#' @inheritParams mapFlanders
#' @return leaflet map
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapSpread <- function(
  spatialDir = system.file("extdata", package = "reporting-grofwild"), 
  spatialLevel = c("pixels", "municipalities"), 
  unit = c("model_EP", "model_OH", "risk_EP", "risk_OH"), 
  startYear = 2019,
  legend = "none", addGlobe = FALSE) {
  
  
  spatialLevel <- match.arg(spatialLevel)
  unit <- match.arg(unit)
  
  spatialFile <- switch(spatialLevel,
    # pixels
    pixels = "Pixels_ModelOutput_toekomst_verspr_2022.shp",
    # gemeente
    municipalities = "Municipalities_ModelOutput_toekomst_verspr_2022.shp"
  )
  
  unitChoices <- if (spatialLevel == "pixels")
      c("Mdl_EP_", "Mdl_OH_", "Rsc_ExP", "Rsc_OpH") else
      c("M_EP_A_", "M_OH_A_", "M_EP__G_", "M_OH__G_")
  unitVariable <- unitChoices[match(unit, c("model_EP", "model_OH", "risk_EP", "risk_OH"))]
  
  baseMap <- rgdal::readOGR(file.path(spatialDir, spatialFile)) %>%
    sp::spTransform(CRS("+proj=longlat +datum=WGS84"))
  
  
  # Modify data
  ## Risico
  riskLevels <- c("Hoog risico", "Gemiddeld risico", "Laag risico", "Verwaarloosbaar risico") 
  if (spatialLevel == "pixels") {
    baseMap$Rsc_ExP <- factor(baseMap$Rsc_ExP, levels = riskLevels)
    baseMap$Rsc_OpH <- factor(baseMap$Rsc_OpH, levels = riskLevels)
  } else {
    baseMap$M_EP__G_ <- factor (baseMap$M_EP__G_, levels = riskLevels)
    baseMap$M_OH__G_ <- factor (baseMap$M_OH__G_, levels = riskLevels)
  }
  
  
  modelShape <- subset(baseMap, !is.na(baseMap@data[, unitVariable]))
  modelShape[[unitVariable]] <- as.factor(modelShape[[unitVariable]])
  
  modelColors <- paletteMap(variable = unit, groupNames = levels(modelShape[[unitVariable]]))
  pal_model <- colorFactor(palette = modelColors$colors, levels = modelColors$levels, na.color = NA)
 
  
  
  finalMap <- leaflet(baseMap)
  
  finalMap <- finalMap %>%
    
    addPolygons(
      data = modelShape,
      stroke = spatialLevel != "pixels",
      smoothFactor = 1,
      fillOpacity = if (spatialLevel == "pixels") 1 else 0.5,
      fillColor =  ~pal_model(get(unitVariable)),
      weight = if (spatialLevel == "pixels") 0 else 0.75,
      color = "black",
      group = "modelPolygons")
  
  
  startVariable <- switch(unitVariable,
    Mdl_EP_ = "Strt_EP",
    Mdl_OH_ = "Strt_OH",
    NULL
  )
  
  if (!is.null(startVariable) && startVariable %in% colnames(baseMap@data)) {
    
    startShape <- subset(baseMap, baseMap@data[[startVariable]] == startYear)
    
    startColors <- paletteMap(variable = startVariable, groupNames = unique(startShape[[startVariable]]))
    pal_start <- colorFactor(palette = startColors$colors, levels = startColors$levels, na.color = NA)
    
    finalMap <- finalMap %>%
      addPolygons(
        data = startShape,
        stroke = FALSE,
        smoothFactor = 1,
        fillOpacity = 1,
        fillColor =  ~pal_start(get(startVariable)),
        group = "startPolygons")
    
  }
  
  
  # Add legend
  if (legend != "none") { 
    
    finalMap <- addLegend(
      map = finalMap, 
      position = legend,
      pal = pal_model, 
      values = modelColors$levels,
      title = if (grepl("model", unit))
          "Waarschijnlijkheid verspreiding" else 
          "Risico klasse",
      opacity = 1,
      na.label = "",
      layerId = "modelLegend")
    
    if (!is.null(startVariable) && startVariable %in% colnames(baseMap@data))
      finalMap <- addLegend(
        map = finalMap,
        position = legend,
        pal = pal_start, 
        values = startColors$levels,
        title = "Startlocatie",
        opacity = 1,
        layerId = "startLegend")
    
  }
  
  # Add background map
  if (addGlobe) {
    
    finalMap <- finalMap %>% addProviderTiles("OpenStreetMap.HOT")
    
  }
  
  
  finalMap
  
}





#' Shiny module for creating the plot \code{\link{mapFlanders}} - server side
#' @param id character, unique identifier for the module
#' @param defaultYear numeric, default year
#' @param species character, species for which to show the graphs
#' @param currentWbe numeric, KBO number; default value is NULL
#' @param hideGlobeDefault boolean, whether the globe is shown by default 
#' when the map is first created; default value is TRUE
#' @param type character, defines the layout depending on which page it is shown;
#' should be one of \code{c("grofwild", "wildschade", "wbe", "dash")}
#' @param geoData SpatialPolygonsDataFrame with geographical data
#' @param biotoopData data.frame, with background biotoop data for selected region level;
#' default value is NULL
#' @param allSpatialData list with SpatialPolygonsDataFrame 
#' @param regionLevel reactive, value of regionLevel if chosen outside module;
#' only relevant for \code{type} "dash"
#' @param locaties reactive, value of region if chosen outside module;
#' only relevant for \code{type} "dash"
#' 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom ggplot2 fortify
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @export
mapSpreadServer <- function(id) {
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
     
      
      
      ## Map for spread ##
      ## -------------- ##
   
      # Send map to the UI
      output$spreadPlot <- renderLeaflet({
          
          mapSpread(
            spatialDir = "~/git/reporting-rshiny-grofwildjacht/dashboard/input/spatial",
            spatialLevel = input$spatialLevel,
            unit = input$unit
          )          
          
        })
      
      
      # TODO Center view for WBE
      observe({
          
          req(type %in% c("wbe", "dash"))
          
          selectedPolygons <- subset(spatialData(), 
            spatialData()$NAAM %in% results$region_value)
          
          coordData <- ggplot2::fortify(selectedPolygons)
          centerView <- c(range(coordData$long), range(coordData$lat))
          
          leafletProxy("spreadPlot", data = spatialData()) %>%
            
            fitBounds(lng1 = centerView[1], lng2 = centerView[2],
              lat1 = centerView[3], lat2 = centerView[4])
          
        })
      
      
      # TODO Plot thick border for selected regions
      observe({
          
          if (!is.null(results$region_value)) {
            
            validate(need(spatialData(), "Geen data beschikbaar"))
            
            selectedPolygons <- subset(spatialData(), 
              spatialData()$NAAM %in% results$region_value)
            
            leafletProxy("spreadPlot", data = spatialData()) %>%
              
              clearGroup(group = "regionLines") %>%
              
              addPolylines(data = selectedPolygons, color = "gray", weight = 5,
                group = "regionLines")
            
          } else {
            
            leafletProxy("spreadPlot", data = spatialData()) %>%
              
              clearGroup(group = "regionLines")
            
          }
          
        })
      
      
      # Add world map
      observe({
      
          proxy <- leafletProxy("spreadPlot", data = spatialData())
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == as.numeric(hideGlobeDefault)){
              
              updateActionLink(session, inputId = "globe", 
                label = "Verberg landkaart")
              
              proxy %>% addProviderTiles("OpenStreetMap.HOT")
              
            } else {
              
              updateActionLink(session, inputId = "globe", 
                label = "Voeg landkaart toe")
              
              proxy %>% clearTiles()
              
            }
            
          }
          
        })
      
      
      # TODO Add legend
      observe({
          
          req(input$legend)
          
          proxy <- leafletProxy("spreadPlot")
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            palette <- colorFactor(palette = results$colorScheme(), 
              levels = levels(results$summarySpaceData()$data$group))
            
            if (type == "wbe")
              valuesPalette <- results$summarySpaceData()$data[
                spatialData()$NAAM %in% results$summarySpaceData()$data$locatie,
                "group"] else 
              valuesPalette <- results$summarySpaceData()$data[
                match(spatialData()$NAAM, results$summarySpaceData()$data$locatie),
                "group"]
            
            proxy %>% addLegend(
              position = input$legend,
              pal = palette, 
              values = valuesPalette,
              opacity = 0.8,
              title = "Legende",
              layerId = "legend"
            )                      
            
          }
          
        })
      
      
      
      
      # TODO Create final map (for download)
      results$finalMap <- reactive({
          
          validate(need(results$summarySpaceData()$data, "Geen data beschikbaar"))
          
          newMap <- mapFlanders(
            regionLevel = results$regionLevel(), 
            species = species(),
            allSpatialData = allSpatialData,
            summaryData = results$summarySpaceData()$data,
            colorScheme = results$colorScheme(),
            legend = input$legend,
            addGlobe = input$globe %% 2 == as.numeric(hideGlobeDefault)
          )
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spreadPlot_center$lng,
            lat = input$spreadPlot_center$lat,
            zoom = input$spreadPlot_zoom
          )
          
          tmpFile <- tempfile(fileext = ".html")
          
          # write map to temp .html file
          htmlwidgets::saveWidget(newMap, file = tmpFile, selfcontained = FALSE)
          
          # output is path to temp .html file containing map
          tmpFile
          
        }) 
      
      
      # TODO Download the map
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            year = input$year, 
            content = "kaart", fileExt = "png"),
        content = function(file) {
          
          # convert temp .html file into .png for download
          webshot::webshot(url = results$finalMap(), file = file,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
        }
      )
      
      # TODO Download data
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = paste(species(), collapse = "-"),
            year = input$year, 
            content = "kaartData", fileExt = "csv"),
        content = function(file) {
          
          myData <- results$summarySpaceData()$data
          # change variable names
          names(myData)[names(myData) == "freq"] <- results$unitText()
          names(myData)[names(myData) == "group"] <- "groep"
          
          ## write data to exported file
          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
      })
   
}



#' Shiny module for creating the plot \code{\link{mapSpread}} - UI side
#' @inheritParams mapSpreadServer 
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
mapSpreadUI <- function(id) {
  
  ns <- NS(id)
  type <- match.arg(type)
  
  # Map spread
  
  tags$div(class = "container",
    
    ## countMap: all species
    wellPanel(
      fixedRow(
        column(4, selectInput(inputId = ns("spatialLevel"), label = "Regio-schaal",
            choices = c(
              "Gemeente" = "municipalities",
              "2x2 UTM" = "pixels"
            ))      
        ),
        column(4, selectInput(inputId = ns("legend"), label = "Legende",
            choices = c(
              "Bovenaan rechts" = "topright",
              "Onderaan rechts" = "bottomright",
              "Bovenaan links" = "topleft",
              "Onderaan links" = "bottomleft",
              "<geen>" = "none")) 
        ),
        
        column(4, selectInput(inputId = ns("unit"), label = "Eenheid",
            choices = c(
              "Model output exacte pixels" = "model_EP", 
              "Model output optimaal habitat" = "model_OH", 
              "Risico klasse exacte pixels" = "risk_EP", 
              "Risico klasse optimaal habitat" = "risk_OH")
          )
        )
      ),
      
      actionLink(inputId = ns("globe"), label = "Voeg landkaart toe",
        icon = icon("globe"))
  
    ),
    
    fixedRow(
      withSpinner(leafletOutput(ns("spreadPlot"))),
      tags$div(align = "center", uiOutput(ns("stats"))),
      tags$br(),
      downloadButton(ns("download"), label = "Download figuur", class = "downloadButton"),
      downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton")
    ),
    
    ,tags$hr()
  
  )
  
}
