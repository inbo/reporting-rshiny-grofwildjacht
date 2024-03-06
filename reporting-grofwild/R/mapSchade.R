# Map(s) for wildschade
# 
# Author: mvarewyck
###############################################################################




#' Summarized data for perceelplot
#' 
#' Create object of type schadedata, filtered for cases within \code{timeRange}
#' whilst retaining only a select number of columns relevant for the perceelPlot 
#' created by \code{\link{mapSchade}} and relevant for data download.
#' 
#' @inheritParams mapSchade
#' @inheritParams createShapeData
#' @inheritParams filterDataSource
#' @param timeRange numeric vector, year span of interest
#' @param fullNames named character vector, values for the \code{variable} to be 
#' displayed instead of original data values
#' @return a filtered spatialPointsDataFrame
#' 
#' @author Eva Adriaensen
#' @export
createSchadeSummaryData <- function(schadeData, timeRange,
    sourceIndicator = NULL, fullNames = NULL) {
	
  
  plotData <- filterDataSource(plotData = schadeData,
    sourceIndicator = sourceIndicator, returnStop = "message")
    
  # filter columns
  colnamesToRetain <- c("season", "afschotjaar", "wildsoort", "gemeente_afschot_locatie", "schadeBasisCode",
                        "schadeCode", "provincie", "NISCODE", "postcode", "x", "y")
  plotData <- plotData[, colnames(plotData) %in% colnamesToRetain]
  
  # filter cases by timeRange
  plotData <- plotData[plotData$afschotjaar %in% timeRange[1]:timeRange[2], ]
  
  # decrypte schade names
  if (!is.null(fullNames)) {
    
    plotData$schadeBasisCode <- names(fullNames)[match(plotData$schadeBasisCode, fullNames)]
    plotData$schadeCode <- names(fullNames)[match(plotData$schadeCode, fullNames)]
    
  }
  
  # Create spatial object
  plotData <- sf::st_as_sf(plotData, coords = c("x", "y"), crs = 31370)
  plotData <- sf::st_transform(plotData, crs = "+proj=longlat +datum=WGS84")        
    
  
  plotData
}


#' Create spatial object with afschot locations
#' which can directly serve as input for \code{\link{mapSchade}}
#' 
#' @param data data.frame with ecoData and geoData combined;
#' need at least \code{c("season", "afschotjaar", "verbatimLatitude", "verbatimLongitude", "PuntLocatieTypeID")}
#' @param accuracy numeric vector, accuracy levels to filter
#' @return SpatialPointsDataFrame from \code{data}
#' @inheritParams createSchadeSummaryData
#' 
#' @author mvarewyck
#' @importFrom sf st_as_sf
#' @export
createAfschotLocationsData <- function(data, accuracy = NULL, timeRange) {
  
  mapData <- data[, c("wildsoort", "season", "afschotjaar", "jachtmethode_comp",
      "verbatimLatitude", "verbatimLongitude", "PuntLocatieTypeID")]
  
  # Filter on accuracy
  if (!is.null(accuracy))
    mapData <- mapData[mapData$PuntLocatieTypeID %in% accuracy, ]
  # Filter on time period
  mapData <- mapData[mapData$afschotjaar %in% timeRange[1]:timeRange[2], ]
  
  nTotal <- nrow(mapData)
  
  # Exclude NAs
  mapData <- mapData[!is.na(mapData$verbatimLatitude) & !is.na(mapData$verbatimLongitude), ]
  nAvailable <- nrow(mapData)
  
  if (nAvailable == 0)
    return(NULL)
  
  # Create spatial object
  # create shape data
  mapData <- sf::st_as_sf(mapData, coords = c("verbatimLongitude", "verbatimLatitude"), crs = "+proj=longlat +datum=WGS84")
  
  # Annotation on percentage collected
  attr(mapData, "annotation") <- percentCollected(nAvailable = nAvailable,
    nTotal = nTotal, text = "gekende afschotlocatie")
  
  
  return(mapData)
  
}

#' Format summary schadedata data for download with nice column names and correct column order
#' @param summarySchadeData spatialPointsDataFrame as obtained from \code{\link{createSchadeSummaryData}}
#' @return data.frame, dataframe with nicely formatted columnnames and specific column order  
#' 
#' @author Eva Adriaensen
#' @importFrom sf st_drop_geometry
#' @export
formatSchadeSummaryData <- function(summarySchadeData) {
	
  formatData <- sf::st_drop_geometry(summarySchadeData)
  
  # Change values
  if ("PuntLocatieTypeID" %in% colnames(formatData))
    formatData$PuntLocatieTypeID <- ifelse(is.na(formatData$PuntLocatieTypeID), 
      "Onbekend", c("Exact", NA, "Binnen 250m", NA, "Binnen een gebied")[formatData$PuntLocatieTypeID])
  
  # change variable names
  newNames <- c(
    "afschotjaar" = "jaar",
    "gemeente_afschot_locatie" = "locatie",
    "season" = "seizoen",
    "schadeBasisCode" = "basisTypeSchade",
    "schadeCode" = "typeSchade",
    "PuntLocatieTypeID" = "nauwkeurigheid"
  )
  
  for (i in seq_along(newNames))
    names(formatData)[names(formatData) == names(newNames)[i]] <- newNames[i]
    
  
  # re-arrange columns
  firstColumns <- c("jaar", "locatie", "NISCODE", "postcode")
  formattedData <- cbind(
                        formatData[na.omit(match(firstColumns, names(formatData)))],
                        formatData[setdiff(names(formatData), firstColumns)]
                        )
  
  return(formattedData)                        

}

#' Create map for Wildschade percelen
#' @param schadeData spatialPointsDataFrame contains the points where there was
#' wildschade and descriptives in data.frame
#' @inheritParams mapFlanders 
#' @param variable character, indicates the variable of interest to color points by. 
#' Should be one of \code{c("season", "schadeCode", "afschotjaar")}
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles fitBounds
#' @importFrom RColorBrewer brewer.pal
#' @importFrom INBOtheme inbo_palette
#' @export
mapSchade <- function(
        schadeData, 
        regionLevel, 
        variable = c("season", "schadeCode", "afschotjaar", "jachtmethode_comp"),
        allSpatialData,
        addGlobe = FALSE,
        legend = "topright"
) {
    
    variable <- match.arg(variable)
    schadeData$variable <- schadeData[[variable]]
    schadeData$variable <- as.factor(schadeData$variable)
    
  
    # Color palette
    nColors <- length(levels(schadeData$variable))
    colors <- if (nColors < 10) {
        inbo_palette(n = nColors) 
      } else {
        paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
        unlist(sapply(paletteNames, function(x)
              suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
      }

    palette <- colorFactor(colors, levels(schadeData$variable))
         
    
    if (is.null(regionLevel)) {
      centerView <- getCenterView(schadeData)
    } else {
      centerView <- getCenterView(allSpatialData[[regionLevel]])
    }
    
    
    myMap <- leaflet(schadeData) %>%
            
            addCircleMarkers(
                    fillColor = ~palette(variable),
                    stroke = TRUE, color = "black", weight = 1, 
                    fillOpacity = 0.5,
                    popup = paste0("<h4>Info</h4>",  
                            "<ul>", 
                            "<li><strong> Jaar </strong>: ", schadeData$afschotjaar,
                            if ("wildsoort" %in% colnames(schadeData)) 
                              paste0("<li><strong> Wildsoort </strong>: ", schadeData$wildsoort),
                            if ("gemeente_afschot_locatie" %in% colnames(schadeData))
                              paste0("<li><strong> Gemeente </strong>: ", schadeData$gemeente_afschot_locatie),
                            if ("schadeBasisCode" %in% colnames(schadeData))
                              paste0("<li><strong> Schade type </strong>: ", schadeData$schadeBasisCode),
                            if ("season" %in% colnames(schadeData))
                              paste0("<li><strong> Seizoen </strong>: ", schadeData$season),
                            "</ul>"
                    )
            ) %>%
            
            fitBounds(lng1 = centerView[1], lng2 = centerView[2],
              lat1 = centerView[3], lat2 = centerView[4])
    
    # Add black borders
    if (!is.null(regionLevel)) {
        
        myMap <- addPolylines(
                map = myMap,
                data = allSpatialData[[regionLevel]], 
                weight = 3, 
                color = "black",
                opacity = 1,
                group = "borderRegion"
        )
        
    }
    
    
    # Add world map
    if (addGlobe) {
        
        myMap <- myMap %>%
                    addProviderTiles("OpenStreetMap.HOT")
        
    }
    
    # Add legend
    if (legend != "none") {
        
        myMap <- addLegend(
                map = myMap,
                position = legend,
                pal = palette, 
                values = ~variable,
                opacity = 0.8,
                na.label = "onbekend",
                title = "Legende",
                layerId = "legend"
        )
        
        
    }
    
    
    myMap
    
    
}


#' Shiny module for creating map on schade data - server side
#' @param id character, unique identifier for module
#' @param schadeData reacive object as returned by \code{loadRawData(type = "wildschade")}
#' @param allSpatialData reactive with sf objects with spatial data 
#' for selected region (and year for WBE)
#' @param timeRange integer vector, relevant period that can be selected for the map
#' @param defaultYear integer, current (default) end year of the selected period
#' @param species character vector, selected species for the plot
#' @param borderRegion character, for which \code{regionLevel} to show black border;
#' see also \code{\link{mapSchade}}; default is NULL
#' @param type character, type of plot this module is used for. Historically
#' only \code{"schade"}. Later extended to also cover \code{"afschot"}, i.e. 
#' "gerapporteerde afschot locaties", see also \code{mapAfschotUI}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom webshot webshot
#' @importFrom leaflet renderLeaflet setView leafletProxy clearTiles
#' @export
mapSchadeServer <- function(id, schadeData, allSpatialData, timeRange, 
  defaultYear, species, borderRegion = NULL, type = c("schade", "afschot")) {
  
  type <- match.arg(type)
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      results <- reactiveValues()
      
      
      # Metadata schade
      metaSchade <- loadMetaSchade()
      
      schadeWildsoorten <- metaSchade$wildsoorten
      schadeTypes <- metaSchade$types
      schadeCodes <- metaSchade$codes
      names(schadeCodes) <- NULL
      schadeCodes <- unlist(schadeCodes)
      sourcesSchade <- metaSchade$sources
      fullNames <- c(schadeTypes, schadeCodes, schadeWildsoorten)
      
      ## Region level
      results$regionLevelName <- reactive({

          unique(results$schadeData()$WBE_Naam_Toek)
          
        })
      
   
      # Data-dependent input fields
      output$subcode <- renderUI({
          
          gewasChoices <- loadMetaSchade()$codes[["GEWAS"]]
          voertuigChoices <- loadMetaSchade()$codes[["VRTG"]]
          
          tagList(
            if ("GEWAS" %in% input$code)
              selectInput(inputId = ns("gewas"), label = "Filter Gewas Schade",
                choices = gewasChoices,
                selected = gewasChoices,
                multiple = TRUE,
                width = "100%"
              ),
            if ("VRTG" %in% input$code)
              selectInput(inputId = ns("voertuig"), label = "Filter Voertuig Schade",
                choices = voertuigChoices,
                selected = voertuigChoices,
                multiple = TRUE,
                width = "100%"
              )
          )
          
        })
      
      output$time_schade <- renderUI({
          
          sliderInput(inputId = ns("time_schade"), label = "Periode", 
            value = c(timeRange()[1], min(timeRange()[2], defaultYear)),
            min = timeRange()[1],
            max = timeRange()[2],
            step = 1,
            sep = "")
          
        })
      
      
      output$titlePerceel <- renderUI({
          
          nSpecies <- length(species())      
          
          h3(paste(
              if (type == "schade")
                "Schadegevallen" else
                "Gerapporteerde afschotlocaties", 
              "voor", if (nSpecies > 1) 
                  paste(paste(tolower(species())[1:nSpecies-1], collapse = ", "), "en", tolower(species()[nSpecies])) else
                  tolower(species()),
              "per", switch(input$variable, 
                season = "seizoen",
                schadeCode = "schadetype",
                afschotjaar = "jaar",
                jachtmethode_comp = "jachtmethode"),
              ifelse(input$time_schade[1] != input$time_schade[2],
                paste0("(", input$time_schade[1], " tot ", input$time_schade[2], ")"),
                paste0("(", input$time_schade[1], ")")
              )
            ))
          
        })
      
      output$footerPerceel <- renderUI({
          
          attr(results$summaryPerceelData(), "annotation")
          
        })
      
      # Filter schade data
      results$schadeData <- reactive({
          
          # Already filtered beforehand - no filters in module
          if (is.null(input$code))
            return(schadeData())
          
          # Select species & code & exclude data before 2014
          toRetain <- schadeData()$wildsoort %in% req(species()) &
            schadeData()$schadeBasisCode %in% req(input$code) &
            schadeData()$afschotjaar >= 2014
          
          
          # Filter gewas
          if ("GEWAS" %in% input$code) {
            otherCodes <- input$code[input$code != "GEWAS"]
            toRetain <- toRetain &
              (schadeData()$schadeBasisCode %in% otherCodes |
                schadeData()$schadeCode %in% input$gewas)
          }
          
          # Filter voertuig
          if ("VRTG" %in% input$code) {
            otherCodes <- input$code[input$code != "VRTG"]
            toRetain <- toRetain &
              (schadeData()$schadeBasisCode %in% otherCodes |
                schadeData()$schadeCode %in% input$voertuig)
          }
          
          schadeData()[toRetain, ]
          
        })
      
      output$nFilter <- renderUI({
          
          helpText(paste("Filter", nrow(results$schadeData()), "observaties"))
          
        })

      
      # Create data for map, summary of schade data, given year
      results$summaryPerceelData <- reactive({
          
          if (type == "schade") {
            
            validate(need(results$schadeData(), "Geen data beschikbaar"),
              need(input$time_schade, "Gelieve periode te selecteren"),
              need(input$bron, "Gelieve data bron te selecteren"))
            
            if (nrow(results$schadeData()) == 0)
              return(results$schadeData())
            
            createSchadeSummaryData(
              schadeData = results$schadeData(),
              timeRange = input$time_schade,
              sourceIndicator = input$bron,
              fullNames = fullNames)
            
          } else {
            
            validate(need(schadeData(), "Geen data beschikbaar"),
              need(input$time_schade, "Gelieve periode te selecteren"),
              need(input$accuracy, "Gelieve nauwkeurigheid te selecteren"))
            
            toReturn <- createAfschotLocationsData(data = schadeData(),
              accuracy = input$accuracy,
              timeRange = input$time_schade)            
            
            # Check after filtering
            validate(need(!is.null(toReturn), "Geen data beschikbaar"))
            toReturn
          
          }
        
        })
      
      # Map for UI
      output$perceelPlot <- renderLeaflet({
          
          validate(need(allSpatialData(), "Geen data beschikbaar"),
            # Also show map if 0 observations
            need(ncol(results$summaryPerceelData()) > 0, "Geen data beschikbaar"),
            need(input$time_schade, "Gelieve periode te selecteren"))
          
          mapSchade(
            schadeData = results$summaryPerceelData(),
            regionLevel = if (grepl("WBE", borderRegion)) 
                paste0(borderRegion, "_", input$time_schade[2]) else
                borderRegion,
            variable = input$variable,
            allSpatialData = allSpatialData(),
            addGlobe = input$globe_schade %% 2 == 0, 
            legend = input$legend_schade)
          
        })
      
      # Create final perceelplot map (for download)
      results$perceelMap <- reactive({
          
          validate(need(results$summaryPerceelData(), "Geen data beschikbaar"))
          
          newPerceelMap <- mapSchade(
            schadeData = results$summaryPerceelData(),
            regionLevel = if (grepl("WBE", borderRegion)) 
                paste0(borderRegion, "_", input$time_schade[2]) else
                borderRegion, 
            variable = input$variable,
            allSpatialData = allSpatialData(),
            legend = input$legend_schade,
            addGlobe = input$globe_schade %% 2 == 0
          )
          
          # save the zoom level and centering
          newPerceelMap %>% setView(
            lng = input$perceelPlot_center$lng,
            lat = input$perceelPlot_center$lat,
            zoom = input$perceelPlot_zoom
          )
                    
        })
      
      # Add world map
      observeEvent(input$globe_schade, {
          
          proxy <- leafletProxy("perceelPlot")
          
          if (input$globe_schade %% 2 == 0) {
            
            updateActionLink(session, 
              inputId = "globe_schade",
              label = "Verberg landkaart")
            
            proxy %>% addProviderTiles("OpenStreetMap.HOT")
            
          } else {
            
            updateActionLink(session, 
              inputId = "globe_schade",
              label = "Voeg landkaart toe")
            
            proxy %>% clearTiles()
            
          }
          
        })
      
      
      # Generating image outside of downloadHandler
      map <- reactiveVal()
      observeEvent(input$genereerMap, {
          map(NULL)
          idNote <- showNotification("Aanvraag wordt verwerkt... Even geduld.", type = "message", duration = NULL)
          
          htmlFile <- tempfile(fileext = ".html")
          pngFile <- gsub(".html", ".png", htmlFile)
          
          # write map to temp .html file
          htmlwidgets::saveWidget(results$perceelMap(), file = htmlFile, selfcontained = FALSE)
          
          # convert temp .html file into .png for download
          webshot::webshot(url = htmlFile, file = pngFile,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
          # save in reactive value
          map(pngFile)
          
          removeNotification(id = idNote)
          
          session$sendCustomMessage(type = "imageReady", 
            message = list(id = ns("downloadPerceelMap")))
        })
      
      # Download the perceeplot map
      output$downloadPerceelMap <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            year = unique(input$time_schade), 
            content = paste0(type, "Kaart", switch(input$variable, 
              season = "Seizoen", 
              schadeCode = "TypeSchade",
              afschotjaar = "SchadeJaar",
              jachtmethode_comp = "Jachtmethode")), 
            fileExt = "png"),
        content = function(file) {
          file.copy(map(), file, overwrite = TRUE)
        }
      )
      
      # Download the minimal perceeplot map data
      output$downloadPerceelmapData <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            year = unique(input$time_schade), 
            content = "kaartDataPerVariabele", 
            fileExt = "csv"),
        content = function(file) {
          
          myPerceelplotData <- formatSchadeSummaryData(results$summaryPerceelData())
          
          ## write data to exported file
          write.table(x = myPerceelplotData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
   
  
    
    ## Time plot for selected region ##
    ## ----------------------------- ##
    
    # Create data for map, time plot
    results$timeData <- reactive({
        
        validate(need(input$time_schade, "Gelieve periode te selecteren"),
          need(nrow(results$schadeData()) > 0, "Geen data beschikbaar"))
        
        createTrendData(
          data = results$schadeData(),
          allSpatialData = allSpatialData(),
          timeRange = input$time_schade,
          species = species(),
          regionLevel = "WBE_buitengrenzen"
        )
        
      })
    
    callModule(module = optionsModuleServer, id = "timePlotSchade", 
      data = results$timeData)
    callModule(module = plotModuleServer, id = "timePlotSchade",
      plotFunction = "trendYearRegion", 
      data = results$timeData,
      locaties = results$regionLevelName,
      timeRange = reactive(input$time_schade),
      isSchade = TRUE,
      combinatie = reactive(FALSE),
    )
    
    
    })
    
}



#' Shiny module for creating map on schade data - UI side
#' @param filterCode boolean whether to include the option to filter on schade code;
#' default value is FALSE
#' @param filterSubcode boolean, whether to include the option to filter on schade subcode;
#' default value is FALSE
#' @param filterSource boolean, whether to show filter option for source
#' @param filterAccuracy boolean, whether to show filter option for accuracy
#' @param variableChoices named character vector, choices for coloring variable
#' @inheritParams mapFlandersUI
#' @inherit welcomeSectionUI
#' 
#' @author mvarewyck
#' @importFrom leaflet leafletOutput
#' @export
mapSchadeUI <- function(id, filterCode = FALSE, filterSubcode = FALSE,  
  filterSource = TRUE, filterAccuracy = FALSE,
  variableChoices = c(
    "Seizoen" = "season",
    "Jaar" = "afschotjaar",
    "Type schade" = "schadeCode"),
  uiText, plotDetails = NULL) {
  
  ns <- NS(id)
  
  metaSchade <- loadMetaSchade()
  
  tagList(  
    
    tags$p(HTML(uiText[, strsplit(id, "_")[[1]][1]])),
    
      wellPanel(
        if (filterCode || filterSubcode)
          tagList(
            fixedRow(  
              # Select type schade
              column(6, selectInput(inputId = ns("code"), label = "Type Schade",
                  choices = metaSchade$types,
                  selected = metaSchade$types,
                  multiple = TRUE,
                  width = "100%"
                )),
              
              # Select gewas & voertuig
              column(6, uiOutput(ns("subcode")))
            ),
            uiOutput(ns("nFilter"))
          ),
        
        
        fixedRow(
          column(6, uiOutput(ns("time_schade"))),
          column(6, selectInput(inputId = ns("variable"), label = "Variabele",
              choices = variableChoices)
          )
        ),
        fixedRow(
          column(6,
            if (filterSource)
              selectInput(inputId = ns("bron"),
                label = "Data bron",
                choices = metaSchade$sources,
                selected = metaSchade$sources,
                multiple = TRUE),
            if (filterAccuracy)
              selectInput(inputId = ns("accuracy"),
                label = "Nauwkeurigheid",
                choices = c("Exact" = 1, "Binnen 250m" = 3, 
                  "Binnen een gebied" = 5, "Onbekend" = NA),
                selected = c(1, 3),
                multiple = TRUE)
          ),
          column(6, selectInput(inputId = ns("legend_schade"), "Legende (kaart)",
              choices = c("Bovenaan rechts" = "topright",
                "Onderaan rechts" = "bottomright",
                "Bovenaan links" = "topleft",
                "Onderaan links" = "bottomleft",
                "<geen>" = "none")))
        
        ),
        
        actionLink(inputId = ns("globe_schade"), label = "Verberg landkaart",
          icon = icon("globe"))
      ),
      
      fixedRow(
        column(if (length(plotDetails) == 1) 6 else 12,
          
          uiOutput(ns("titlePerceel")),        
          withSpinner(leafletOutput(ns("perceelPlot"))),
          uiOutput(ns("footerPerceel")),
          tags$br(),
          actionButton(ns("genereerMap"), "Download figuur", icon = icon("download"), class = "downloadButton"),
          singleton(
            tags$head(tags$script(src = "www/triggerDownload.js"))
          ),
          downloadButton(ns("downloadPerceelmapData"), "Download data", class = "downloadButton"),
          downloadLink(ns("downloadPerceelMap"), " ")
        
        ),
        
        if ("region" %in% plotDetails)
          column(6, 
            h3("Evolutie schadegevallen WBE"),
            plotModuleUI(id = ns("timePlotSchade"), height = "400px"),
            optionsModuleUI(id = ns("timePlotSchade"), exportData = TRUE,
              doWellPanel = FALSE)
          )      
      ),
      
      tags$hr()
    
  )

  
}

#' Copy of mapSchadeUI as being used for "WBE" pagina
#' @inherit welcomeSectionUI
#' @inheritParams mapSchadeUI 
#' @export
mapAfschotUI <- function(id, filterCode = FALSE, filterSubcode = FALSE,  
  filterSource = TRUE, filterAccuracy = FALSE,
  variableChoices = c(
    "Seizoen" = "season",
    "Jaar" = "afschotjaar",
    "Type schade" = "schadeCode"),
  uiText, plotDetails = NULL) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == "mapAfschotUI", ]
  
  tagList(
    actionLink(inputId = ns("linkMapAfschot"), label =
        h3(HTML(uiText$title))),
    conditionalPanel("input.linkMapAfschot % 2 == 1", ns = ns,
      
      mapSchadeUI(id = id, filterCode = filterCode, filterSubcode = filterSubcode, 
        filterSource = filterSource, filterAccuracy = filterAccuracy,
        variableChoices = variableChoices, 
        uiText = uiText, 
        plotDetails = plotDetails)
    )
  )

}

