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
#' @inheritParams readShapeData
#' @inheritParams filterSchade
#' @param timeRange numeric vector, year span of interest
#' @param fullNames named character vector, values for the \code{variable} to be 
#' displayed instead of original data values
#' @return a filtered spatialPointsDataFrame
#' 
#' @author Eva Adriaensen
#' @export
createSchadeSummaryData <- function(schadeData, timeRange,
    dataDir = system.file("extdata", package = "reportingGrofwild"),
    sourceIndicator = NULL, fullNames = NULL) {
	
  
  plotData <- filterSchade(plotData = schadeData,
    sourceIndicator = sourceIndicator, returnStop = "message")
    
  # filter columns
  colnamesToRetain <- c("season", "afschotjaar", "wildsoort", "gemeente_afschot_locatie", "schadeBasisCode",
                        "schadeCode", "provincie")
  plotData <- plotData[, colnames(plotData@data) %in% colnamesToRetain]
  
  # filter cases by timeRange
  plotData <- plotData[plotData$afschotjaar %in% timeRange[1]:timeRange[2], ]
  
  # add nis and postcode
  gemeenteData <- read.csv(file.path(dataDir, "gemeentecodes.csv"), header = TRUE, sep = ",")
  
  plotData$niscode <- gemeenteData$NIS.code[match(plotData$gemeente_afschot_locatie, 
      gemeenteData$Gemeente)]
  plotData$postcode <- gemeenteData$Postcode[match(plotData$gemeente_afschot_locatie, 
      gemeenteData$Gemeente)]
  
  
  # decrypte schade names
  if (!is.null(fullNames)) {
    
    plotData$schadeBasisCode <- names(fullNames)[match(plotData$schadeBasisCode, fullNames)]
    plotData$schadeCode <- names(fullNames)[match(plotData$schadeCode, fullNames)]
    
  }
  
  plotData
}

#' Format summary schadedata data for download with nice column names and correct column order
#' @param summarySchadeData spatialPointsDataFrame as obtained from \code{\link{createSchadeSummaryData}}
#' @return data.frame, dataframe with nicely formatted columnnames and specific column order  
#' 
#' @author Eva Adriaensen
#' @export
formatSchadeSummaryData <- function(summarySchadeData) {
	
  formatData <- summarySchadeData@data
  
  # change variable names
  names(formatData)[names(formatData) == "afschotjaar"] <- "jaar"
  names(formatData)[names(formatData) == "gemeente_afschot_locatie"] <- "locatie"
  names(formatData)[names(formatData) == "season"] <- "seizoen"
  names(formatData)[names(formatData) == "schadeBasisCode"] <- "basisTypeSchade"
  names(formatData)[names(formatData) == "schadeCode"] <- "typeSchade"
  
  
  # re-arrange columns
  firstColumns <- c("jaar", "locatie", "niscode", "postcode")
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
#' @importFrom ggplot2 fortify
#' @export
mapSchade <- function(
        schadeData, 
        regionLevel, 
        variable = c("season", "schadeCode", "afschotjaar"),
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
      centerView <- as.numeric(apply(coordinates(schadeData), 2, range))
    } else {
      coordData <- suppressMessages(ggplot2::fortify(allSpatialData[[regionLevel]]))
      centerView <- c(range(coordData$long), range(coordData$lat))
    }
    
    myMap <- leaflet(schadeData) %>%
            
            addCircleMarkers(
                    fillColor = ~palette(variable),
                    stroke = TRUE, color = "black", weight = 1, 
                    fillOpacity = 0.5,
                    popup = paste0("<h4>Info</h4>",  
                            "<ul>", 
                            "<li><strong> Jaar </strong>: ", schadeData$afschotjaar,
                            "<li><strong> Wildsoort </strong>: ", schadeData$wildsoort, 
                            "<li><strong> Gemeente </strong>: ", schadeData$gemeente_afschot_locatie,
                            "<li><strong> Schade type </strong>: ", schadeData$schadeBasisCode,
                            "<li><strong> Seizoen </strong>: ", schadeData$season,
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
#' @param allSpatialData reactive object spatialPolygonsDataFrame with
#' spatial data for selected region (and year for WBE)
#' @param timeRange integer vector, relevant period that can be selected for the map
#' @param defaultYear integer, current (default) end year of the selected period
#' @param species character vector, selected species for the plot
#' @param borderRegion character, for which \code{regionLevel} to show black border;
#' see also \code{\link{mapSchade}}; default is NULL
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @importFrom mapview mapshot
#' @importFrom leaflet renderLeaflet setView leafletProxy clearTiles
#' @export
mapSchadeServer <- function(id, schadeData, allSpatialData, timeRange, 
  defaultYear, species, borderRegion = NULL) {
  
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

          unique(results$schadeData()@data$WBE_Naam_Toek)
          
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
            value = c(timeRange()[1], defaultYear),
            min = timeRange()[1],
            max = timeRange()[2],
            step = 1,
            sep = "")
          
        })
      
      
      output$titlePerceel <- renderUI({
          
          nSpecies <- length(species())      
          
          h3(paste("Schademeldingen", 
              "voor", if (nSpecies > 1) 
                  paste(paste(tolower(species())[1:nSpecies-1], collapse = ", "), "en", tolower(species()[nSpecies])) else
                  tolower(species()),
              "per", switch(input$variable, 
                season = "seizoen",
                schadeCode = "schadetype",
                afschotjaar = "jaar"),
              ifelse(input$time_schade[1] != input$time_schade[2],
                paste0("(", input$time_schade[1], " tot ", input$time_schade[2], ")"),
                paste0("(", input$time_schade[1], ")")
              )
            ))
          
        })
      
      # Filter schade data
      results$schadeData <- reactive({
          
          # Select species & code & exclude data before 2018
          toRetain <- schadeData()@data$wildsoort %in% req(species()) &
            schadeData()@data$schadeBasisCode %in% req(input$code) &
            schadeData()@data$afschotjaar >= 2018
          
          
          # Filter gewas
          if ("GEWAS" %in% input$code) {
            otherCodes <- input$code[input$code != "GEWAS"]
            toRetain <- toRetain &
              (schadeData()@data$schadeBasisCode %in% otherCodes |
                schadeData()@data$schadeCode %in% input$gewas)
          }
          
          # Filter voertuig
          if ("VRTG" %in% input$code) {
            otherCodes <- input$code[input$code != "VRTG"]
            toRetain <- toRetain &
              (schadeData()@data$schadeBasisCode %in% otherCodes |
                schadeData()@data$schadeCode %in% input$voertuig)
          }
          
          schadeData()[toRetain, ]
          
        })
      
      output$nFilter <- renderUI({
          
          helpText(paste("Filter", nrow(results$schadeData()), "observaties"))
          
        })

      
      # Create data for map, summary of schade data, given year
      results$summaryPerceelData <- reactive({
          
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
          
        })
      
      # Map for UI
      output$perceelPlot <- renderLeaflet({
          
          validate(need(allSpatialData(), "Geen data beschikbaar"),
            # Also show map if 0 observations
            need(ncol(results$summaryPerceelData()@data) > 0, "Geen data beschikbaar"),
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
          
          print(list(
              lng = input$perceelPlot_center$lng,
              lat = input$perceelPlot_center$lat,
              zoom = input$perceelPlot_zoom
            ))
          
          # save the zoom level and centering
          newPerceelMap %>%  setView(
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
          
          file <- tempfile(fileext = ".png")
          map(file)
          
          mapview::mapshot(x = results$perceelMap(), file = file,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
          removeNotification(id = idNote)
          
          session$sendCustomMessage(type = "imageReady", 
            message = list(id = ns("downloadPerceelMap")))
        })
      
      # Download the perceeplot map
      output$downloadPerceelMap <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            year = unique(input$time_schade), 
            content = switch(input$variable, 
              season = "kaartSchadeSeizoen", 
              schadeCode = "kaartSchadeTypeSchade",
              afschotjaar = "kaartSchadeJaar"), 
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
          need(nrow(results$schadeData()@data) > 0, "Geen data beschikbaar"))
        
        createTrendData(
          data = results$schadeData()@data,
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
#' @inheritParams mapFlandersUI
#' @template moduleUI
#' 
#' @author mvarewyck
#' @importFrom leaflet leafletOutput
#' @export
mapSchadeUI <- function(id, filterCode = FALSE, filterSubcode = FALSE, uiText,
  plotDetails = NULL) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  metaSchade <- loadMetaSchade()
  
  tagList(
    
    actionLink(inputId = ns("linkMapSchade"), label =
        h3(HTML(uiText$title))),
    conditionalPanel("input.linkMapSchade % 2 == 1", ns = ns,
      
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
              choices = c(
                "Seizoen" = "season",
                "Jaar" = "afschotjaar",
                "Type schade" = "schadeCode"))
          )
        ),
        fixedRow(
          column(6,
            selectInput(inputId = ns("bron"),
              label = "Data bron",
              choices = names(metaSchade$sources),
              selected = names(metaSchade$sources),
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
      
      tags$p(HTML(uiText[, id])),
      
      fixedRow(
        column(if (length(plotDetails) == 1) 6 else 12,
          
          uiOutput(ns("titlePerceel")),        
          withSpinner(leafletOutput(ns("perceelPlot"))),
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
            h3("Evolutie schademeldingen WBE"),
            plotModuleUI(id = ns("timePlotSchade"), height = "400px"),
            optionsModuleUI(id = ns("timePlotSchade"), exportData = TRUE,
              doWellPanel = FALSE)
          )      
      ),
      
      tags$hr()
    
    )
  
  )

  
}

