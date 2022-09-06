#' Create data for plotting trend over years in \code{\link{trendYearFlanders}}
#' @inheritParams createSpaceData
#' @inheritParams trendYearRegion
#' @inheritParams createShapeData
#' @return data.frame, summary of number of animals per species, region and year.
#' Ready for plotting with \code{\link{trendYearFlanders}} 
#' @author mvarewyck
#' @export
createTrendData <- function(data, allSpatialData, biotoopData = NULL,
  timeRange, species, regionLevel, 
  unit = c("absolute", "relative", "relativeDekking"),
  sourceIndicator = NULL,
  dataDir = system.file("extdata", package = "reportingGrofwild")) {
  
  
  # To prevent warnings R CMD check
  afschotjaar <- NULL
  wildsoort <- NULL
  
  unit <- match.arg(unit)
  
  # Select correct spatial data
  chosenTimes <- timeRange[1]:timeRange[2]
  spatialData <- do.call(rbind, lapply(chosenTimes, function(iYear) {
        tmp <- filterSpatial(
          allSpatialData = allSpatialData,
          species = species,
          regionLevel = regionLevel,
          year = iYear)
        if (!is.null(tmp))
          tmpData <- tmp@data else
          return(NULL)
        tmpData$YEAR <- iYear
        tmpData
      }))
  
  # filter for source
  plotData <- filterSchade(plotData = data, sourceIndicator = sourceIndicator,
    returnStop = "message")
  
  
  # Generic location name
  plotData$locatie <- as.character(switch(regionLevel,
      flanders = "Vlaams Gewest",
      provinces = plotData$provincie,
      communes = plotData$gemeente_afschot_locatie,
      faunabeheerzones = plotData$FaunabeheerZone,
      fbz_gemeentes = plotData$fbz_gemeente,
      utm5 = plotData$UTM5,
      WBE_buitengrenzen = plotData$PartijNummer
    ))
  
  # Need to match partijNummer to WBE_Naam_Toek later
  if (regionLevel == "WBE_buitengrenzen") {
    matchLocaties <- plotData[, c("PartijNummer", "WBE_Naam_Toek")]
    matchLocaties <- matchLocaties[!duplicated(matchLocaties), ]
  } else matchLocaties <- NULL
  
  # Select subset for time & species
  plotData <- subset(plotData, 
    subset = afschotjaar %in% chosenTimes & wildsoort %in% species,
    select = c("afschotjaar", "locatie"))
  
  # Exclude data with missing time or space
  plotData <- plotData[!is.na(plotData$afschotjaar) & 
      !is.na(plotData$locatie) & plotData$locatie != "",]
  
  # Summarize data over years
  summaryData <- plyr::count(df = plotData, vars = names(plotData))
  
  # Add names & times with 0 observations
  fullData <- cbind(expand.grid(
      afschotjaar = chosenTimes,
      locatie = if (!is.null(matchLocaties)) 
          matchLocaties$PartijNummer else unique(spatialData$NAAM)))
  
  if (unit == "relativeDekking") {
    # add dekkingsgraad 100ha bos&natuur      
    fullData <- merge(fullData, biotoopData[, c("regio", "Area_hab_km2_bos", "year")],
      by.x = c("locatie", "afschotjaar"), by.y = c("regio", "year"))
    names(fullData)[names(fullData) == "Area_hab_km2_bos"] <- "AREA"
    
  } else {
    # add Area
    fullData <- merge(fullData, spatialData[, c("NAAM", "AREA", "YEAR")],
      by.x = c("locatie", "afschotjaar"), by.y = c("NAAM", "YEAR"))
    
  }
  
  allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
  allData$freq[is.na(allData$freq)] <- 0
  
  # unit taken into account
  if (grepl("relative", unit))
    allData$freq <- allData$freq/allData$AREA 
  
  allData$AREA <- NULL
  
  allData$afschotjaar <- as.factor(allData$afschotjaar)
  allData$wildsoort <- paste(species, collapse = ", ")
  
  
  if (regionLevel == "communes") {
    
    # NOTE: match returns FIRST match, so gemeentecodes.csv should be correctly sorted
    # in orde to obtain HOOFDpostcode. 
    gemeenteData <- read.csv(file.path(dataDir, "gemeentecodes.csv"), 
      header = TRUE, sep = ",")
    
    # Match gemeente NAAM to niscode and (hoofd)postcode
    allData$niscode <- gemeenteData$NIS.code[match(allData$locatie, gemeenteData$Gemeente)]
    allData$postcode <- gemeenteData$Postcode[match(allData$locatie, gemeenteData$Gemeente)]
    
    # order columns and rows
    allData <- allData[order(allData$afschotjaar),]
    allData <- allData[c("afschotjaar", "locatie", "niscode", "postcode", 
        setdiff(names(allData), c("afschotjaar", "locatie", "niscode", "postcode")))]
    
    
  } else if (regionLevel == "WBE_buitengrenzen") {
    
    allData$locatie <- matchLocaties$WBE_Naam_Toek[
      match(allData$locatie, matchLocaties$PartijNummer)]
    
  }
  
  
  return(allData)
  
}



#' Create interactive plot for counts per selected communes and years
#' 
#' @param data data.frame with raw data for plotting
#' @param locaties character vector, regions that were selected to plot
#' @param combinatie logical, summarised view of selected regions
#' @param timeRange numeric vector, time range selected for plot
#' @param isFlanders boolean, whether the trend plot is made for Flanders (whole region)
#' @param isSchade boolean, whether the function should generate titles for schadeData; default is FALSE
#' @inheritParams createSpaceData
#' @inheritParams countYearProvince
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the observed number 
#' per year and per selected commune is plotted in a line plot}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animals was shot}
#' \item{'locatie': }{comune name}
#' \item{'aantal' or 'aantal/100ha': }{absolute or relative counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom stats aggregate
#' @export
trendYearRegion <- function(data, locaties = NULL, combinatie = FALSE, 
  timeRange = NULL, unit = c("absolute", "relative", "relativeDekking"), 
  isFlanders = FALSE, isSchade = FALSE, width = NULL, height = NULL) {
  
  
  # To prevent warnings with R CMD check
  locatie <- NULL
  
  unit <- match.arg(unit)
  unitName <- switch(unit,
    "absolute" = "",
    "relative" = "/100ha",
    "relativeDekking" = "/100ha bos & natuur",
  )
  
  wildNaam <- unique(data$wildsoort)
  title_wildnaam <- unlist(strsplit(wildNaam, split = ", "))
  titlePrefix <- if (!isSchade) "Gerapporteerd afschot" else "Evolutie schademeldingen"
  
  
  # Select data
  if (!isFlanders) {
    if (is.null(locaties))
      stop("Gelieve regio('s) te selecteren")
    plotData <- subset(data, locatie %in% locaties)
    colorList <- replicateColors(nColors = length(locaties))
  } else {
    plotData <- data
    colorList <- replicateColors(nColors = 1)
  }
  
  
  title <- paste0(titlePrefix, unitName,
    " voor ", 
    if (length(title_wildnaam) > 3) paste0(paste(tolower(title_wildnaam[1:3]), collapse = ", "), ", ...")
      else if (length(title_wildnaam) > 1) paste0(paste(tolower(title_wildnaam[1:length(title_wildnaam)-1]), collapse = ", "), " en ", tolower(title_wildnaam[length(title_wildnaam)]) )
      else (tolower(wildNaam)), 
    
    "\n in ", 
    if (isFlanders) {
        "Vlaanderen" 
      } else {
        if (length(locaties) > 3) paste0(paste(locaties[1:3], collapse = ", "), ", ...")
        else if (length(locaties) > 1) paste0(paste(locaties[1:length(locaties) - 1], collapse = ", "), " en ", locaties[length(locaties)])
        else paste(locaties, collapse = ", ") 
      },
    ifelse(timeRange[1] != timeRange[2],
      paste(" van", timeRange[1], "tot", timeRange[2]),
      paste(" in", timeRange[1])
    )
  )
  
  if (combinatie) {
    plotData <- plotData[, c("afschotjaar", "freq")]
    plotData <- aggregate(freq ~ afschotjaar, plotData, sum)
    plotData$locatie <- "Totaal"
  }
  
  # Filter NA's
  plotData <- plotData[!is.na(plotData$freq), ]
  
  
  # Create plot
  pl <- plot_ly(data = plotData, x = ~afschotjaar, y = ~freq,
      color = ~locatie, colors = colorList$colors, 
      hoverinfo = "x+y+name",
      type = "scatter", mode = "lines+markers",
      width = width, height = height) %>%
    layout(title = title,
      xaxis = list(title = "Jaar"), 
      yaxis = list(title = paste0("Aantal", unitName),
        tickformat = if (unit == "absolute") ",d" else NULL,
        range = c(0, ~max(freq)*1.05),
        rangemode = "nonnegative"),
      showlegend = TRUE,
      margin = list(b = 80, t = 100))     
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  # change variable names
  names(plotData)[names(plotData) == "freq"] <- paste0("aantal", unitName)
  
  
  return(list(plot = pl, data = plotData, warning = if (!is.null(colorList$warning))
        "Door het grote aantal gekozen regio's werden de kleuren van deze grafiek hergebruikt. 
          Hierdoor is verwarring mogelijk. Selecteer minder regio's om dit te voorkomen."))
  
}



#' Shiny module for creating the plot \code{\link{trendYearRegion}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams trendYearRegion 
#' @inheritParams createTrendData
#' @param geoData reactive object, geographical data for the selected species
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
trendYearRegionServer <- function(id, data, timeRange, species, regionLevel, locaties,
  geoData, allSpatialData, biotoopData = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$period <- renderUI({
          
          req(nrow(geoData()) > 0)
          
          sliderInput(inputId = ns("period"), 
            label = "Periode", 
            value = c(timeRange()[1], defaultYear),
            min = timeRange()[1],
            max = timeRange()[2],
            step = 1,
            sep = "")
          
        })
      
      # Create data for map, time plot
      timeData <- reactive({
          
          validate(need(input$period, "Gelieve periode te selecteren"))
          
          createTrendData(
            data = geoData(),
            allSpatialData = allSpatialData,
            biotoopData = biotoopData,
            timeRange = input$period,
            species = species(),
            regionLevel = regionLevel(),
            unit = input$unit
          )
          
        })      
      
      callModule(module = optionsModuleServer, id = "yearRegion", 
        data = timeData,
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "yearRegion",
        plotFunction = "trendYearRegion", 
        data = timeData,
        locaties = locaties,
        timeRange = reactive(input$period),
        unit = reactive(input$unit),
        combinatie = reactive(input$combinatie)
      )
      
    }) 
  
}


#' Shiny module for creating the plot \code{\link{trendYearRegion}} - ui side
#' 
#' @template moduleUI
#' @param title character, plot title in the app, overrides the automatic title
#' @param doHide boolean, whether to initially hide the plot; default TRUE
#' 
#' @author mvarewyck
#' @export
trendYearRegionUI <- function(id, uiText, title = NULL, doHide = TRUE) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkYearRegion"), 
      label = h3(HTML(if (!is.null(title)) title else uiText$title))),
    conditionalPanel(paste("input.linkYearRegion % 2 ==", as.numeric(doHide)), ns = ns,
      fixedRow(
        
        column(4, 
            wellPanel(
              uiOutput(ns("period")),
              selectInput(inputId = ns("unit"), label = "Eenheid",
                choices = c("Aantal" = "absolute", "Aantal/100ha" = "relative")
              ),
              checkboxInput(inputId = ns("combinatie"), 
                label = "Combineer alle geselecteerde regio's"),
              optionsModuleUI(id = ns("yearRegion"), exportData = TRUE,
                doWellPanel = FALSE)
          ),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, 
          plotModuleUI(id = ns("yearRegion"))
        ),
        tags$hr()
      )
    
    )
  )
  
  
}

