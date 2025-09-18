# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Create interactive plot for counts per province and year
#' 
#' Figure p. 4 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param data data.frame with raw data for plotting
#' @param jaartallen integer vector, defines the year(s) that should be considered
#' in the plot; if NULL no selection on year(s) is made
#' @param type character, regional level of interest should be one of 
#' \code{c("provinces", "flanders", "faunabeheerzones")}
#' @inheritParams filterDataSource
#' @inheritParams reportingGrofwild-common-args
#' @param title character, title prefix; default is NULL
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return list with:
#' \itemize{
#' \item 'plot':  plotly object, for a given specie the observed number 
#' per year and per province is plotted in a stacked bar chart 
#' \item 'data':  data displayed in the plot, as data.frame with:
#' \itemize{
#' \item 'afschotjaar':  year at which the animals was shot 
#' \item 'locatie':  location name, could be province, flanders or fbz name 
#' \item 'value':  counts of animals 
#' }
#' }
#' @import plotly
#' @importFrom reshape2 melt
#' @importFrom stringr str_sort
#' @export
countYearProvince <- function(data, jaartallen = NULL, 
        type = c("provinces", "flanders", "faunabeheerzones"),
        interval = c("Per jaar", "Per maand", "Per kwartaal", "Per twee weken"), 
        sourceIndicator = NULL, title = NULL, width = NULL, height = NULL,
        regio = "") {
  
  type <- match.arg(type)
	wildNaam <- paste(unique(data$wildsoort), collapse = ", ")
  
  if (length(interval) > 1)
    interval <- "Per jaar"
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
  # filter for source
  plotData <- filterDataSource(plotData = data, sourceIndicator = sourceIndicator,
    returnStop = "message")
  
  isFbz <- FALSE
  if (all(regio == "Vlaams Gewest")) {
    plotData$locatie <- as.factor("Vlaams Gewest")
  } else if (all(regio %in% c("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren", "Onbekend"))) {
    plotData$locatie <- plotData$provincie
  } else {
    plotData$locatie <- plotData$FaunabeheerZone
    isFbz <- TRUE
  }
  
  if(nrow(plotData) == 0) {
    stop(paste0("Geen data beschikbaar voor de geselecteerde locatie: ", paste(regio, collapse = ", "), ". "))
  }
    
	# Select data
	plotData <- plotData[plotData$afschotjaar %in% jaartallen, c("afschotjaar", "afschot_datum", "locatie")]
	plotData <- plotData[!is.na(plotData$afschotjaar), ]
  if(nrow(plotData) == 0) {
    stop(paste0("Geen data beschikbaar voor de geselecteerde afschotjaren: ", jaartallen, "."))
  }
  nRecords <- nrow(plotData)
  
  
	# Exclude unused provinces/fbz's
  plotData$locatie <- droplevels(as.factor(plotData$locatie))
  
	# Summarize data per province and year
	plotData$afschotjaar <- with(plotData, factor(afschotjaar, levels = 
							min(jaartallen):max(jaartallen)))
  
  # Summarize data per year
  totalCount <- as.data.frame(table(plotData$afschotjaar))
  colnames(totalCount) <- c("year", "value")
  
  # Extract month/day
  plotData$maand <- as.numeric(format(plotData$afschot_datum, "%m"))
  plotData$dag <- as.numeric(format(plotData$afschot_datum, "%d"))
  
  
  if (interval == "Per jaar") {
    
    newLevels <- sort(unique(plotData$afschotjaar))
    
  } else if (interval == "Per maand") {
    
    newLevels <- c(
      "januari",
      "februari",
      "maart",
      "april",
      "mei",
      "juni", 
      "juli",
      "augustus",
      "september",
      "oktober",
      "november",
      "december")
    
    plotData$timeGroup <- plotData$maand
    
    
  } else if (interval == "Per kwartaal") {
    
    newLevels <- c("Kwartaal 1 (jan-mrt)", "Kwartaal 2 (apr-jun)", "Kwartaal 3 (jul-sept)", "Kwartaal 4 (okt-dec)")
    
    plotData$timeGroup <- ceiling(plotData$maand/3)
    
    
  } else if(interval == "Per twee weken") {
    
    plotData$timeGroup <- (plotData$maand-1)*2 + (plotData$dag > 15) + 1 
    
    newLevels <- c(
      "01/01-15/01",
      "16/01-31/01",
      "01/02-15/02",
      "16/02-28/02 of 29/02",
      "01/03-15/03",
      "16/03-31/03",
      "01/04-15/04",
      "16/04-30/04",
      "01/05-15/05",
      "16/05-31/05",
      "01/06-15/06",
      "16/06-30/06",
      "01/07-15/07",
      "16/07-31/07",
      "01/08-15/08",
      "16/08-31/08",
      "01/09-15/09",
      "16/09-30/09",
      "01/10-15/10",
      "16/10-31/10",
      "01/11-15/11",
      "16/11-30/11",
      "01/12-15/12",
      "16/12-31/12")
    
  }
  
  if (interval == "Per jaar") {
    summaryData <- melt(table(plotData[, c("afschotjaar", "locatie")]), 
      id.vars = c("afschotjaar", "locatie"))
    summaryData$timeGroup <- as.numeric(as.factor(summaryData$afschotjaar))
  } else {
    summaryData <- melt(table(plotData[, c("afschotjaar", "timeGroup", "locatie")]), 
      id.vars = c("afschotjaar", "timeGroup", "locatie"))
  }
  
  # Calculate percentages
  tmpPercent <- ddply(summaryData, "afschotjaar", transform, 
    percent = value / sum(value) * 100)
  summaryData <- merge(tmpPercent, summaryData, all.y = TRUE)
  summaryData$afschotjaar <- as.numeric(as.character(summaryData$afschotjaar))
  
  # For optimal displaying in the plot
  summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
  summaryData$timeChar <- factor(newLevels[summaryData$timeGroup], levels = newLevels)
  if (interval == "Per jaar")
    summaryData$timeChar <- as.numeric(as.character(summaryData$timeChar))
  
  # sort numerically again for fbz's (numeric and string combination is not well ordered by default)
  if (isFbz) {
    summaryData$locatie <- factor(summaryData$locatie, 
      levels = levels(droplevels(factor(unique(summaryData$locatie), 
            levels = c(as.character(1:10), "Onbekend")))))
  } else {
    newLevels <- levels(summaryData$locatie)
    summaryData$locatie <- factor(summaryData$locatie, 
      levels = c(newLevels[newLevels != "Onbekend"], newLevels[newLevels == "Onbekend"]))
  }
 
  # Hover text
  totalCount <- setNames(totalCount$value, totalCount$year)
  summaryData$text <- paste0(
    "n = ", summaryData$value,
    ifelse(is.na(summaryData$percent), "", paste0(" (", round(summaryData$percent), "%)")),
    paste0("<br><em>Totaal in ", summaryData$afschotjaar, "</em> ", totalCount[match(summaryData$afschotjaar, names(totalCount))])
  )
  
  colorList <- replicateColors(values = levels(summaryData$locatie))
  title <- paste0(
    if (!is.null(title)) paste0(title, "\n"), wildNaam, " ",
    ifelse(length(jaartallen) > 1, 
      paste(min(jaartallen), "tot", max(jaartallen)),
      jaartallen
    )
  )
  
  singleYear <- length(unique(summaryData$afschotjaar)) == 1
	
  # Create plot per year
  if (interval == "Per jaar") {
    
    allPlots <- plot_ly(data = summaryData, x = ~timeChar, y = ~value, 
        color = ~locatie, colors = colorList$colors, text = ~text, textposition = "none",
        hoverinfo = "x+text+name",
        type = "bar",  width = width, height = height) %>%
      plotly::layout(title = title,
        xaxis = list(title = "Jaar",
          tickvals = unique(summaryData$afschotjaar), 
          ticktext = unique(summaryData$afschotjaar)
        ), 
        yaxis = list(title = "Aantal"),
        margin = list(b = 80, t = 100), 
#        barmode = if (singleYear) "group" else "stack",
        showlegend = TRUE)  
  } else {
    allPlots <- lapply(seq_along(levels(summaryData$afschotjaar)), function(i) {
        iYear <- levels(summaryData$afschotjaar)[i]
        plot_ly(data = summaryData[summaryData$afschotjaar %in% iYear, ],
            x = ~timeChar, y = ~value, 
            text = ~text, textposition = "none",
            hoverinfo = "x+text+name", type = "bar",
            color = ~locatie, colors = colorList$colors,
            showlegend = i == 1,
            width = width, height = height) %>%
          plotly::layout(xaxis = list(title = "", showticklabels = FALSE)) %>%
          add_annotations(
            text = iYear,
            x = newLevels[round(length(newLevels)/2)], y = 0, xref = paste0("x", if (i != 1) i), yref = "paper", 
            yanchor = "top", textangle = 90, showarrow = FALSE)
      })
  }
  
  # Combine all plots
  pl <- subplot(allPlots, titleX = TRUE, shareY = TRUE, 
      margin = c(0.01, 0, 0, 0)) %>%
    plotly::layout(barmode = if (singleYear) "group" else "stack", showlegend = TRUE,
      title = title,
      yaxis = list(title = "Aantal"),
      margin = list(b = if (interval == "Per jaar") 120 else 150, t = 100)) %>% 
    add_annotations(text = percentCollected(
        nAvailable = sum(!is.na(plotData$afschot_datum) & plotData[, "locatie"] != "Onbekend"), 
        nTotal = nRecords,
        text = paste("gekende afschotdatum en", strsplit("locatie", split = "_")[[1]][1])),
      xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
      y = if (interval == "Per jaar") -0.25 else -0.3, yanchor = "bottom", showarrow = FALSE)
  
	
	# To prevent warnings in UI
	pl$elementId <- NULL
  
  # Change variable name
  names(summaryData)[names(summaryData) == "value"] <- "aantal"
  
  
	return(list(plot = pl, data = summaryData, warning = colorList$warning))
	
}




#' Shiny module for creating the plot \code{\link{countYearProvince}} - server side
#' @inheritParams trendYearRegionServer
#' @inheritParams countYearProvince
#' @inheritParams optionsModuleServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countYearProvinceServer <- function(id, data, types = NULL, labelTypes = "Type", 
  typesDefault = types, timeRange, title = reactive(NULL), allRegionsSelected = FALSE,
  preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
    
      output$disclaimerYearProvince <- renderUI({
          
          req(title())
          
          if (grepl("\\*", title()))
            getDisclaimerLimited()
          
        })

      
      # Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "yearProvince", 
        data = data,
        types = types,
        intervals = c("Per jaar", "Per maand", "Per kwartaal", "Per twee weken"),
        labelTypes = labelTypes,
        typesDefault = typesDefault,
        timeRange = timeRange,
        allRegionsSelected = allRegionsSelected
      )
      callModule(module = plotModuleServer, id = "yearProvince",
        plotFunction = "countYearProvince", 
        title = if (id == "dash") "Aantal drukjachten" else NULL,
        data = data,
        preSelected = preSelected)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countYearProvince}} - UI side
#' @inherit welcomeSectionUI
#' @inheritParams trendYearRegionUI
#' @inheritParams optionsModuleUI 
#' @inheritParams getOutputDescription
#' @export
countYearProvinceUI <- function(
  id, uiText, specie = NULL, plotFunction = "countYearProvinceUI",
  showType = FALSE, showTime = FALSE, showDataSource = NULL, showInterval = FALSE, 
  regionLevels = NULL, regionLevelSelected = NULL, doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, specie = specie, 
    uiText = uiText,
    type = strsplit(plotFunction, split = "-")[[1]][2])
  description <- getOutputDescription(output = plotFunction, 
    specie = specie, uiText = uiText, context = "description")
  
  tagList(
    
    actionLink(inputId = ns("linkYearProvince"), label = tags$h3(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkYearProvince % 2 ==", as.numeric(doHide)), 
      ns = ns,
      
      uiOutput(ns("disclaimerYearProvince")),
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("yearProvince"))
        ),
        column(4,
          optionsModuleUI(
            id = ns("yearProvince"), 
            regionLevels = regionLevels, 
            regionLevelSelected = regionLevelSelected,
            showTime = showTime, exportData = TRUE,
            showType = showType, showInterval = showInterval,
            showDataSource = showDataSource
          )
        )
      ),
      tags$p(HTML(description))
    )
  )
  
}