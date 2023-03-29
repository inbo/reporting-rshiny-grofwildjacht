# Project: grofWild_git
# 
# Author: dbemelmans
###############################################################################

#' Interactive barplot to show the distribution of the hunted animals in a certain interval
#' @inheritParams countYearAge
#' @param regio, empty function argument needed for generalization in \code{\link{plotModuleServer}}
#' @param interval character, data shown in intervals
#' should be one of \code{c("Per jaar", "Per maand", "Per kwartaal", "Per twee weken")}
#' @param groupVariable character, variable name in \code{data} for 
#' which colored bars should be plot
#' @param sourceIndicator_leeftijd character, source used to filter \code{data} ('leeftijd_comp_bron' column)
#' should be one of \code{c("inbo", "both")}, where \code{"both"} refers to both inbo and meldingsformulier, 
#' i.e. no filtering. Defaults to \code{"both"}
#' @param type character, used to filter the data
#' 
#' @author dbemelmans
#' @export 
countYearShotAnimals <- function(data, regio, jaartallen = NULL, width = NULL, height = NULL, 
  interval = c("Per jaar", "Per maand", "Per kwartaal", "Per twee weken"), 
  groupVariable, 
  sourceIndicator_leeftijd = c("both", "inbo"),
  type = NULL) {
  
  
  wildNaam <- unique(data$wildsoort)
  interval <- match.arg(interval)
  sourceIndicator_leeftijd <- match.arg(sourceIndicator_leeftijd)
  
  plotData <- data
 
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  # regions get already filtered out in the plotModuleServer 
  
  # Special case: inbo leeftijd_comp distinguishes frisling <6m and >6m
  if (groupVariable == "leeftijd_comp" & sourceIndicator_leeftijd == "inbo")
    groupVariable <- "leeftijd_comp_inbo"
  
  # Select on years & type
  plotData <- plotData[plotData$afschotjaar %in% jaartallen, 
      c("afschotjaar", "afschot_datum", groupVariable, "leeftijd_comp_bron")]
  if (!is.null(type) && type != "all")
    plotData <- plotData[plotData[, groupVariable] %in% c(type, "Onbekend"), ] #include onbekend for nRecords
  nRecords <- nrow(plotData)
    
  # Clean data for groupVariable
  plotData[, groupVariable] <- droplevels(plotData[, groupVariable])
  plotData[is.na(plotData[, groupVariable]), groupVariable] <- "Onbekend"
  if (groupVariable == "leeftijd_comp_inbo")
    plotData <- filterGrofwild(plotData = plotData, 
      sourceIndicator_leeftijd = sourceIndicator_leeftijd)
  if (!is.null(type) && type != "all") ## only retains animals of specified type
    plotData <- plotData[plotData[, groupVariable] %in% type, ]
  plotData$leeftijd_comp_bron <- NULL
  
#  plotData <- plotData[!is.na(plotData$afschot_datum), ]
  
  
  plotData$afschotjaar <- with(plotData, factor(afschotjaar, levels = 
        min(jaartallen):max(jaartallen)))
  
  colors <- replicateColors(values = c(loadMetaEco(species = wildNaam)[[groupVariable]], "Onbekend"))$colors
  
  title <- paste0("Afschot van ",
    ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
      jaartallen))
  
  # Summarize data per year
  plotData$afschotjaar <- droplevels(plotData$afschotjaar)
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
    summaryData <- melt(table(plotData[, c("afschotjaar", groupVariable)]), 
      id.vars = c("afschotjaar", groupVariable))
    summaryData$timeGroup <- as.numeric(as.factor(summaryData$afschotjaar))
  } else 
    summaryData <- melt(table(plotData[, c("afschotjaar", "timeGroup", groupVariable)]), 
      id.vars = c("afschotjaar", "timeGroup", groupVariable))
  
  # For optimal displaying in the plot
  summaryData$timeChar <- factor(newLevels[summaryData$timeGroup], levels = newLevels)
  summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
  
  # Create plot per year
  if (interval == "Per jaar") {
    allPlots <- plot_ly(data = summaryData,
            x = ~timeChar, y = ~value, type = "bar", 
            color = ~base::get(groupVariable), colors = colors,
            width = width, height = height) %>%
          plotly::layout(
            xaxis = list(title = ''),            
            annotations = list(x = totalCount$year,
              y = totalCount$value,
              text = totalCount$value,
              xanchor = 'center', yanchor = 'bottom',
              showarrow = FALSE))
  } else {
    allPlots <- lapply(seq_along(levels(summaryData$afschotjaar)), function(i) {
        iYear <- levels(summaryData$afschotjaar)[i]
        plot_ly(data = summaryData[summaryData$afschotjaar %in% iYear, ],
            x = ~timeChar, y = ~value, 
            text = paste0("Totaal in ", iYear, ": ", totalCount$value[totalCount$year == iYear]),
            type = "bar", hoverinfo = 'x+y+text+name', 
            color = ~base::get(groupVariable), colors = colors,
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
  pl <- subplot(allPlots, titleX = TRUE, shareY = TRUE) %>%
    plotly::layout(barmode = 'stack', showlegend = TRUE,
      title = title,
      yaxis = list(title = "Aantal"),
      margin = list(b = if (interval == "Per jaar") 120 else 150, t = 100)) %>% 
    add_annotations(text = percentCollected(
        nAvailable = sum(!is.na(plotData$afschot_datum) & plotData[, groupVariable] != "Onbekend"), 
        nTotal = nRecords,
        text = paste("gekende afschotdatum en", strsplit(groupVariable, split = "_")[[1]][1])),
      xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
      y = if (interval == "Per jaar") -0.25 else -0.3, yanchor = "bottom", showarrow = FALSE)
 
  colnames(summaryData)[colnames(summaryData) == "timeChar"] <- gsub("Per ", "", interval) 
  summaryData$group <- NULL
  
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  return(list(plot = pl, data = summaryData))
}



#' Shiny module for creating the plot \code{\link{countYearShotAnimals}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @param timeRange numeric vector of length 2, min and max year to subset data
#' @param types character vector
#' @inheritParams countYearShotAnimals
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countYearShotServer <- function(id, data, timeRange, types, groupVariable) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Verdeling afschot over de jaren
      callModule(module = optionsModuleServer, id = "countYearShot", 
        data = data,
        timeRange = timeRange,
        intervals = c("Per jaar", "Per maand", "Per kwartaal", "Per twee weken"),
        types = types,
        labelTypes = if (groupVariable == "leeftijd_comp") 
            "Leeftijdscategorie" else
            "Jachtmethode",
        multipleTypes = TRUE)
      callModule(module = plotModuleServer, id = "countYearShot",
        plotFunction = "countYearShotAnimals", 
        groupVariable = groupVariable,
        data = data)
           
    })
  
}


#' Shiny module for creating the plot \code{\link{countYearShotAnimals}} - UI side
#' @param regionLevels numeric vector, region level choices
#' @inheritParams countYearShotAnimals
#' @inherit welcomeSectionUI
#' 
#' @export
countYearShotUI <- function(id, groupVariable, regionLevels = NULL, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == paste0(as.character(match.call())[1], "-", groupVariable), ]
  
  tagList(
    
    actionLink(inputId = ns("linkYearShot"),
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkYearShot % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("countYearShot"), showTime = TRUE, 
            regionLevels = regionLevels, exportData = TRUE,
            showType = TRUE, showInterval = TRUE,
            showDataSource = if (groupVariable == "leeftijd_comp") "leeftijd"),
          tags$p(HTML(uiText[, strsplit(id, split = "_")[[1]][1]])),
        ),
        column(8, plotModuleUI(id = ns("countYearShot")))
      ),
      tags$hr(),
    )
  
  ) 
  
}

