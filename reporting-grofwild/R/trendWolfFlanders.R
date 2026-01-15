#' Create interactive plot for counts of schade by wolves in Flanders
#' 
#' @inheritParams countYearProvince
#' @inheritParams reportingGrofwild-common-args
#' @return list with:
#' \itemize{
#' \item 'plot':  plotly object, for a given species the observed number 
#' per year and per age category is plotted in a stacked bar chart 
#' \item 'data' data displayed in the plot, as data.frame with:
#' \itemize{
#' \item 'jaar': year at which the animals was counted 
#' \item count, depending if \code{summarizeBy} is:  
#' \itemize{
#' \item 'count':  counts of animals in the 'freq' column 
#' \item 'percent':  percentage of counts of animals in the 'percent'  column 
#' }
#' \item 'totaal':  total number of animals across categories 
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @export
trendWolfFlanders <- function(data, jaartallen = NULL, regio = "",
  width = NULL, height = NULL, sourceIndicator = NULL) {
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$year)
  
  # Select data
  data$group <- "Vlaams Gewest"
  plotData <- data[!is.na(data$year) & data$year %in% jaartallen & data$Schade == "Wolf", 
    c("year", "group")]
  names(plotData) <- c("year", "group") 
  
  
  # Summarize data per year
  summaryData <- count(df = plotData, vars = names(plotData))

  colors <- replicateColors(values = "Vlaams Gewest")$colors
  title <- paste0("Bevestigde schadegevallen door wolven ",
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
      paste("in", jaartallen)), 
    if (!all(regio == "")) paste0("\n(", toString(regio), ")") else ("\n in Vlaanderen")
  )
  
  # Create plot
  toPlot <- plot_ly(data = summaryData, x = ~year, 
        y = ~freq, color = ~group,
        hoverinfo = "x+y+name",
        colors = colors, type = "scatter", mode = "lines+markers",
        width = width, height = height) %>%
      plotly::layout(title = title,
        xaxis = list(
          title = "Jaar", 
          tickvals = unique(summaryData$year), 
          ticktext = unique(summaryData$year)), 
        yaxis = list(title = "Aantal bevestigde schadegevallen"),
        margin = list(b = 120, t = 100))
  
  
  
  colsFinal <- colnames(summaryData)
  
  # To prevent warnings in UI
  toPlot$elementId <- NULL
  
  
  return(list(plot = toPlot, data = summaryData[, colsFinal]))
  
}



#' Shiny module for creating the plot \code{\link{trendWolfFlanders}} - server side
#' @inheritParams countAgeGenderServer 
#' @return no return value
#' @author sjuniu
#' @import shiny
#' @export
trendWolfFlandersServer <- function(id, data, timeRange = reactive(NULL), 
  filterDataOnRegion = FALSE, preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "trendWolfFlanders", 
        data = data,
        timeRange = timeRange
      )
      toReturn <- callModule(module = plotModuleServer, id = "trendWolfFlanders",
        plotFunction = "trendWolfFlanders", 
        data = data,
        filterDataOnRegion = filterDataOnRegion,
        preSelected = preSelected)
      
      return(reactive(toReturn()))
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{trendWolfFlanders}} - UI side
#' @inherit welcomeSectionUI
#' @param plotFunction character, for matching file with plot titles
#' @inheritParams reportingGrofwild-common-args
#' @export
trendWolfFlandersUI <- function(id, uiText, plotFunction = "trendWolfFlanders", 
  context = "description", showTime = FALSE,
  doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(
    
    actionLink(inputId = ns("linkTrendWolfFlanders"), label = h3(HTML(title))),
    conditionalPanel(paste("input.linkTrendWolfFlanders % 2 ==", as.numeric(doHide)), ns = ns,
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("trendWolfFlanders"))
        ),
        column(4,
          optionsModuleUI(id = ns("trendWolfFlanders"),
            showTime = showTime, exportData = TRUE)
        )
      ),
      tags$br(),
      tags$div(class = "larger-description", HTML(description)),
      tags$hr()
    )
  )
  
}

