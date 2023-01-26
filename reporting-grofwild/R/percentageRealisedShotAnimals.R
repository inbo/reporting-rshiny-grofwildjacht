# Project: grofWild_git
# 
# Author: mvarewyck
###############################################################################




#' Create interactive plot for verwezenlijkt and toegekend afschot per jaar
#' @inheritParams countEmbryos
#' @param data data.frame, with \code{loadToekenningen()} for specific WBE 
#' @param unit character, which values to show on the y-axis;
#' should be one of \code{c("absolute", "percentage")}; default \code{"absolute"}
#' @param regio, empty function argument needed for generalization in \code{\link{plotModuleServer}}
#' @param unit character, which values to show on the y-axis;
#' should be one of \code{c("absolute", "percentage")}; default \code{"absolute"}
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for the specified type and jaartallen}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'jaar': }{year at which the animal was shot}
#' \item{'verwezenlijkt': }{count for actually shot animals}
#' \item{'toegekend': }{count for assigned animals}
#' \item{'percent': }{verwezenlijkt / toegekend * 100}
#' }}
#' }
#' @author mvarewyck
#' @import plotly
#' @export
percentageRealisedShot <- function(data, regio, type = NULL,
  jaartallen = NULL, unit = c("absolute", "percentage"),
  width = NULL, height = NULL){
  
  # R CMD check notes
  jaar <- NULL
  
  unit <- match.arg(unit)
  
  kboNaam <- unique(data$WBE_Naam_Toek)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$labeljaar)[!is.na(unique(data$labeljaar))]
  
  
  if (is.null(type))
    type <- unique(data$labeltype)[!is.na(unique(data$labeltype))]
  
  
  # Select on years/type
  plotData <- data[data$labeljaar %in% jaartallen & data$labeltype %in% type,
    c("labeljaar", "labeltype", "toegekend", "verwezenlijkt")]
  
  if (nrow(plotData) == 0)
    stop("Geen data beschikbaar")
  
  # Aggregate over type
  plotData <- aggregate(plotData[, c('toegekend', 'verwezenlijkt')], 
    by = list(jaar = plotData$labeljaar), sum)
  
  # Calculate accuracy
  accData <- colSums(subset(plotData, jaar %in% jaartallen))
  accuracy <- if (length(accData) > 0)
      round(accData[names(accData) == "verwezenlijkt"] / 
          accData[names(accData) == "toegekend"] * 100) else
      0
  totalAccuracy <- accData[names(accData) == "toegekend"]
  names(totalAccuracy) <- NULL
  names(accuracy) <- NULL
  
  
  plotData$jaar <- as.factor(plotData$jaar)
  # percent 
  plotData$percent <- plotData$verwezenlijkt / plotData$toegekend * 100 
  # niet-verwezenlijkt
  plotData$`niet-verwezenlijkt` <- plotData$toegekend - plotData$verwezenlijkt
  
#  percentages <- data.frame(
#    value = paste0(round(plotData$percent), "%"),
#    y = plotData$toegekend,
#    x = plotData$jaar
#  )

  title <- paste0(
    if (unit == "absolute") 
        "Toegekende en verwezenlijkte labels\n" else
        "Percentage verwezenlijkte labels\n",
    paste(kboNaam, collapse = ","),
    paste0(" (", paste(type, collapse = ", "),") "),
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen)
  )
  
  if (unit == "absolute") {
    
    summaryData <- melt(plotData[, c("jaar", "verwezenlijkt", "niet-verwezenlijkt")], id.vars = "jaar")
    summaryData <- merge(summaryData, plotData[, c("jaar", "toegekend")])
    
    colorList <- replicateColors(nColors = nlevels(summaryData$variable))
    colors <- colorList$colors
    names(colors) <- unique(summaryData$variable)
    
    pl <- plot_ly(data = summaryData, x = ~jaar, y = ~value, color = ~variable,
      text = ~paste("Toegekend:", toegekend), textposition = "none", 
      hoverinfo = "x+y+name+text",
      colors = colors, type = "bar", width = width, height = height)
    
  } else {
    
    colors <- replicateColors(nColors = 3)$colors
    
    pl <- plot_ly(data = plotData, x = ~jaar, y = ~round(percent, 2),
      text = ~paste("Toegekend:", toegekend),
      textposition = "none", hoverinfo = "x+y+text", 
      marker = list(color = colors[1]),
      type = "bar", width = width, height = height)
    
  }
  
  pl <- pl %>%
    layout(title = title,
      xaxis = list(title = "Labeljaar"), 
      yaxis = list(
        title = if (unit == "absolute") "Aantal" else "Percentage",
        range = if (unit == "percentage") c(0, 100)),
      margin = list(b = 120, t = 100, r = 10),
      legend = list(y = 0.8, yanchor = "top"),
      barmode = if (length(unique(plotData$jaar)) == 1) "group" else "stack"
    )
#      annotations = list(x = percentages$x, y = percentages$y, 
#        text = paste(if(nrow(percentages) == 1) "totaal:" else "", percentages$value),
#        xanchor = 'center', yanchor = 'bottom', showarrow = FALSE)) 
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  
  return(list(plot = pl, data = plotData,
      accuracy = list(value = accuracy, total = totalAccuracy)))
  
}



#' Shiny module for creating the plot \code{\link{percentageRealisedShot}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams optionsModuleServer 
#' @inheritParams percentageRealisedShot
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
percentageRealisedShotServer <- function(id, data, timeRange, types) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Gerealiseerd afschot
      callModule(module = optionsModuleServer, id = "percentageRealisedShot", 
        timeRange = timeRange,
        types = types,
        multipleTypes = TRUE,
        data = data)
      callModule(module = plotModuleServer, id = "percentageRealisedShot",
        plotFunction = "percentageRealisedShot",
        data = data,
        unit = reactive(input$percentageRealisedUnit)
      )
      
    })
  
}


#' Shiny module for creating the plot \code{\link{plotBioindicator}} - UI side
#' @param showAccuracy boolean, whether to show gauge for accuracy
#' @param regionLevels numeric vector, region level choices
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
percentageRealisedShotUI <- function(id, showAccuracy = FALSE, regionLevels = NULL, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkPlotRealisedShot"), 
      label = h3(HTML(uiText$title))
    ),
    conditionalPanel("input.linkPlotRealisedShot % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          wellPanel(
            selectInput(inputId = ns("percentageRealisedUnit"), label = "Eenheid",
              choices = c("Aantal" = "absolute", "Percentage" = "percentage")),
            optionsModuleUI(id = ns("percentageRealisedShot"),
              showTime = TRUE, showType = TRUE, regionLevels = regionLevels,
              exportData = TRUE,
              doWellPanel = FALSE)
          ),
          tags$p(HTML(uiText[, id])),
          if (showAccuracy)
            accuracyModuleUI(id = ns("percentageRealisedShot"), title = "Realisatie geselecteerde periode"),
        ),
        column(8, 
          plotModuleUI(id = ns("percentageRealisedShot"))
        ),
        tags$hr()
      )
    )
  )
  
}



