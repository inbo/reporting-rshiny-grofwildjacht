# Project: grofWild_git
# 
# Author: mvarewyck
###############################################################################




#' Create interactive plot for verwezenlijkt and toegekend afschot per jaar
#' @inheritParams countEmbryos
#' @param data data.frame, with \code{loadToekenningen()} for specific WBE 
#' @param currentYear numeric, current year to calculate accuracy for
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
percentageRealisedShot <- function(data, type = NULL,
  currentYear = as.numeric(format(Sys.Date(), "%Y")) - 1,
  jaartallen = NULL, 
  width = NULL, height = NULL){
  
  # R CMD check notes
  jaar <- NULL
  
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
  accData <- subset(plotData, jaar == currentYear)
  accuracy <- if (nrow(accData) > 0)
      round(accData$verwezenlijkt / accData$toegekend * 100) else
      0
  
  
  plotData$jaar <- as.factor(plotData$jaar)
  # percent 
  plotData$percent <- plotData$verwezenlijkt / plotData$toegekend * 100 
  # niet-verwezenlijkt
  plotData$`niet-verwezenlijkt` <- plotData$toegekend - plotData$verwezenlijkt
  
  percentages <- data.frame(
    value = paste0(round(plotData$percent), "%"),
    y = plotData$toegekend,
    x = plotData$jaar
  )
  
  summaryData <- melt(plotData[, c("jaar", "verwezenlijkt", "niet-verwezenlijkt")], id.vars = "jaar")
  summaryData <- merge(summaryData, plotData[, c("jaar", "toegekend")])
  
  title <- paste0("Toegekende en verwezenlijkte labels\n", paste(kboNaam, collapse = ","),
    paste0(" (", paste(type, collapse = ", "),") "),
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen)
  )
  
  
  colorList <- replicateColors(nColors = nlevels(summaryData$variable))
  colors <- colorList$colors
  names(colors) <- unique(summaryData$variable)
  
  pl <- plot_ly(data = summaryData, x = ~jaar, y = ~value, color = ~variable,
      text = ~paste("Toegekend:", toegekend), hoverinfo = "x+y+name+text",
      colors = colors, type = "bar", width = width, height = height) %>%
    
    layout(title = title,
      xaxis = list(title = "Labeljaar"), 
      yaxis = list(title = "Aantal"),
      margin = list(b = 120, t = 100, r = 10),
      legend = list(y = 0.8, yanchor = "top"),
      barmode = if (nrow(percentages) == 1) "group" else "stack",
      annotations = list(x = percentages$x, y = percentages$y, 
        text = paste(if(nrow(percentages) == 1) "totaal:" else "", percentages$value),
        xanchor = 'center', yanchor = 'bottom', showarrow = FALSE)) 
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  
  return(list(plot = pl, data = plotData,
      accuracy = list(value = accuracy, total = accData$toegekend)))
  
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
        multipleTypes = TRUE)
      callModule(module = plotModuleServer, id = "percentageRealisedShot",
        plotFunction = "percentageRealisedShot",
        data = data)
      
    })
  
}


#' Shiny module for creating the plot \code{\link{plotBioindicator}} - UI side
#' @inheritParams optionsModuleUI
#' @param showAccuracy boolean, whether to show gauge for accuracy
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
percentageRealisedShotUI <- function(id, showAccuracy = FALSE) {
  
  ns <- NS(id)
  
  tagList(
    
    actionLink(inputId = ns("linkPlotRealisedShot"), 
      label = h3("FIGUUR: Gerealiseerd afschot per jaar")
    ),
    conditionalPanel("input.linkPlotRealisedShot % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("percentageRealisedShot"),
            showTime = TRUE, showType = TRUE,
            exportData = TRUE),
          tags$p("Vergelijking tussen het verwezenlijkt en het toegekend afschot van ree.",
              "Het percentage bovenaan de staven geeft de graad van verwezenlijking weer.",
              "Binnen een wbe wordt best gestreefd naar een afschot van 100% van de toegekende labels."),
          if (showAccuracy)
              accuracyModuleUI(id = ns("percentageRealisedShot"), title = "Realisatie huidig jaar"),
        ),
        column(8, 
          plotModuleUI(id = ns("percentageRealisedShot"))
        ),
        tags$hr()
      )
    )
  )
  
}



