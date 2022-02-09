# TODO: Add comment
# 
# Author: mvarewyck
###############################################################################


#' Create interactive plot for verwezenlijkt and toegekend afschot per jaar
#' @inheritParams countEmbryos
#' @inheritParams tableProvince 
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
countYearRealizedShot <- function(
  data,
  assignedData,
  type = NULL,
  jaartallen = NULL, 
  width = NULL, height = NULL){
  
  
  wildNaam <- unique(data$wildsoort)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  
  data$labeltype <- tolower(gsub("REE", "", data$labeltype))
  
  if (is.null(type))
    type <- unique(data$labeltype)[unique(data$labeltype) != ""]
  
  
  # Select on years/type
  plotData <- data[data$afschotjaar %in% jaartallen & data$labeltype %in% type,
    c("afschotjaar", "labeltype")]
  
  sumData <- as.data.frame(table(plotData$afschotjaar, dnn = "jaar"),
    stringsAsFactors = FALSE)
  
  if (nrow(sumData) == 0)
    stop("Geen data beschikbaar")
  
  # Combine with assignedData
  assignedData <- aggregate(Aantal ~ Jaar, data = assignedData, sum)
  allData <- merge(sumData, assignedData, by.x = "jaar", by.y = "Jaar", all = TRUE)
  allData$Freq[is.na(allData$Freq)] <- 0
  colnames(allData) <- c("jaar", "verwezenlijkt", "toegekend")
  allData$percent <- allData$verwezenlijkt / allData$toegekend * 100 
  
  percentages <- data.frame(
    value = paste0(round(allData$percent), "%"),
    y = allData$toegekend,
    x = allData$jaar
  )
  
  # toegekend = niet-verwezenlijkt in graph
  allData$toegekend <- allData$toegekend - allData$verwezenlijkt
  summaryData <- melt(allData[, c("jaar", "verwezenlijkt", "toegekend")], id.vars = "jaar")
  
  title <- paste0(wildNaam, " toegekende en verwezenlijkte labels ",
    paste0("(", paste(type, collapse = ", "),")\n"),
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen)
    )
  
  
  colorList <- replicateColors(nColors = nlevels(summaryData$variable))
  colors <- colorList$colors
  names(colors) <- unique(summaryData$variable)
  
  pl <- plot_ly(data = summaryData, x = ~jaar, y = ~value, color = ~variable,
      hoverinfo = "x+y+name",
      colors = colors, type = "bar", width = width, height = height) %>%
    
    layout(title = title,
      xaxis = list(title = "Afschotjaar"), 
      yaxis = list(title = "Aantal"),
      margin = list(b = 120, t = 100, r = 10),
      legend = list(y = 0.8, yanchor = "top"),
      barmode = if (nrow(percentages) == 1) "group" else "stack",
      annotations = list(x = percentages$x, y = percentages$y, 
        text = paste(if(nrow(percentages) == 1) "totaal:" else "", percentages$value),
        xanchor = 'center', yanchor = 'bottom', showarrow = FALSE)) 
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  
  return(list(plot = pl, data = allData))
  
}



##' Shiny module for creating the plot \code{\link{countYearRealizedShot}} - server side
##' @inheritParams countAgeGenderServer 
##' @inheritParams optionsModuleServer 
##' @inheritParams countYearRealizedShot
##' @return no return value
##' 
##' @author mvarewyck
##' @import shiny
##' @export
#countYearRealizedShotServer <- function(id, data, timeRange, types, typesDefault,
#  bioindicator = c("onderkaaklengte", "ontweid_gewicht")) {
#  
#  moduleServer(id,
#    function(input, output, session) {
#      
#      ns <- session$ns
#      
#      # Bioindicator plot
#      callModule(module = optionsModuleServer, id = "countYearRealizedShot", 
#        data = data,
#assignedData = assignedData,
#        timeRange = timeRange,
#        types = types,
#        typesDefault = typesDefault,
#        multipleTypes = TRUE)
#      callModule(module = plotModuleServer, id = "plotBioindicator",
#        plotFunction = "plotBioindicator", 
#        bioindicator = bioindicator,
#        data = data)
#      
#    })
#  
#}
#
#
##' Shiny module for creating the plot \code{\link{plotBioindicator}} - UI side
##' @inheritParams plotBioindicatorServer
##' @inheritParams optionsModuleUI
##' @return UI object
##' 
##' @author mvarewyck
##' @import shiny
##' @export
#plotBioindicatorUI <- function(id, bioindicator = c("onderkaaklengte", "ontweid_gewicht"), regionLevels) {
#  
#  ns <- NS(id)
#  
#  tagList(
#    
#    actionLink(inputId = ns("linkPlotBioindicator"), 
#      label = h3(switch(bioindicator,
#          ontweid_gewicht = "FIGUUR: Gewicht per jaar",
#          onderkaaklengte = "FIGUUR: Onderkaaklengte per jaar (INBO of Meldingsformulier)"
#        ))
#    ),
#    conditionalPanel("input.linkPlotBioindicator % 2 == 1", ns = ns,
#      
#      fixedRow(
#        
#        column(4,
#          optionsModuleUI(id = ns("plotBioindicator"),
#            showTime = TRUE, showType = TRUE,
#            regionLevels = regionLevels, exportData = TRUE,
#            showDataSource = switch(bioindicator,
#              ontweid_gewicht = c("leeftijd", "geslacht"),
#              onderkaaklengte = c("onderkaak", "leeftijd", "geslacht")
#            )),
#          switch(bioindicator,
#            ontweid_gewicht = tagList(
#              tags$p("Evolutie van de gerapporteerde leeggewichten (met 95% betrouwbaarheidsinterval) doorheen de geselecteerde jaren voor de gekozen regio en types."),
#              tags$p(tags$i("Opmerking: Observaties met leeggewicht < 5kg of > 25kg zijn niet opgenomen in de figuur."))
#            ),
#            onderkaaklengte = tags$p("Verdeling van de onderkaaklengte voor alle gegevens uit de geselecteerde periode, regio('s) en type(s). Indien de leeftijdscategorie van INBO o.b.v. ingezamelde onderkaak gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier gebruikt.")
#          )
#        ),
#        column(8, 
#          plotModuleUI(id = ns("plotBioindicator"))
#        ),
#        tags$hr()
#      )
#    )
#  )
#  
#}
