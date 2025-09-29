# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplots for Aantrekkingskracht
#' 
#' @param data data.frame with summarized data (1 line per group) 
#' @param yVar NULL, placeholder for consistency with \code{\link{pciDraagvlak}}
#' @return list with plotly object and data.frame
#' 
#' @author mvarewyck
#' @import plotly
#' @importFrom INBOtheme inbo_palette
#' @export 
barDraagkracht <- function(data, yVar = NULL) {
  

  data$yesPercentage <- as.numeric(data$Aantal_tot/data$totaal)
  data$yesPercentageLabel <- paste0(round(data$yesPercentage * 100, 2), "%")
  data$noPercentage <- 1 - data$yesPercentage
  data$noPercentageLabel <- paste0(round(data$noPercentage * 100, 2), "%")
  
  data[, "Year"] <- droplevels(data[, "Year"])
  data <- data[order(data$Year), ]

  myColors <- inbo_palette()[1:2]
  
  groupLevels <- unique(data$Sector)
  
  plotList <- list()
  
  for (iLevel in groupLevels) {
    
    subData <- data[data$Sector == iLevel, ]
    
    plotList[[iLevel]] <- plot_ly(
        data = subData, 
        x = ~Year, 
        y = ~yesPercentage, 
        text = ~paste("<b>", iLevel, "</b><br> Ja:", yesPercentageLabel, "(n =", totaal, ")"), 
        textposition = "none", name = "Ja",
        type = 'bar', marker = list(color = myColors[1]), 
        showlegend = (iLevel == groupLevels[1]),
        hoverinfo = "text"
      ) %>%
      plotly::add_trace(y = ~noPercentage, name = "Nee",
        text = ~paste("<b>", iLevel, "</b><br> Nee:", noPercentageLabel, "(n=", totaal, ")"),
        marker = list(color = myColors[2])
      ) %>% 
      plotly::layout(
        barmode = "stack",
        annotations = list(
          # groupVariable text
          list(x = 0.5, y = 1, 
            text = paste0("<b>", iLevel, "</b>"),
            showarrow = FALSE, font = list(size = 12),
            xref = 'paper', yref = 'paper',
            xanchor = 'center', 
            yanchor = 'bottom')
        ),
        
        shapes = list(
#          # vertical line between plots
#          list(
#            type = "line",
#            x0 = 1.1,
#            x1 = 1.1,
#            xref = "paper",
#            y0 = -0.2,
#            y1 = 1.2,
#            yref = "paper",
#            line = list(color = "gray80")
#          ),
          # groupVariable gray bar
          list(
            type = "rect",
            y0 = 1,
            y1 = 1.1,
            yref = "paper",
            x0 = 0, 
            x1 = 1,
            xref = "paper",
            fillcolor = toRGB("gray80"),
            line = list(color = "transparent")
          )
        ),
        legend = list(title = list(text = "<b>Antwoord</b>")),
        yaxis = list(title = "", 
          # for absolute values as tickmarks
          tickvals = seq(-1, 1, by = 0.2),
          ticktext = paste0(abs(seq(-100, 100, by = 20)), "%")
        ),
        xaxis = list(title = "", ticksuffix = "  ")
      )
    
  }
  
  
  myPlot <- do.call(subplot, c(plotList,
      shareY = TRUE,
      nrows = 1,
      margin = 0.03))
  
  
  return(list(plot = myPlot, data = data))
  
}
