# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Create interactive plot for comparing age based on cheek or reported by hunter
#' 
#' Figure p. 9 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearAge
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the percentage per age category
#' based on cheek or hunter report in a stacked bar chart}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'jager': }{category based on the meldingsformulier}
#' \item{'kaak': }{category based on the lower jaw inspection}
#' \item{'freq': }{counts of animals}
#' \item{'percent': }{percentage in the entire category based on
#' the lower jaw inspection}
#' }
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @importFrom INBOtheme inbo.2015.colours
#' @author mvarewyck
#' @export
countAgeCheek <- function(data, wildNaam = "", jaartallen = NULL, 
    width = NULL, height = NULL) {
  
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  # Select data
  plotData <- data[data$afschotjaar %in% jaartallen, 
      c("leeftijdscategorie_MF", "Leeftijdscategorie_onderkaak")]
  names(plotData) <- c("jager", "kaak")
  
  # Percentage collected
  nRecords <- nrow(plotData)
  
  # Remove some categories
  plotData <- plotData[with(plotData, !is.na(jager) & jager != "Onbekend" &
              !is.na(kaak) & kaak != "Niet ingezameld"), ]
  percentCollected <- nrow(plotData)/nRecords
  
  # Define names and ordering of factor levels
  if ("Frisling" %in% plotData$jager) {  # wild zwijn
    
    newLevels <- c("Frisling", "Overloper", "Volwassen")
    
  } else {  # ree
    
    newLevels <- c("Kits", "Jongvolwassen", "Volwassen")
    
  }
  
  
  # Summarize data per province and year
  summaryData <- count(df = plotData, vars = names(plotData))
  freq <- NULL  # to prevent warnings with R CMD check 
  summaryData <- ddply(summaryData, "kaak", transform, 
      percent = freq / sum(freq) * 100)
  
  
 
  # For optimal displaying in the plot
  summaryData$jager <- factor(summaryData$jager, levels = newLevels)
  summaryData$kaak <- factor(summaryData$kaak, levels = newLevels)
  
  summaryData$text <- paste0(round(summaryData$percent), "%",
      " (", summaryData$freq, ")")
  
  totalCount <- count(df = summaryData, vars = "kaak", wt_var = "freq")$freq
  
  colors <- rev(inbo.2015.colours(n = nlevels(summaryData$jager)))
  title <- paste(wildNaam, paste0("(", 
          ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
      jaartallen), ")"))
  
  
  # Create plot
  pl <- plot_ly(data = summaryData, x = ~kaak, y = ~percent, color = ~jager,
          text = ~text,  hoverinfo = "x+text+name",
          colors = colors, type = "bar",  width = width, height = height) %>%
      layout(title = title,
          xaxis = list(title = "Categorie op basis van onderkaak"), 
          yaxis = list(title = "Percentage"),
          legend = list(y = 0.8, yanchor = "top"),
          margin = list(r = 300, b = 120, t = 100), 
          barmode = "stack",
          annotations = list(x = levels(summaryData$kaak), y = -10, 
              text = totalCount, xanchor = 'center', yanchor = 'bottom', 
              showarrow = FALSE)) %>%
      add_annotations(text = "Categorie op basis van meldingsformulier", 
          xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
          y = 0.8, yanchor = "bottom",    # Same y as legend below
          legendtitle = TRUE, showarrow = FALSE) %>%
      add_annotations(text = paste0(round(percentCollected, 2)*100, 
              "% ingezamelde onderkaken van totaal (", nrow(plotData), "/", nRecords, ")"),
          xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
          y = -0.3, yanchor = "bottom", showarrow = FALSE)
	
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  
	return(list(plot = pl, data = summaryData[, colnames(summaryData) != "text"]))
  
}
