# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Create interactive plot for comparing age based on cheek or reported by hunter
#' 
#' Figure p. 9 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearProvince 
#' @return plotly object, for a given species the percentage per age category
#' based on cheek or hunter report in a stacked bar chart
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
    
    plotData$jager[plotData$jager == "Volwassen"] <- "Adult/Volwassen"
    plotData$jager[plotData$jager == "Adult"] <- "Adult/Volwassen"
    
    newLevelsJager <- c("Frisling", "Overloper", "Adult/Volwassen")
    newLevelsKaak <- c("Frisling", "Overloper", "Adult")
    
  } else {  # ree
    
    plotData$kaak[plotData$kaak == "Adult"] <- "Volwassen"
    
    newLevelsJager <- c("Kits", "Jongvolwassen", "Volwassen")
    newLevelsKaak <- c("Kits", "Jongvolwassen", "Volwassen")
    
  }
  
  
  # Summarize data per province and year
  summaryData <- count(df = plotData, vars = names(plotData))
  freq <- NULL  # to prevent warnings with R CMD check 
  summaryPercent <- ddply(summaryData, "kaak", transform, 
      percent = freq / sum(freq) * 100)
  
  
 
  # For optimal displaying in the plot
  summaryPercent$jager <- factor(summaryPercent$jager, levels = newLevelsJager)
  summaryPercent$kaak <- factor(summaryPercent$kaak, levels = newLevelsKaak)
  
  summaryPercent$text <- paste(round(summaryPercent$percent, 2), "%")
  
  colors <- rev(inbo.2015.colours(n = nlevels(summaryPercent$jager)))
  title <- paste(wildNaam, paste0("(", 
          ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
      jaartallen), ")"))
  subTitle <- "Categorie op meldingsformulier versus onderkaak"
  
  
  # Create plot
  plot_ly(data = summaryPercent, x = ~kaak, y = ~percent, color = ~jager,
          text = ~text,  hoverinfo = "x+text+name",
          colors = colors, type = "bar",  width = width, height = height) %>%
      layout(title = title,
          xaxis = list(title = "Categorie op basis van onderkaak"), 
          yaxis = list(title = "Percentage"),
          legend = list(y = 0.8, yanchor = "top"),
          margin = list(r = 300, b = 120, t = 100), 
          barmode = "stack") %>%
      add_annotations(text = "Categorie op basis van meldingsformulier", 
          xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
          y = 0.8, yanchor = "bottom",    # Same y as legend below
          legendtitle = TRUE, showarrow = FALSE) %>%
      add_annotations(text = paste0(round(percentCollected, 4)*100, 
              "% ingezamelde onderkaken van totaal (", nrow(plotData), "/", nRecords, ")"),
          xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
          y = -0.3, yanchor = "bottom", showarrow = FALSE)
  
}
