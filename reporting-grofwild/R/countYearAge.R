# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' Create interactive plot for counts per age category and year
#' 
#' Figure p. 11 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearProvince
#' @param regio character vector, names of the selected regions in \code{data}
#' to be shown in the plot title
#' @return plotly object, for a given species the observed number 
#' per year and per age category is plotted in a stacked bar chart
#' @import plotly
#' @importFrom plyr count ddply
#' @export
countYearAge <- function(data, wildNaam = "", jaartallen = NULL, regio = "",
    doodsoorzaak = "afschot", width = NULL, height = NULL) {
  
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  # Select data
  plotData <- data[data$afschotjaar %in% jaartallen &
          data$doodsoorzaak %in% doodsoorzaak, c("afschotjaar", "Leeftijdscategorie_onderkaak")]
  names(plotData) <- c("jaar", "kaak")
  
  # Percentage collected
  nRecords <- nrow(plotData)
  
  # Remove some categories
  plotData <- plotData[!is.na(plotData$jaar) & !is.na(plotData$kaak), ]
  percentCollected <- nrow(plotData)/nRecords
  
  # Define names and ordering of factor levels
  if ("Frisling" %in% plotData$kaak) {  # wild zwijn
    
    newLevelsKaak <- c("Frisling", "Overloper", "Adult", "Niet ingezameld")
    
  } else {  # ree
    
    plotData$kaak[plotData$kaak == "Adult"] <- "Volwassen"
    
    newLevelsKaak <- c("Kits", "Jongvolwassen", "Volwassen", "Niet ingezameld")
    
  }
  
  # Summarize data per year and age category
  summaryData <- count(df = plotData, vars = names(plotData))
  
  
  # Add line for records with 0 observations
  fullData <- cbind(expand.grid(jaar = min(summaryData$jaar):max(summaryData$jaar),
          kaak = unique(summaryData$kaak)))
  summaryData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
  summaryData$freq[is.na(summaryData$freq)] <- 0
  
  
  # Calculate percentages excluding "niet ingezameld"
  kaak <- NULL  # to prevent warnings with R CMD check
  subData <- subset(summaryData, kaak != "Niet ingezameld")
  freq <- NULL  # to prevent warnings with R CMD check 
  subData <- ddply(subData, "jaar", transform, 
      percent = freq / sum(freq) * 100)
  subDataMissing <- subset(summaryData, kaak == "Niet ingezameld")
  subDataMissing$percent <- NA
  summaryData <- rbind(subData, subDataMissing)
  
  # Summarize data per year
  totalCount <- count(df = plotData, vars = "jaar")
  totalCount$totaal <- totalCount$freq
  totalCount$freq <- NULL
  
  summaryData <- merge(summaryData, totalCount)

  
  # For optimal displaying in the plot
  summaryData$kaak <- factor(summaryData$kaak, levels = newLevelsKaak)
  summaryData$jaar <- as.factor(summaryData$jaar)
  
  summaryData$text <- paste0("<b>Jaar ", summaryData$jaar, "</b>",
      "<br>", summaryData$freq, 
      "<br>Totaal: ", summaryData$totaal,
      ifelse(is.na(summaryData$percent), "", 
          paste0("<br><br><i>Subset ingezamelde onderkaken </i>",
              "<br>", round(summaryData$percent), "% van totaal aantal ingezamelde onderkaken"))
  )
  
  colors <- c("#989868", "#688599", "#CC3D3D", "grey70")
  names(colors) <- newLevelsKaak
  
  title <- paste0(wildNaam, " ",
      ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
          jaartallen),
      " (", paste(doodsoorzaak, collapse = " en "), 
      if (!all(regio == "")) 
        paste(" in", paste(regio, collapse = " en ")),
      ")")
  
  
  
  # Create plot
  plot_ly(data = summaryData, x = ~jaar, y = ~freq, color = ~kaak,
          text = ~text,  hoverinfo = "text+name",
          colors = colors, 
          type = "scatter", mode = "lines+markers", 
          width = width, height = height) %>%
      layout(title = title,
          xaxis = list(title = "Jaar"), 
          yaxis = list(title = "Aantal"),
          margin = list(b = 80, t = 100))     
  
  
}

