# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplot for Inschatting van het risico op verkeersongelukken (F07_3)
#' 
#' @param data dataframe with columns including percentage, Vraag and Antwoord 

#' @return list with plotly object and data.frame
#' 
#' @author wverlinden
#' @import plotly
#' @export 
barInschatting <- function(data) {
  
  plotData <- data[data$Vraag != "populatie_evolutie", ]
  plotData$percentage <- as.numeric(plotData$percentage)
  
  plotData$Antwoord <- factor(plotData$Antwoord , 
    levels = c('Erg veel toegenomen','Veel toegenomen',
      'Beetje toegenomen', 'Hetzelfde gebleven',
      'Beetje afgenomen', 'Veel afgenomen',
      'Erg veel afgenomen', 'Geen mening'))
  
  myPlot <- plot_ly(plotData, x = ~percentage, y = ~Vraag, type = 'bar', name = ~Antwoord, 
      hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
        '<b>Antwoord</b>: %{text}<extra></extra>'),
      text = ~Antwoord,
      textposition = "none") %>%
    layout(
      legend = list(title = list(text = "<b> Antwoord </b>")),
      barmode = "stack",
      yaxis = list(title = "Vraag",
        ticktext = list("Schade in het verkeer", "Schade aan priveterreinen", "Schade aan de landbouw"), 
        tickvals = list(2,1,0),
        tickmode = "array"),
      xaxis = list(title = "Percentage") 
    ) 
  
  
  return(list(plot = myPlot, data = plotData[, c("Vraag", "Antwoord", "percentage")]))
  
}

