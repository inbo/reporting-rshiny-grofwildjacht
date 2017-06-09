#' Create interactive version of plot at page 7
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return plotly object
#' @import plotly
#' @importFrom RColorBrewer brewer.pal
#' @importFrom plyr ddply .
#' @export
createPlot2 <- function(width = NULL, height = NULL) {
  
  d2006 <- c(2, 3)
  d2007 <- c(sample.int(5, size = 15, replace = TRUE), 
      sample.int(5, size = 2, replace = TRUE) + 5, 
      sample.int(5, size = 1, replace = TRUE) + 10, 
      sample.int(10, size = 0, replace = TRUE) + 20, 
      sample.int(10, size = 0, replace = TRUE) + 30)
  d2008 <- c(sample.int(5, size = 14, replace = TRUE), 
      sample.int(5, size = 2, replace = TRUE) + 5, 
      sample.int(5, size = 1, replace = TRUE) + 10, 
      sample.int(10, size = 1, replace = TRUE) + 20, 
      sample.int(10, size = 0, replace = TRUE) + 30)
  d2009 <- c(sample.int(5, size = 12, replace = TRUE), 
      sample.int(5, size = 5, replace = TRUE) + 5, 
      sample.int(5, size = 0, replace = TRUE) + 10, 
      sample.int(10, size = 1, replace = TRUE) + 20, 
      sample.int(10, size = 1, replace = TRUE) + 30)
  d2010 <- c(sample.int(5, size = 8, replace = TRUE), 
      sample.int(5, size = 4, replace = TRUE) + 5, 
      sample.int(5, size = 1, replace = TRUE) + 10, 
      sample.int(10, size = 0, replace = TRUE) + 20, 
      sample.int(10, size = 2, replace = TRUE) + 30)
  d2011 <- c(sample.int(5, size = 11, replace = TRUE), 
      sample.int(5, size = 1, replace = TRUE) + 5, 
      sample.int(5, size = 4, replace = TRUE) + 10, 
      sample.int(10, size = 1, replace = TRUE) + 20, 
      sample.int(10, size = 3, replace = TRUE) + 30)
  d2012 <- c(sample.int(5, size = 10, replace = TRUE), 
      sample.int(5, size = 4, replace = TRUE) + 5, 
      sample.int(5, size = 5, replace = TRUE) + 10, 
      sample.int(10, size = 3, replace = TRUE) + 20, 
      sample.int(10, size = 6, replace = TRUE) + 30)
  d2013 <- c(sample.int(5, size = 11, replace = TRUE), 
      sample.int(5, size = 5, replace = TRUE) + 5, 
      sample.int(5, size = 8, replace = TRUE) + 10, 
      sample.int(10, size = 1, replace = TRUE) + 20, 
      sample.int(10, size = 8, replace = TRUE) + 30)
  d2014 <- c(sample.int(5, size = 12, replace = TRUE), 
      sample.int(5, size = 6, replace = TRUE) + 5, 
      sample.int(5, size = 6, replace = TRUE) + 10, 
      sample.int(10, size = 1, replace = TRUE) + 20, 
      sample.int(10, size = 8, replace = TRUE) + 30)
  
  
  dataVals <- c(d2006, d2007, d2008, d2009, d2010, d2011, d2012, d2013, 
      d2014)
  dataYears <- c(rep(2006, length(d2006)), 
      rep(2007, length(d2007)),
      rep(2008, length(d2008)),
      rep(2009, length(d2009)),
      rep(2010, length(d2010)),
      rep(2011, length(d2011)),
      rep(2012, length(d2012)),
      rep(2013, length(d2013)),
      rep(2014, length(d2014))
  )
  
  data <- cbind.data.frame(dataYears, dataVals)
  colnames(data) <- c("Year", "Value")
  data$Year <- factor(data$Year)
  
  provinces <- c(rep("Antwerpen", 70), 
      rep("Vlaams-Brabant", 65), 
      rep("Oost-Vlaanderen", 65), 
      rep("Limburg", 44), 
      rep("West-Vlaanderen", 64)
  )
  
  data$Province <- sample( provinces, nrow(data))
  
  dataDir <- system.file("extdata", package = "grofWild")
  communeData <- readShapeData(zipFile = file.path(dataDir, "communes.zip"))  
  allCommunes <- communeData$NAAM
  
  
  # Count number of duplicated rows
  data$Group <- cut(data$Value, breaks = c(0, 5, 10, 20, 30, 50),
      labels = c("1-5", "6-10", "11-20", "21-30", ">30"))
  plotData <- ddply(data, .(Year, Group), nrow)
  plotData$hoverText <- apply(plotData, 1, function(x)
        paste0("<b>Gemeenten in ", x["Year"],":</b><br>", 
            paste(sample(allCommunes, size = x['V1'], replace = FALSE), collapse = "<br>")))
  
  plotSummary <- tapply(plotData$V1, plotData$Group, mean)
  plotSummaryText <- tapply(plotData$hoverText, plotData$Group, function(x) paste(x, collapse = "<br>"))
  plotData <- rbind(plotData, 
      data.frame(Year = rep("Gemiddelde", length(plotSummary)), 
          Group = names(plotSummary),
          V1 = as.numeric(plotSummary),
          hoverText = plotSummaryText))
  nColors <- nlevels(plotData$Year) - 1
#  colors <- c(colorRampPalette(brewer.pal(nColors, "Spectral"))(nColors), "Grey")
  pal1 <- brewer.pal(12,"Paired")[c(1:10, 12)] # eliminate the bad yellow
  pal2 <- brewer.pal(8, "Set2")[c(1,2,4,5,6)] # eliminate the last two
  colors <- c(c(pal1, pal2)[1:nColors], "Grey") # total of 16 colors
#   plotData$symbols <- ifelse(plotData$Province == "Gemiddelde", 19, 18)
  
  plot_ly(x = plotData$Group, y = plotData$V1, color = plotData$Year,
          colors = colors,
          type = "scatter", 
          mode = "lines+markers",
          text = plotData$hoverText,
		  width = width, height = height) %>%
      layout(xaxis = list(title = "Aantal everzwijnen"), 
          yaxis = list(title = "Aantal gemeenten"),
          margin = list(b = 120)
#          annotations = list(
#              list(xref = "paper", yref = "paper", 
#                  xanchor = "left", yanchor = "bottom",
#                  x = 1.02, y = 0.4, showarrow = FALSE, bordercolor = "white",
#                  font = list(family = "arial", size = 14), 
#                  text = "Aantal overlappende<br>punten bepaalt<br>puntgrootte<br>(behalve gemiddelde)")              
#          )
      )
  
}

