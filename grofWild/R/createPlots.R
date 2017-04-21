#' Create interactive version of plot at page 4
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return plotly object
#' @import plotly
#' @importFrom stats reshape
#' @export
createPlot1 <- function(width = NULL, height = NULL) {
  
  voeren <- c(1, 7, 12, 26, 52, 28, 29, 34, 68)
  limburg <- c(NA, 20, 28, 68, 91, 190, 400, 444, 
      410)
  antwerpen <- c(NA, NA, NA, 3, NA, NA, 8, 15, 38)
  vlaamsBrabant <- c(NA, 1, 2, 1, 1, 1, 4, 6, 0)
  oostVlanderen <- c(NA, 1, 1, 2, 1, 2, 0, 2, 0)
  westVlanderen <- c(20, 15, 7, 5, 8, 41, 78, 7)
  
  data <- rbind.data.frame(voeren, limburg, antwerpen, 
      vlaamsBrabant, oostVlanderen, westVlanderen)
  colnames(data) <- 2006:2014
  rownames(data) <- c("Voeren", "Limburg", "Antwerpen", 
      "Vlaams-Brabant", "Oost-Vlaanderen", "West-Vlaanderen")
  
  # plotly
  plotData <- reshape(data = data, varying = list(as.character(colnames(data))), 
      v.names = "aantal", ids = rownames(data), direction = "long")
  plotData$time <- as.character(colnames(data))[plotData$time]
  rownames(plotData) <- NULL
  plot_ly(x = plotData$time, y = plotData$aantal, color = plotData$id,
          type = "scatter", mode = "lines+markers",
		  width = width, height = height) %>% 
      layout(xaxis = list(title = "Jaar"), 
          yaxis = list(title = "Aantal everzwijnen (afschot en valwild)"),
          margin = list(b = 120))  
  
}


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

#' Create interactive version of plot at page 13
#' @param year integer, year that you want to compare to all remaining years
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return plotly object
#' @import plotly
#' @importFrom stats median runif
#' @export
createPlot3 <- function(year = 2014, width = NULL, height = NULL) {
  
  # data
  
# min, median, max, new
  vec <- c(2, 5, 16, 8, 
      4, 10, 15, 5, 
      6, 12, 14, 7, 
      7, 9, 12, 6, 
      7.5, 8.5, 13, 5.5, 
      7.6, 7.9, 17, 11.5, 
      6.7, 8, 17, 13, 
      3, 6.5, 12, 6, 
      3.5, 5, 6, 4.5, 
      3.8, 12.5, 17.5, 7.7, 
      3, 6.2, 9, 10.7, 
      3, 5, 7.5, 14.5)
  data <- matrix(vec, ncol = 4, byrow = TRUE)
  colnames(data) <- c("min", "median", "max", "new")
  dataNew <- data.frame(data)
  
#  makeData <- function(data) {
#    set.seed(1)
#    dataNew <- matrix(0, ncol = 10, nrow = 12)
#    colnames(dataNew) <- 2005:2014
#    for(i in 1:10) {
#      for(j in 1:12) {
#        
#        dataNew[j, i] <- runif(1, min = data$min[j], max = data$max[j])
#        
#      }
#    }
#    return(dataNew)
#  }
#  
#  dataNew <- makeData(data)
  allMonths <- c("Januari", "Februari", 
      "Maart", "April", "Mei", "Juni", "Juli", "Augustus", 
      "September", "Oktober", "November", "December")
  dataNew$month <- factor(allMonths, levels = allMonths)
  dataNew$mean <- mean(dataNew$new)
  
#  otherYears <- dataNew[, colnames(dataNew) != as.character(year)]
#  plotData <- data.frame(
#      month = factor(rownames(dataNew), levels = rownames(dataNew)), 
#      minimum = apply(otherYears, 1, min),
#      maximum = apply(otherYears, 1, max),
#      mediaan = apply(otherYears, 1, median),
#      huidig = dataNew[, colnames(dataNew) == as.character(year)]
#      )
  
  plot_ly(dataNew, x = ~month, y = ~min, type = 'scatter', mode = 'lines',
          line = list(color = 'transparent'), showlegend = FALSE, name = "Min-Max",
		  width = width, height = height) %>%
      add_trace(y = ~max, type = 'scatter', mode = 'lines',
          fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', 
          line = list(color = 'transparent'), showlegend = TRUE, 
          name = "Min-Max (2009-2013)") %>%
      add_trace(x = ~month, y = ~median, type = 'scatter', mode = 'markers+lines',
          line = list(color='rgb(0,100,80)'), 
          name = "Mediaan (2009-2013)", showlegend = TRUE) %>%
      add_trace(x = ~month, y = ~new, type = 'scatter', mode = 'markers+lines',
          name = paste0("Huidig geobserveerd (", as.character(year), ")"), 
          line = list(color='rgb(100,0,0)'), showlegend = TRUE) %>%
      add_trace(x = ~month, y = ~mean, type = 'scatter', mode = 'lines',
          name = paste0("Gemiddelde (", as.character(year), ")"), 
          line = list(color="gray", dash = "dot"), showlegend = TRUE) %>%
      layout(xaxis = list(title = "Maand"), 
          yaxis = list(title = "Percentage jaarlijks afschot"),
          margin = list(b = 150))
    
}