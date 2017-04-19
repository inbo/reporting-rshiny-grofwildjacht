#' Create interactive version of plot at page 4
#' @return plotly object
#' @import plotly
#' @export
createPlot1 <- function() {
  
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
  plotData <- reshape(data = data, varying = list(as.character(2006:2014)), 
      v.names = "aantal", ids = rownames(data), direction = "long")
  plotData$time <- as.character(2006:2014)[plotData$time]
  rownames(plotData) <- NULL
  plot_ly(x = plotData$time, y = plotData$aantal, color = plotData$id,
          type = "scatter", mode = "lines+markers") %>% 
      layout(xaxis = list(title = "Jaar"), 
          yaxis = list(title = "Aantal everzwijnen (afschot en valwild)"))  
  
}


#' Create interactive version of plot at page 7
#' @return plotly object
#' @import plotly
#' @importFrom plyr ddply  
#' @export
createPlot2 <- function() {
  
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
  
  # Count number of duplicated rows
  plotData <- ddply(data, .(Year, Value, Province), nrow)
  plotData$Province <- as.factor(plotData$Province)
  plotSummary <- tapply(plotData$Value, plotData$Year, mean)
  plotData <- rbind(plotData, 
      data.frame(Year = names(plotSummary), Value = as.numeric(plotSummary),
          Province = "Gemiddelde", V1 = max(plotData$V1)+1))
  plotData$symbols <- ifelse(plotData$Province == "Gemiddelde", 19, 18)
  
  plot_ly(x = plotData$Year, y = plotData$Value, color = plotData$Province, 
          marker = list(size = plotData$V1*7), type = "scatter", 
          mode = "markers", symbol = plotData$symbols) %>%
      layout(xaxis = list(title = "Jaar"), 
          yaxis = list(title = "Aantal everzwijnen"),
          annotations = list(
              list(xref = "paper", yref = "paper", 
                  xanchor = "left", yanchor = "bottom",
                  x = 1.02, y = 0.6, showarrow = FALSE, bordercolor = "white",
                  font = list(family = "arial", size = 14), 
                  text = "Aantal overlappende \n punten bepaalt \n puntgrootte \n (behalve gemiddelde)")              
          ))
  
}

#' Create interactive version of plot at page 13
#' @param year integer, year that you want to compare to all remaining years
#' @return plotly object
#' @import plotly
#' @export
createPlot3 <- function(year = 2014) {
  
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
  data <- data.frame(data)
  
  makeData <- function(data) {
    set.seed(1)
    dataNew <- matrix(0, ncol = 10, nrow = 12)
    colnames(dataNew) <- 2005:2014
    for(i in 1:10) {
      for(j in 1:12) {
        
        dataNew[j, i] <- runif(1, min = data$min[j], max = data$max[j])
        
      }
    }
    return(dataNew)
  }
  
  dataNew <- makeData(data)
  rownames(dataNew) <- c("Januari", "Februari", 
      "Maart", "April", "Mei", "Juni", "Juli", "Augustus", 
      "September", "Oktober", "November", "December")
  
  otherYears <- dataNew[, colnames(dataNew) != as.character(year)]
  plotData <- data.frame(
      month = factor(rownames(dataNew), levels = rownames(dataNew)), 
      minimum = apply(otherYears, 1, min),
      maximum = apply(otherYears, 1, max),
      mediaan = apply(otherYears, 1, median),
      huidig = dataNew[, colnames(dataNew) == as.character(year)]
      )
  
  plot_ly(plotData, x = ~month, y = ~minimum, type = 'scatter', mode = 'lines',
          line = list(color = 'transparent'), showlegend = FALSE, name = "Min-Max") %>%
      add_trace(y = ~maximum, type = 'scatter', mode = 'lines',
          fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', 
          line = list(color = 'transparent'), showlegend = TRUE, name = "Min-Max") %>%
      add_trace(x = ~month, y = ~mediaan, type = 'scatter', mode = 'markers+lines',
          line = list(color='rgb(0,100,80)'), name = "Mediaan", showlegend = TRUE) %>%
      add_trace(x = ~month, y = ~huidig, type = 'scatter', mode = 'markers+lines',
          name = paste0("Huidig (", as.character(year), ")"), 
          line = list(color='rgb(100,0,0)'), showlegend = TRUE) %>%
      layout(xaxis = list(title = "Maand"), 
          yaxis = list(title = "Percentage jaarlijks afschot"))
    
}