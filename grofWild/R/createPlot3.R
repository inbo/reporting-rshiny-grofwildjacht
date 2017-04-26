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