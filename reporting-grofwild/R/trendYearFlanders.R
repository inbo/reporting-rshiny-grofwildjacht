

#' Create interactive plot for counts in Flanders per selected years
#' @param data data.frame, formatted data for plotting as returned by \code{\link{createTrendData}}
#' @inheritParams trendYearRegion
#' @return list with plot and data
#' 
#' @author mvarewyck
#' @import plotly
#' @export
trendYearFlanders <- function(data, timeRange, 
  unit = c("absolute", "relative", "relativeDekking"), isSchade = FALSE, 
		width = NULL, height = NULL) {
	
	
	trendYearRegion(data = data, timeRange = timeRange,
    unit = unit, isSchade = isSchade, isFlanders = TRUE,
    width = width, height = height)
  
  
}
