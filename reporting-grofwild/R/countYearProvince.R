# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Create interactive plot for counts per province and year
#' 
#' Figure p. 4 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param data data.frame with raw data for plotting
#' @param jaartallen integer vector, defines the year(s) that should be considered
#' in the plot; if NULL no selection on year(s) is made
#' @param type character, regional level of interest should be one of 
#' \code{c("provinces", "flanders", "faunabeheerzones")}
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given specie the observed number 
#' per year and per province is plotted in a stacked bar chart}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animals was shot}
#' \item{'locatie': }{location name, could be province, flanders or fbz name}
#' \item{'value': }{counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom reshape2 melt
#' @importFrom INBOtheme inbo_palette
#' @importFrom stringr str_sort
#' @export
countYearProvince <- function(data, jaartallen = NULL, 
        type = c("provinces", "flanders", "faunabeheerzones"),
		width = NULL, height = NULL) {
	
	
  type <- match.arg(type)
	wildNaam <- paste(unique(data$wildsoort), collapse = ", ")
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
  plotData <- data
  plotData$locatie <- switch(type,
          flanders = "Vlaams Gewest",
          provinces = plotData$provincie,
          faunabeheerzones = plotData$FaunabeheerZone
  )
    
	# Select data
	plotData <- plotData[plotData$afschotjaar %in% jaartallen, c("afschotjaar", "locatie")]
	plotData <- plotData[!is.na(plotData$afschotjaar), ]
  
  # Rename provincie NA to "Onbekend" -  provincie is already factor
  if (type == "provinces") {
  	  plotData$locatie <- factor(plotData$locatie, levels = levels(addNA(plotData$locatie)), 
        labels = c(levels(plotData$locatie), "Onbekend"), exclude = NULL)
  
  } else {
  	
    plotData$locatie[is.na(plotData$locatie)] <- "Onbekend"
  }

	# Exclude unused provinces/fbz's
  plotData$locatie <- as.factor(plotData$locatie)    
	plotData$locatie <- droplevels(plotData$locatie)
  
  # sort numerically again for fbz's (numeric and string combination is not well ordered by default)
  if (type == "faunabeheerzones") {
    plotData$locatie <- factor(plotData$locatie, levels = stringr::str_sort(levels(plotData$locatie), numeric = TRUE))
#    levels(plotData$locatie) <- stringr::str_sort(levels(plotData$locatie), numeric = TRUE)
  }
	
	# Summarize data per province and year
	plotData$afschotjaar <- with(plotData, factor(afschotjaar, levels = 
							min(jaartallen):max(jaartallen)))
	
	summaryData <- melt(table(plotData), id.vars = "afschotjaar")
	
	# Summarize data per year
	totalCount <- table(plotData$afschotjaar)
	
	
	# For optimal displaying in the plot
  summaryData$locatie <- as.factor(summaryData$locatie)
	summaryData$locatie <- factor(summaryData$locatie, levels = rev(levels(summaryData$locatie)))
	summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
	
	colors <- rev(inbo_palette(n = nlevels(summaryData$locatie)))
	title <- paste0(wildNaam, " ",
			ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
					jaartallen)
	)
	
	
	# Create plot
	pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~value, color = ~locatie,
					colors = colors, type = "bar",  width = width, height = height) %>%
			layout(title = title,
					xaxis = list(title = "Jaar"), 
					yaxis = list(title = "Aantal"),
					margin = list(b = 80, t = 100), 
					barmode = ifelse(nlevels(summaryData$afschotjaar) == 1, "group", "stack"),
					annotations = list(x = levels(summaryData$afschotjaar), 
							y = totalCount, 
							text = paste(ifelse(nlevels(summaryData$afschotjaar) == 1, "totaal:", ""), totalCount),
							xanchor = 'center', yanchor = 'bottom',
							showarrow = FALSE),
          showlegend = TRUE)  
	
	# To prevent warnings in UI
	pl$elementId <- NULL
  
  # Change variable name
  names(summaryData)[names(summaryData) == "value"] <- "aantal"
  
  
	return(list(plot = pl, data = summaryData))
	
}
