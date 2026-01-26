#' Create interactive plot for counts per genetic wolves per category and year
#' 
#' @inheritParams countYearProvince
#' @inheritParams reportingGrofwild-common-args
#' @return list with:
#' \itemize{
#' \item 'plot':  plotly object, for a given species the observed number 
#' per year and per age category is plotted in a stacked bar chart 
#' \item 'data' data displayed in the plot, as data.frame with:
#' \itemize{
#' \item 'jaar': year at which the animals was counted 
#' \item count, depending if \code{summarizeBy} is:  
#' \itemize{
#' \item 'count':  counts of animals in the 'freq' column 
#' \item 'percent':  percentage of counts of animals in the 'percent'  column 
#' }
#' \item 'totaal':  total number of animals across categories 
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @export
countGeneticWolves <- function(data, jaartallen = NULL,
		summarizeBy = c("count", "percent"), groupVariable = "Status",
		width = NULL, height = NULL) {
	
	summarizeBy <- match.arg(summarizeBy)
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$year)
	
	# Select data
	plotData <- data[data$year %in% jaartallen, 
			c("year", groupVariable)]
  names(plotData) <- c("year", "group")
	
	# Remove some categories
	plotData <- plotData[plotData$group != "AANVULLEN", ]
	
  
	# Summarize data per year and age category
	summaryData <- count(df = plotData, vars = names(plotData))
	
	# Summarize data per year
	totalCount <- count(df = plotData, vars = "year")
	totalCount$totaal <- totalCount$freq
	totalCount$freq <- NULL
	
	summaryData <- merge(summaryData, totalCount)
	
	
	# For optimal displaying in the plot
	summaryData$group <- as.factor(summaryData$group)
	summaryData$percent <- round(summaryData$freq/summaryData$totaal * 100)
  
	if (summarizeBy == "count") {
		
		summaryData$text <- paste0("<b>", summaryData$group, " in ", summaryData$year, "</b>",
				"<br>Aantal: ", summaryData$freq, " (", summaryData$percent, "%)", 
				"<br>Totaal: ", summaryData$totaal)
      
      title <- paste0("Aantal genetisch ge\u00EFdentificeerde wolven ",
        ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
          paste("in", jaartallen)), "\n in Vlaanderen"
      )
		
	} else {
		
		summaryData$text <- paste0("<b>", summaryData$group, " in ", summaryData$year, "</b>",
				"<br>", round(summaryData$percent), "%")
      
      title <- paste0("Percentage genetisch ge\u00EFdentificeerde wolven ",
        ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
          paste("in", jaartallen)), "\n in Vlaanderen"
      )
		
	}
	
	colors <- replicateColors(values = levels(summaryData$group))$colors
	
  singleYear <- length(unique(summaryData$year)) == 1
	
	
	# Create plot
	toPlot <- switch(summarizeBy,
			count = plot_ly(data = summaryData, x = ~year, 
							y = ~freq, color = ~group, text = ~text,
              textposition = "none", hoverinfo = "text+name",
							colors = colors, type = "bar",
							width = width, height = height) %>%
            plotly::layout(title = title,
							xaxis = list(
                title = "Jaar", 
                tickvals = unique(summaryData$year), 
                ticktext = unique(summaryData$year)), 
							yaxis = list(title = "Aantal wolven"),
							barmode = if (singleYear) "group" else "stack",
							margin = list(b = 120, t = 100)),
			percent = plot_ly(data = summaryData, x = ~year, 
							y = ~percent, color = ~group, text = ~text,
              textposition = "none", hoverinfo = "text+name",
							colors = colors, type = "bar",
							width = width, height = height) %>%
            plotly::layout(title = title,
							xaxis = list(title = "Jaar", 
                tickvals = unique(summaryData$year), 
                ticktext = unique(summaryData$year)), 
							yaxis = list(title = "Percentage wolven", range = c(0, 100)),
              barmode = if (singleYear) "group" else "stack",
							margin = list(b = 120, t = 100))
	)
	
	
	
	colsFinal <- colnames(summaryData)[
			!colnames(summaryData) %in% c("text", 
					if(summarizeBy == "count")	"percent"	else	c("freq", "totaal")
			)
	]
	
	# To prevent warnings in UI
	toPlot$elementId <- NULL
	
	
	return(list(plot = toPlot, data = summaryData[, colsFinal]))
	
}



#' Shiny module for creating the plot \code{\link{countGeneticWolves}} - server side
#' @inheritParams countAgeGenderServer 
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @export
countGeneticWolvesServer <- function(id, data, timeRange = reactive(NULL), preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
     
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "countGeneticWolves", 
        data = data,
        timeRange = timeRange
      )
      toReturn <- callModule(module = plotModuleServer, id = "countGeneticWolves",
        plotFunction = "countGeneticWolves", 
        data = data,
        filterDataOnRegion = FALSE,
        preSelected = preSelected)
      
      return(reactive(toReturn()))
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countGeneticWolves}} - UI side
#' @inherit welcomeSectionUI
#' @param plotFunction character, for matching file with plot titles
#' @inheritParams reportingGrofwild-common-args
#' @export
countGeneticWolvesUI <- function(id, uiText, plotFunction = "countGeneticWolvesUI", 
  context = "description", showTime = FALSE,
  doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(
    
    actionLink(inputId = ns("linkCountGeneticWolves"), label = h3(HTML(title))),
    conditionalPanel(paste("input.linkCountGeneticWolves % 2 ==", as.numeric(doHide)), ns = ns,
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("countGeneticWolves"))
        ),
        column(4,
          optionsModuleUI(id = ns("countGeneticWolves"), 
#            summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
            showTime = showTime, exportData = TRUE)
        )
      ),
      tags$br(),
      tags$div(class = "larger-description", HTML(description)),
      tags$hr()
    )
  )
  
}