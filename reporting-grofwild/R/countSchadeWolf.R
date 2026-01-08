#' Create interactive plot for counts of schade by wolves per category and year
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
countSchadeWolves <- function(data, jaartallen = NULL,
		summarizeBy = c("count", "percent"), groupVariable = c("Schade", "wolfproof", "Prooidier"),
		width = NULL, height = NULL, sourceIndicator = NULL) {
	
	summarizeBy <- match.arg(summarizeBy)
  groupVariable <- match.arg(groupVariable)
  
  if (groupVariable == "Prooidier") {
    data <- data[data$Schade == "Wolf", ]
    ylabel <- "bevestiged schadegevallen"
  } else if (groupVariable == "wolfproof") {
    data <- data[data$Preventie != "", ]
    ylabel <- "gemelde schadegevallen"
  } else {
    ylabel <- "gemelde schadegevallen"
  }
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$Jaar)
  
	# Select data
	plotData <- data[!is.na(data$Jaar) & data$Jaar %in% jaartallen, 
			c("Jaar", groupVariable)]
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
		
	} else {
		
		summaryData$text <- paste0("<b>", summaryData$group, " in ", summaryData$year, "</b>",
				"<br>", round(summaryData$percent), "%")
		
	}
	
	colors <- replicateColors(values = levels(summaryData$group))$colors
	
  singleYear <- length(unique(summaryData$year)) == 1
	
  title <- switch(groupVariable,
    Schade = paste0("Identificatie gemelde schadegevallen ", 
      ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
        paste("in", jaartallen)), "\n in Vlaanderen"),
    wolfproof = paste0("Wolfwerende omheiningen per schadegeval ", 
      ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
        paste("in", jaartallen)), "\n in Vlaanderen"),
    Prooidier = paste0("Schadegevallen per veesoort ", 
      ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
        paste("in", jaartallen)), "\n in Vlaanderen"))
  
	
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
							yaxis = list(title = paste("Aantal", ylabel)),
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
							yaxis = list(title = paste("Percentage", ylabel), range = c(0, 100)),
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



#' Shiny module for creating the plot \code{\link{countSchadeWolves}} - server side
#' @inheritParams countAgeGenderServer 
#' @param title reactive character, title with asterisk to show in the \code{actionLink}
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @export
countSchadeWolvesServer <- function(id, data, groupVariable = NULL, timeRange = reactive(NULL), preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
     
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "countSchadeWolves", 
        data = data,
        timeRange = timeRange
      )
      toReturn <- callModule(module = plotModuleServer, id = "countSchadeWolves",
        plotFunction = "countSchadeWolves", 
        data = data,
        filterDataOnRegion = FALSE,
        groupVariable = groupVariable,
        preSelected = preSelected)
      
      return(reactive(toReturn()))
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countSchadeWolves}} - UI side
#' @inherit welcomeSectionUI
#' @param showRegion boolean, whether to show the region filter; default is TRUE
#' @param plotFunction character, for matching file with plot titles
#' @inheritParams reportingGrofwild-common-args
#' @export
countSchadeWolvesUI <- function(id, uiText, plotFunction = "countSchadeWolvesUI", 
  context = "description", showTime = FALSE,
  doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(
    
    actionLink(inputId = ns("linkCountSchadeWolves"), label = h3(HTML(title))),
    conditionalPanel(paste("input.linkCountSchadeWolves % 2 ==", as.numeric(doHide)), ns = ns,
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("countSchadeWolves"))
        ),
        column(4,
          optionsModuleUI(id = ns("countSchadeWolves"), 
            summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
            showTime = showTime, exportData = TRUE)
        )
      ),
      tags$br(),
      tags$div(class = "larger-description", HTML(description)),
      tags$hr()
    )
  )
  
}