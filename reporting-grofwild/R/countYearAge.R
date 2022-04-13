#' Create interactive plot for counts per age category and year
#' 
#' Figure p. 11 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearProvince
#' @param regio character vector, names of the selected regions in \code{data}
#' to be shown in the plot title
#' @param summarizeBy character, whether to summarize data in terms of counts or percentages
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the observed number 
#' per year and per age category is plotted in a stacked bar chart}
#' \item{'data'}{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'jaar': }{year at which the animals was counted}
#' \item{count, depending if \code{summarizeBy} is: }{
#' \itemize{
#' \item{'count': }{counts of animals in the 'freq' column}
#' \item{'percent': }{percentage of counts of animals in the 'percent'  column}
#' }
#' }
#' \item{'totaal': }{total number of animals across categories}
#' }
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @importFrom INBOtheme inbo_palette inbo_lichtgrijs
#' @export
countYearAge <- function(data, jaartallen = NULL, regio = "",
		summarizeBy = c("count", "percent"),
		width = NULL, height = NULL) {
	
	
	wildNaam <- unique(data$wildsoort)
	
	summarizeBy <- match.arg(summarizeBy)
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Select data
	plotData <- data[data$afschotjaar %in% jaartallen, 
			c("afschotjaar", "Leeftijdscategorie_onderkaak", "geslacht_comp")]
	names(plotData) <- c("jaar", "kaak", "geslacht")
	
	# Percentage collected
	nRecords <- nrow(plotData)
	
	# Remove some categories
	plotData <- plotData[!is.na(plotData$jaar) & !is.na(plotData$kaak), ]
	
  newLevelsKaak <- c(loadMetaEco(species = wildNaam)$leeftijd_comp, "Niet ingezameld")
  
  # Define names and ordering of factor levels
  if (wildNaam == "Ree") {  
    
    # Exclude categories 'jongvolwassen' and 'volwassen' for all animals labelled 'mannelijk' 
		# see github issue no. 31
		geslacht <- NULL  # to prevent warnings with R CMD check
		plotData <- subset(plotData, 
				subset = !(geslacht %in% "Mannelijk" & kaak %in% c("Jongvolwassen", "Volwassen")))
		
	}
	
  plotData$geslacht <- NULL
	
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
	nCollected <- sum(subData$freq)
	percentCollected <- nCollected/nRecords
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
	
	if (summarizeBy == "count") {
		
		summaryData$text <- paste0("<b>", summaryData$kaak, " in ", summaryData$jaar, "</b>",
				"<br>Aantal: ", summaryData$freq, 
				"<br>Totaal: ", summaryData$totaal)
		
	} else {
		
		summaryData$text <- paste0("<b>", summaryData$kaak, " in ", summaryData$jaar, "</b>",
				"<br><i>Subset ingezamelde onderkaken </i>",
				"<br>", round(summaryData$percent), "%")
		
	}
	
	
	
	colors <- c(inbo_palette(3), inbo_lichtgrijs)
	names(colors) <- newLevelsKaak
	
	title <- paste0(wildNaam, " ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
					paste("in", jaartallen)),
			if (!all(regio == ""))
				  paste0("\n(in ", paste(regio, collapse = " en "), ")")
	  )
	
	
	
	# Create plot
	toPlot <- switch(summarizeBy,
			count = plot_ly(data = summaryData, x = ~jaar, 
							y = ~freq, color = ~kaak, text = ~text, hoverinfo = "text+name",
							colors = colors, type = "bar",
							width = width, height = height) %>%
					layout(title = title,
							xaxis = list(title = "Jaar"), 
							yaxis = list(title = "Aantal"),
							barmode = if (nlevels(summaryData$jaar) == 1) "group" else "stack",
							margin = list(b = 120, t = 100),
							annotations = list(x = totalCount$jaar, 
									y = totalCount$totaal, 
									text = paste(if (length(unique(totalCount$jaar)) == 1) "totaal:" else "", 
											totalCount$totaal),
									xanchor = 'center', yanchor = 'bottom',
									showarrow = FALSE)),
			percent = plot_ly(data = summaryData, x = ~jaar, 
							y = ~percent, color = ~kaak, text = ~text, hoverinfo = "text+name",
							colors = colors, type = "scatter", mode = "lines+markers",
							width = width, height = height) %>%
					layout(title = title,
							xaxis = list(title = "Jaar"), 
							yaxis = list(title = "Percentage", range = c(0, 100)),
							margin = list(b = 120, t = 100)) %>% 
					add_annotations(text = paste0(round(percentCollected, 2)*100, 
									"% ingezamelde onderkaken van totaal (", nCollected, "/", nRecords, ")"),
							xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
							y = -0.3, yanchor = "bottom", showarrow = FALSE)
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



#' Shiny module for creating the plot \code{\link{countYearAge}} - server side
#' @inheritParams countAgeGenderServer 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countYearAgeServer <- function(id, data, timeRange) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "yearAge", 
        data = data,
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "yearAge",
        plotFunction = "countYearAge", 
        data = data)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countYearAge}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
countYearAgeUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkYearAge"), 
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkYearAge % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("yearAge"), 
            summarizeBy = c("Aantal (alle data)" = "count",
              "Percentage (enkel ingezamelde onderkaken)" = "percent"),
            showTime = TRUE, regionLevels = 1:2, exportData = TRUE),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, 
          plotModuleUI(id = ns("yearAge"))
        ),
        tags$hr()
      )
    )
  )
  
}