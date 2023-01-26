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
	plotData <- plotData[!is.na(plotData$kaak), ]
	
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
#	summaryData$jaar <- as.factor(summaryData$jaar)
	
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
	
  singleYear <- length(unique(summaryData$jaar)) == 1
	
	
	# Create plot
	toPlot <- switch(summarizeBy,
			count = plot_ly(data = summaryData, x = ~jaar, 
							y = ~freq, color = ~kaak, text = ~text, hoverinfo = "text+name",
							colors = colors, type = "bar",
							width = width, height = height) %>%
					layout(title = title,
							xaxis = list(
                title = "Jaar", 
                tickvals = unique(summaryData$jaar), 
                ticktext = unique(summaryData$jaar)), 
							yaxis = list(title = "Aantal"),
							barmode = if (singleYear) "group" else "stack",
							margin = list(b = 120, t = 100),
							annotations = list(x = totalCount$jaar, 
									y = if (singleYear) max(summaryData$freq) else totalCount$totaal, 
									text = paste(if (singleYear) "totaal:" else "", 
											totalCount$totaal),
									xanchor = 'center', yanchor = 'bottom',
									showarrow = FALSE)),
			percent = plot_ly(data = summaryData, x = ~jaar, 
							y = ~percent, color = ~kaak, text = ~text, hoverinfo = "text+name",
							colors = colors, type = "scatter", mode = "lines+markers",
							width = width, height = height) %>%
					layout(title = title,
							xaxis = list(title = "Jaar", 
                tickvals = unique(summaryData$jaar), 
                ticktext = unique(summaryData$jaar)), 
							yaxis = list(title = "Percentage", range = c(0, 100)),
							margin = list(b = 120, t = 100)) %>% 
					add_annotations(text = percentCollected(nAvailable = nCollected, nTotal = nRecords,
              text = "ingezamelde onderkaken van totaal"),
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
#' @param title reactive character, title with asterisk to show in the \code{actionLink}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countYearAgeServer <- function(id, data, timeRange, title = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observe({
          
          req(title())
          updateActionLink(session = session, inputId = "linkYearAge",
            label = paste("FIGUUR:", title()))
          
        })
      
      output$disclaimerYearAge <- renderUI({
          
          req(title())
          
          if (grepl("\\*", title()))
            getDisclaimerLimited()
          
        })
      
      
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "yearAge", 
        data = data,
        timeRange = timeRange
      )
      toReturn <- callModule(module = plotModuleServer, id = "yearAge",
        plotFunction = "countYearAge", 
        data = data)
      
      return(reactive(toReturn()))
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countYearAge}} - UI side
#' @template moduleUI
#' @param showRegion boolean, whether to show the region filter; default is TRUE
#' @param plotFunction character, for matching file with plot titles
#' @param doHide boolean, whether to initially hide the plot; default TRUE
#' 
#' @author mvarewyck
#' @export
countYearAgeUI <- function(id, uiText, plotFunction = "countYearAgeUI",
  showRegion = TRUE, doHide = TRUE) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == plotFunction, ]
  
  tagList(
    
    actionLink(inputId = ns("linkYearAge"), label = h3(HTML(uiText$title)),
      class = "action-h3"),
    conditionalPanel(paste("input.linkYearAge % 2 ==", as.numeric(doHide)), ns = ns,
      
      uiOutput(ns("disclaimerYearAge")),
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("yearAge"), 
            summarizeBy = c("Aantal (alle data)" = "count",
              "Percentage (enkel ingezamelde onderkaken)" = "percent"),
            showTime = TRUE, regionLevels = if (showRegion) 1:2, exportData = TRUE),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, 
          plotModuleUI(id = ns("yearAge"))
        )
      ),
      tags$hr()
    )
  )
  
}