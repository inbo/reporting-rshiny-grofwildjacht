# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' Create interactive plot for comparing age per gender
#' 
#' Figure p. 15 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countAgeCheek
#' @inheritParams boxAgeWeight
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the percentage per age category
#' and per gender, based on meldingsformulier data in a stacked bar chart}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'geslacht': }{gender}
#' \item{'leeftijd': }{age category}
#' \item{'freq': }{counts of animals}
#' \item{'percent': }{percentage of counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @author mvarewyck
#' @export
countAgeGender <- function(data, jaartallen = NULL, 
  sourceIndicator_leeftijd = c("both", "inbo"),
  sourceIndicator_geslacht = c("both", "inbo"), 
  width = NULL, height = NULL) {
	
	
  sourceIndicator_leeftijd <- match.arg(sourceIndicator_leeftijd)
  sourceIndicator_geslacht <- match.arg(sourceIndicator_geslacht)
  
	wildNaam <- unique(data$wildsoort)
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Select data
	plotData <- data[data$afschotjaar %in% jaartallen, 
				c("leeftijd_comp", "leeftijd_comp_inbo", "geslacht_comp",
          "leeftijd_comp_bron", "geslacht_comp_bron")]
	names(plotData) <- c("leeftijd_comp", "leeftijd_comp_inbo", "geslacht", 
    "leeftijd_comp_bron", "geslacht_comp_bron")
	
	# For percentage collected
	nRecords <- nrow(plotData)
  
  plotData <- filterGrofwild(plotData = plotData, 
    sourceIndicator_leeftijd = sourceIndicator_leeftijd,
    sourceIndicator_geslacht = sourceIndicator_geslacht)
  plotData$leeftijd <- plotData$leeftijd_comp
	
	# Remove some categories
	# To prevent error with R CMD check
	leeftijd <- NULL
	geslacht <- NULL
	plotData <- subset(plotData, geslacht != "Onbekend" & leeftijd != "Onbekend",
    c("geslacht", "leeftijd"))
  
	# Summarize data per province and year
	summaryData <- count(df = plotData, vars = names(plotData))
	freq <- NULL  # to prevent warnings with R CMD check 
	summaryData <- ddply(summaryData, "leeftijd", transform, 
			percent = freq / sum(freq) * 100)
	
	
	
	# For optimal displaying in the plot
  summaryData$leeftijd <- factor(summaryData$leeftijd, 
    levels = if (sourceIndicator_leeftijd == "inbo") 
        loadMetaEco(species = wildNaam)$leeftijd_comp_inbo else 
        loadMetaEco(species = wildNaam)$leeftijd_comp)
  
  missingLeeftijd <- !levels(summaryData$leeftijd) %in% summaryData$leeftijd
  if (any(missingLeeftijd))
    summaryData <- rbind(summaryData,
      data.frame(
        geslacht = NA,
        leeftijd = levels(summaryData$leeftijd)[missingLeeftijd], 
        freq = NA, percent = NA))
	
	summaryData$text <- paste0(round(summaryData$percent), "%",
			" (", summaryData$freq, ")")
	
	totalCount <- count(df = summaryData, vars = "leeftijd", wt_var = "freq")$freq
  totalCount[is.na(totalCount)] <- 0
	names(totalCount) <- levels(summaryData$leeftijd)
  
	colors <- replicateColors(values = unique(summaryData$geslacht))$colors
	title <- paste(wildNaam, paste0("(", 
					ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
							jaartallen), ")"))
	
	
	# Create plot
	pl <- plot_ly(data = summaryData, x = ~leeftijd, y = ~percent, color = ~geslacht,
					text = ~text, textposition = "none", hoverinfo = "x+text+name",
					colors = colors, type = "bar",  width = width, height = height) %>%
			
        plotly::layout(title = title,
					xaxis = list(title = "Leeftijdscategorie"), 
					yaxis = list(title = "Percentage"),
					legend = list(y = 0.8, yanchor = "top"),
					margin = list(b = 120, t = 100), 
					barmode = "stack",
					shapes = list(type = "line", line = list(color = inbo_lichtgrijs, dash = "dash"), 
							xref = "paper", x0 = 0, x1 = 1, y0 = 50, y1 = 50),
					annotations = list(x = names(totalCount), y = -10, 
							text = totalCount, xanchor = 'center', yanchor = 'bottom', 
							showarrow = FALSE)) %>%
			
			add_annotations(text = "Geslacht", 
					xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
					y = 0.8, yanchor = "bottom",    # Same y as legend below
					legendtitle = TRUE, showarrow = FALSE) %>%
        
        add_annotations(
          text = percentCollected(nAvailable = nrow(plotData), nTotal = nRecords,
            text = "gekende leeftijd en geslacht"),
          xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
          y = -0.3, yanchor = "bottom", showarrow = FALSE)  
      
	colsFinal <- c("geslacht", "leeftijd", "freq", "percent")
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(plot = pl, data = summaryData[, colsFinal]))
	
	
}



#' Shiny module for creating the plot \code{\link{countAgeGender}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @param timeRange numeric vector of length 2, min and max year to subset data
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countAgeGenderServer <- function(id, data, timeRange) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Geslachtsverdeling binnen het afschot per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "ageGender", 
        data = data, 
        types = reactive(NULL),   # no types selection needed
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "ageGender",
        plotFunction = "countAgeGender", 
        data = data)
      
    })
  
}


#' Shiny module for creating the plot \code{\link{countAgeGender}} - UI side
#' @inherit welcomeSectionUI
#' 
#' @export
countAgeGenderUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkAgeGender"),
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkAgeGender % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("ageGender"), showTime = TRUE,
            showDataSource = c("leeftijd", "geslacht"), exportData = TRUE),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, 
          plotModuleUI(id = ns("ageGender"))
        )
      ),
      tags$hr()
    )
  )
  
}
  
  
  

