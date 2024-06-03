# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' Create interactive plot for distribution of weight in function of age
#' 
#' Figure p. 17 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearAge
#' @param type animal type, used to filter \code{data},
#' based on 'Leeftijdscategorie_onderkaak' column
#' @param sourceIndicator_leeftijd character, source used to filter \code{data} ('leeftijd_comp_bron' column)
#' should be one of \code{c("inbo", "both")}, where \code{"both"} refers to both inbo and meldingsformulier, 
#' i.e. no filtering. Defaults to \code{"both"}
#' @param sourceIndicator_geslacht character, source used to filter \code{data} ('geslacht_comp_bron' column)
#' should be one of \code{c("inbo", "both")}, where \code{"both"} refers to both inbo and meldingsformulier, 
#' i.e. no filtering. Defaults to \code{"both"}
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the distribution of weight
#' in function of age is plotted in box plots}
#' \item{'data': }{raw data used for the plot, as data.frame with:
#' \itemize{
#' \item{'gewicht': }{weight in kilograms}
#' \item{'leeftijd': }{age category}
#' \item{'maanden': }{age in months}
#' \item{'geslacht': }{gender}
#' }
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @export
boxAgeWeight <- function(data,
		type, jaartallen = NULL, regio = "", 
  sourceIndicator_leeftijd = c("both", "inbo"),
	sourceIndicator_geslacht = c("both", "inbo"), 
  width = NULL, height = NULL) {
  
  # For R CMD check
  gewicht <- NULL
  leeftijd <- NULL 
	
  sourceIndicator_leeftijd <- match.arg(sourceIndicator_leeftijd)
  sourceIndicator_geslacht <- match.arg(sourceIndicator_geslacht)
  
  
	wildNaam <- unique(data$wildsoort)
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
  	
  # Select data
	plotData <- data[data$afschotjaar %in% jaartallen, 
			c("ontweid_gewicht", "leeftijd_comp",  "leeftijd_comp_inbo", "geslacht_comp", "provincie",
        "leeftijd_comp_bron", "geslacht_comp_bron")]
	names(plotData) <- c("gewicht", "leeftijd_comp", "leeftijd_comp_inbo", "geslacht", "provincie", 
    "leeftijd_comp_bron", "geslacht_comp_bron")
  
  leeftijdVar <- if (sourceIndicator_leeftijd == "inbo") "leeftijd_comp_inbo" else "leeftijd_comp" 
  plotData <- plotData[plotData[[leeftijdVar]] %in% c(type, "Onbekend"), ]  # to calculate nRecords
  
  # Percentage collected
	nRecords <- nrow(plotData)
	
  plotData <- filterGrofwild(plotData = plotData, 
    sourceIndicator_leeftijd = sourceIndicator_leeftijd,
    sourceIndicator_geslacht = sourceIndicator_geslacht)
  plotData$leeftijd <- plotData$leeftijd_comp
  
	# Remove some categories
	plotData <- plotData[!is.na(plotData$gewicht) & plotData$geslacht != "Onbekend", ]
	
	if (nrow(plotData) == 0)
		stop("Geen data beschikbaar")
	
  if (wildNaam != "Wild zwijn") {		
		# Exclude records with weight lower than 5 or more than 30 (unrealistic)
		plotData <- subset(plotData, gewicht >= 5 & gewicht <= 30)
		
	}
	
  # filters out "Onbekend" leeftijd if all leeftijdlevels are passed in 'type'
	plotData <- subset(plotData, leeftijd %in% type)
  
	# For optimal displaying in the plot
	colors <- replicateColors(values = levels(droplevels(plotData$geslacht)))$colors
	
	totalCounts <- count(plotData, vars = c("leeftijd", "geslacht"))
	nIndices <- nrow(totalCounts)/2
	totalCounts$index[totalCounts$geslacht == "Mannelijk"] <- (-2/3 + seq_len(nIndices))/nIndices
	totalCounts$index[totalCounts$geslacht == "Vrouwelijk"] <- (-1/3 + seq_len(nIndices))/nIndices
	
	title <- paste0(wildNaam, " ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
					paste("in", jaartallen)),
			if (!all(regio == "")) 
				paste0(" (", toString(regio), ")"))
	
  # factors moet gelijk zijn aan de geselecteerde leeftijden (voor het correct labelen van de box plots)
	plotData$leeftijd <- droplevels(plotData$leeftijd)
  
	# Create plot
	# Prevent Warning: 'layout' objects don't have these attributes: 'boxmode'
	pl <- plot_ly(data = plotData, x = ~leeftijd, y = ~gewicht, 
					color = ~geslacht, colors = colors, type = "box", 
					width = width, height = height) %>%
        plotly::layout(title = title,
					xaxis = list(title = "Categorie"), 
					yaxis = list(title = "Leeggewicht (kg)"),
					margin = list(b = 120, t = 100),
					boxmode = "group",
					annotations = list(x = totalCounts$index, 
							y = -diff(range(plotData$gewicht, na.rm = TRUE))/10, 
							xref = "paper", text = totalCounts$freq, xanchor = 'center', 
							yanchor = 'bottom', showarrow = FALSE)) %>%
          add_annotations(
            text = percentCollected(nAvailable = nrow(plotData), nTotal = nRecords,
              text = "gekende leeftijd, geslacht en gewicht"),
            xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
            y = -0.3, yanchor = "bottom", showarrow = FALSE)
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(plot = pl, 
					data = subset(plotData, select = c("gewicht", "leeftijd", "geslacht", "provincie")))
	)
	
	
}




#' Shiny module for creating the plot \code{\link{boxAgeWeight}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams boxAgeWeight
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
boxAgeWeightServer <- function(id, data, type, timeRange) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      callModule(module = optionsModuleServer, id = "boxAgeWeight", 
        data = data,
        types = type,
        labelTypes = "Leeftijdscategorie",
        multipleTypes = TRUE,
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "boxAgeWeight",
        plotFunction = "boxAgeWeight", 
        data = data)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{boxAgeWeight}} - UI side
#' @inherit welcomeSectionUI
#' 
#' @export
boxAgeWeightUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkBoxAgeWeight"), 
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkBoxAgeWeight % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("boxAgeWeight"), 
            showTime = TRUE, showType = TRUE, regionLevels = c(1:2, 4), 
            exportData = TRUE, showDataSource = c("leeftijd", "geslacht")),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, 
          plotModuleUI(id = ns("boxAgeWeight"))
        ),
        tags$hr()
      )
    )
  )

}

