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
#' @importFrom INBOtheme inbo_palette inbo_lichtgrijs
#' @export
boxAgeWeight <- function(data,
		type, jaartallen = NULL, regio = "", 
  sourceIndicator_leeftijd = c("both", "inbo"),
		sourceIndicator_geslacht = c("both","inbo"), 
  width = NULL, height = NULL) {
	
  sourceIndicator_leeftijd <- match.arg(sourceIndicator_leeftijd)
  sourceIndicator_geslacht <- match.arg(sourceIndicator_geslacht)
  
  
	wildNaam <- unique(data$wildsoort)
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Select data
	plotData <- data[data$afschotjaar %in% jaartallen, 
			c("ontweid_gewicht", "leeftijd_comp", "leeftijd_comp_bron", "leeftijd_maanden", "geslacht_comp",
					"provincie", "geslacht_comp_bron")]
	names(plotData) <- c("gewicht", "leeftijd", "leeftijd_comp_bron", "maanden", "geslacht", "provincie", "geslacht_comp_bron")
	
  
	# Percentage collected
	nRecords <- nrow(plotData)
	
	# Remove some categories
	# To prevent error with R CMD check
	leeftijd <- NULL
	gewicht <- NULL
	geslacht <- NULL
	plotData <- subset(plotData, leeftijd != "Onbekend" &
					!is.na(gewicht) & geslacht != "Onbekend" & !is.na(geslacht))
	plotData$geslacht <- factor(plotData$geslacht)
	
	if (nrow(plotData) == 0)
		stop("Geen data beschikbaar")
	
	# Define names and ordering of factor levels
	if (wildNaam == "Wild zwijn" & sourceIndicator_leeftijd == "inbo") {  # wild zwijn
		
		plotData$leeftijd[plotData$leeftijd == "Frisling"] <- 
				ifelse(plotData$maanden[plotData$leeftijd == "Frisling"] < 6,
						"Frisling (<6m)", "Frisling (>6m)")
		
#    # Is "maanden" a reliable variable?
#    xtabs(~ leeftijd + maanden, data = plotData)
#    boxplot(plotData$maanden ~ plotData$leeftijd)
		
		newLevelsLeeftijd <- c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen")
		
	} else if (wildNaam == "Wild zwijn" & sourceIndicator_leeftijd == "both") {
		
   newLevelsLeeftijd <- c("Frisling", "Overloper", "Volwassen")
    
	}else {  # ree
		
		# Exclude records with weight lower than 5 or more than 30 (unrealistic)
		plotData <- subset(plotData, gewicht >= 5 & gewicht <= 30)
		newLevelsLeeftijd <- c("Kits", "Jongvolwassen", "Volwassen")
		
	}
	
	plotData$leeftijd <- factor(plotData$leeftijd, levels = newLevelsLeeftijd)
	
  # filters out NA leeftijd if all leeftijdlevels are passed in 'type'
	plotData <- subset(plotData, leeftijd %in% type)
  
 plotData <- filterGrofwild(plotData = plotData, 
   sourceIndicator_leeftijd = sourceIndicator_leeftijd, 
   sourceIndicator_geslacht = sourceIndicator_geslacht)
 
 
	# For optimal displaying in the plot
	colors <- inbo_palette(n = 2)
	names(colors) <- unique(plotData$geslacht)
	
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
	plotData$leeftijd <- factor(plotData$leeftijd, levels = type)
  
	# Create plot
	# Prevent Warning: 'layout' objects don't have these attributes: 'boxmode'
	pl <- plot_ly(data = plotData, x = ~leeftijd, y = ~gewicht, 
					color = ~geslacht, colors = colors, type = "box", 
					width = width, height = height) %>%
			layout(title = title,
					xaxis = list(title = "Categorie"), 
					yaxis = list(title = "Leeggewicht (kg)"),
					margin = list(t = 100),
					boxmode = "group",
					annotations = list(x = totalCounts$index, 
							y = -diff(range(plotData$gewicht, na.rm = TRUE))/10, 
							xref = "paper", text = totalCounts$freq, xanchor = 'center', 
							yanchor = 'bottom', showarrow = FALSE))  
	
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



#' Shiny module for creating the plot \code{\link{percentageboxAgeWeight}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
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
            showTime = TRUE, showType = TRUE, regionLevels = 1:2, 
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

