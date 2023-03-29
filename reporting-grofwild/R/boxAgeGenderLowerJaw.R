#' Create interactive plot for comparing length of lower jaw between age/gender category
#' 
#' Figure p.17 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearAge
#' @inheritParams boxAgeWeight
#' @param type animal type, used to filter \code{data}, based on 'ageGender' column
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object with length lower jaw per 
#' age/gender category for specified \code{jaartallen}}
#' \item{'data': }{raw data used for the plot, as data.frame with:
#' \itemize{
#' \item{'ageGender': }{age/gender category}
#' \item{'onderkaaklengte': }{length of the lower jaw in mm}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @import plotly
#' @importFrom plyr count
#' @export
boxAgeGenderLowerJaw <- function(data, 
		type, jaartallen = NULL, regio = "",
  sourceIndicator_leeftijd = c("both", "inbo"),
  sourceIndicator_geslacht = c("both","inbo"), 
  width = NULL, height = NULL) {
  
  
  # To prevent error with R CMD check
  leeftijd <- NULL
  onderkaaklengte <- NULL
  geslacht <- NULL
  
  sourceIndicator_leeftijd <- match.arg(sourceIndicator_leeftijd)
  sourceIndicator_geslacht <- match.arg(sourceIndicator_geslacht)
	
	
	wildNaam <- unique(data$wildsoort)	
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
  
  # Select data
	plotData <- data[
			# data of specified years
			data$afschotjaar %in% jaartallen, 
			c("onderkaaklengte_comp", "leeftijd_comp", "geslacht_comp", "provincie",
        "leeftijd_comp_bron", "geslacht_comp_bron")]
	names(plotData) <- c("onderkaaklengte", "leeftijd", "geslacht", "provincie", 
    "leeftijd_comp_bron", "geslacht_comp_bron")
  plotData <- subset(plotData, leeftijd %in% c(type, "Onbekend"))  # to calculate nRecords
  
	# Percentage collected
	nRecords <- nrow(plotData)
	
  plotData <- filterGrofwild(plotData = plotData, 
    sourceIndicator_leeftijd = sourceIndicator_leeftijd,
    sourceIndicator_geslacht = sourceIndicator_geslacht)
  
	# Remove some categories
 	plotData <- subset(plotData, leeftijd %in% type & leeftijd != "Onbekend" &
					!is.na(onderkaaklengte) & geslacht != "Onbekend")
	
	if (nrow(plotData) == 0)
		stop("Geen data beschikbaar")
	
  # For optimal displaying in the plot
	colors <- replicateColors(values = unique(plotData$geslacht))$colors
	
	totalCounts <- count(plotData, vars = c("leeftijd", "geslacht"))
  # needed when some groups have NA
  totalCounts <- merge(expand.grid(geslacht = unique(totalCounts$geslacht), leeftijd = unique(totalCounts$leeftijd)), totalCounts, all.x = TRUE)
  nIndices <- nrow(totalCounts) / 2
  totalCounts$index[totalCounts$geslacht == "Vrouwelijk"] <- (-2/3 + seq_len(nIndices))/nIndices
  totalCounts$index[totalCounts$geslacht == "Mannelijk"] <- (-1/3 + seq_len(nIndices))/nIndices
	
	title <- paste0(wildNaam, " onderkaak lengte ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), 
					paste("in", jaartallen)), 
			if (!all(regio == "")) 
				paste0(" (", toString(regio), ")")) 
			
  # factors moet gelijk zijn aan de geselecteerde leeftijden (voor het correct labelen van de box plots)
  plotData$leeftijd <- droplevels(plotData$leeftijd)
  
	# create plot
	pl <- plot_ly(data = plotData, x = ~leeftijd, y = ~onderkaaklengte,
					color = ~geslacht, colors = colors, type = "box", 
					width = width, height = height) %>%
			plotly::layout(title = title,
					xaxis = list(title = "Categorie"), 
					yaxis = list(title = "Onderkaaklengte (mm)", range = c(0, max(plotData$onderkaaklengte))),
					margin = list(b = 120, t = 100),
					boxmode = "group",
          annotations = list(
            x = totalCounts$index, xref = "paper", xanchor = 'center',
            y = 0.05, yref = "paper", yanchor = "top",
            text = ifelse(is.na(totalCounts$freq), "", totalCounts$freq),  
            showarrow = FALSE)
			) %>%
      
      add_annotations(
        text = percentCollected(nAvailable = nrow(plotData), nTotal = nRecords,
          text = "gekende leeftijd, geslacht en onderkaaklengte"),
        xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
        y = -0.3, yanchor = "bottom", showarrow = FALSE)
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(
					plot = pl, 
					data = plotData
	))
	
}



#' Shiny module for creating the plot \code{\link{boxAgeGenderLowerJaw}} - server side
#' @inheritParams countAgeGenderServer
#' @inheritParams optionsModuleServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
ageGenderLowerJawServer <- function(id, data, types, timeRange) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Onderkaaklengte per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
      callModule(module = optionsModuleServer, id = "ageGenderLowerJaw", 
        data = data,
        types = types,
        labelTypes = "Leeftijdscategorie",
        multipleTypes = TRUE,
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "ageGenderLowerJaw",
        plotFunction = "boxAgeGenderLowerJaw", 
        data = data)
      
    })
  
}


#' Shiny module for creating the plot \code{\link{boxAgeGenderLowerJaw}} - UI side
#' @param regionLevels character, choices for region
#' @inherit welcomeSectionUI
#' 
#' @export
ageGenderLowerJawUI <- function(id, regionLevels, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    actionLink(inputId = ns("linkAgeGenderLowerJaw"),
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkAgeGenderLowerJaw % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("ageGenderLowerJaw"), showTime = TRUE, showType = TRUE,
            regionLevels = regionLevels, exportData = TRUE,
            showDataSource = c("leeftijd", "geslacht")),
          tags$p(HTML(uiText[, id]))),
        column(8, 
          plotModuleUI(id = ns("ageGenderLowerJaw"))
        )
      ),
      tags$hr()
    )
  )
  
}





