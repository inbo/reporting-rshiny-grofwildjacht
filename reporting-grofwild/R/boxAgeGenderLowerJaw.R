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
#' @importFrom INBOtheme inbo_lichtgrijs
#' @importFrom plyr count
#' @export
boxAgeGenderLowerJaw <- function(data, 
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
	plotData <- data[
			# data of specified years
			data$afschotjaar %in% jaartallen, 
			c("onderkaaklengte_comp", "leeftijd_comp", "leeftijd_comp_bron", "leeftijd_maanden", 
     "geslacht_comp", "provincie", "geslacht_comp_bron")]
	names(plotData) <- c("onderkaaklengte", "leeftijd", "leeftijd_comp_bron", "maanden",
   "geslacht", "provincie", "geslacht_comp_bron")
	
	
	# Percentage collected
	nRecords <- nrow(plotData)
	
	# Remove some categories
    # To prevent error with R CMD check
	leeftijd <- NULL
	onderkaaklengte <- NULL
	geslacht <- NULL
	plotData <- subset(plotData, leeftijd != "Onbekend" &
					!is.na(onderkaaklengte) & geslacht != "Onbekend" & !is.na(geslacht))
	plotData$geslacht <- factor(plotData$geslacht)
	
	if (nrow(plotData) == 0)
		stop("Geen data beschikbaar")
	

# Define names and ordering of factor levels
  # NOTE: Redundant for wild zwijn (not shown in app)
if (wildNaam == "Wild zwijn" & sourceIndicator_leeftijd == "inbo") {  # wild zwijn
  
  plotData$leeftijd[plotData$leeftijd == "Frisling"] <- 
    ifelse(plotData$maanden[plotData$leeftijd == "Frisling"] < 6,
      "Frisling (<6m)", "Frisling (>6m)")
  
  newLevelsLeeftijd <- c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen")
  
} else if (wildNaam == "Wild zwijn" & sourceIndicator_leeftijd == "both") {
  
  newLevelsLeeftijd <- c("Frisling", "Overloper", "Volwassen")
  
}else {  # ree
  
  # Exclude records with weight lower than 5 or more than 30 (unrealistic)
  newLevelsLeeftijd <- c("Kits", "Jongvolwassen", "Volwassen")
  
}
	
	plotData$leeftijd <- factor(plotData$leeftijd, levels = newLevelsLeeftijd)
	
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
	
	title <- paste0(wildNaam, " onderkaak lengte ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), 
					paste("in", jaartallen)), 
			if (!all(regio == "")) 
				paste0(" (", toString(regio), ")")) 
			
  # factors moet gelijk zijn aan de geselecteerde leeftijden (voor het correct labelen van de box plots)
  plotData$leeftijd <- factor(plotData$leeftijd, levels = type)
  
	# create plot
	pl <- plot_ly(data = plotData, x = ~leeftijd, y = ~onderkaaklengte,
					color = ~geslacht, colors = colors, type = "box", 
					width = width, height = height) %>%
			layout(title = title,
					xaxis = list(title = "Categorie"), 
					yaxis = list(title = "Onderkaaklengte (mm)"),
					margin = list(t = 100),
					boxmode = "group",
					annotations = list(x = totalCounts$index, 
							y = -diff(range(plotData$onderkaaklengte, na.rm = TRUE))/10, 
							xref = "paper", text = totalCounts$freq, xanchor = 'center', 
							yanchor = 'bottom', showarrow = FALSE)
			)
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(
					plot = pl, 
					data = subset(plotData, select = c("onderkaaklengte", "leeftijd", "geslacht", "provincie"))
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
#' @inheritParams ageGenderLowerJawServer 
#' @param regionLevels character, choices for region
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
ageGenderLowerJawUI <- function(id, regionLevels) {
  
  ns <- NS(id)
  
  tagList(
    actionLink(inputId = ns("linkAgeGenderLowerJaw"),
      label = h3("FIGUUR: Onderkaaklengte per leeftijdscategorie en geslacht (INBO of Meldingsformulier)")),
    conditionalPanel("input.linkAgeGenderLowerJaw % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("ageGenderLowerJaw"), showTime = TRUE, showType = TRUE,
            regionLevels = regionLevels, exportData = TRUE,
            showDataSource = c("leeftijd", "geslacht")),
          tags$p("Verdeling van de onderkaaklengte per leeftijdscategorie en per geslacht voor alle gegevens uit de geselecteerde periode en regio('s).
              Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald.")),
        column(8, 
          plotModuleUI(id = ns("ageGenderLowerJaw"), filter = TRUE)
        )
      ),
      tags$hr()
    )
  )
  
}





