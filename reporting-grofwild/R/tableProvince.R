# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Create summary table for specific species
#' @param data data.frame with raw data for summary table
#' @param assignedData data.frame with summary data on the number of assigned
#' animals that can be shot
#' @inheritParams percentageYearlyShotAnimals 
#' @param categorie character, defines the dependent variable in the table
#' besides "provinces" 
#' @param minForTrend numeric, the minimum number of records needed before
#' a trend is being reported
#' @return data.frame, number or percentage of observations 
#' per province and per \code{categorie}
#' @author mvarewyck
#' @importFrom reshape2 dcast
#' @importFrom plyr count join
#' @export
tableProvince <- function(data, assignedData, jaar = NULL, 
		categorie = c("leeftijd", "typeAantal", "typePercent"), minForTrend = 50) {
	
	
	wildNaam <- unique(data$wildsoort)  
	categorie <- match.arg(categorie)
	
	if (is.null(jaar))
		stop("Gelieve jaartal te selecteren")
	
	
	## General Modification of Data
	
	if (categorie == "leeftijd") {
		
		allData <- data[, c("provincie", "leeftijd_comp", "afschotjaar")]
		names(allData) <- c("provincie", "categorie", "jaar")
		
	} else if (categorie == "typeAantal") {
		
		allData <- data[, c("provincie", "labeltype", "afschotjaar")]
		names(allData) <- c("provincie", "categorie", "jaar")
		
	} else {
		
		allData <- data[, c("provincie", "labeltype", "afschotjaar")]
		names(allData) <- c("provincie", "categorie", "jaar")
    
		assignedData <- assignedData[, c("provincie_toek", "labeltype", "labeljaar", "toegekend")]
		names(assignedData) <- c("provincie", "categorie", "jaar", "totaal")
    # summarize per province
    assignedData <- aggregate(totaal ~ jaar + provincie + categorie, data = assignedData, sum)
		
	}
	  
	
	# Exclude records with provincie = NA, jaar = NA
#	allData <- allData[with(allData, !is.na(provincie) & !is.na(jaar)), ]
  allData <- allData[with(allData, !is.na(jaar)), ]
  
	
	# Rename categorie NA to "Onbekend"
	allData$categorie[is.na(allData$categorie) | allData$categorie == ""] <- "Onbekend"
	
  # Rename provincie NA to "Onbekend"
  allData$provincie <- factor(allData$provincie, levels = levels(addNA(allData$provincie)), 
      labels = c(levels(allData$provincie), "Onbekend"), exclude = NULL)
  
  levelsProvincie <- levels(allData$provincie)
     
  
	
	if (categorie == "typePercent") {
		
		if (!jaar %in% allData$jaar | !jaar %in% assignedData$jaar)
			stop("Niet beschikbaar: Geen data voor het gekozen jaar")
		
	} else {
		
		if (!jaar %in% allData$jaar)
			stop("Niet beschikbaar: Geen data voor het gekozen jaar")
		
	}
	
	
	
	
	
	# A. Current Year
	
	# Select data
	tableData <- allData[allData$jaar == jaar, ]
	if (categorie == "typePercent")
		tableAssignedData <- assignedData[assignedData$jaar == jaar, ]
	
	# Define names and ordering of factor levels
	if (categorie == "leeftijd" & wildNaam == "Wild zwijn") {  # wild zwijn for leeftijd
		
		levelsCategorie <- c("Frisling", "Overloper", "Volwassen", "Onbekend")
		
	} else if (categorie == "leeftijd" & wildNaam == "Ree") {  # ree for leeftijd
		
		levelsCategorie <- c("Kits", "Jongvolwassen", "Volwassen", "Onbekend")
		
	} else if (grepl("type", categorie) & wildNaam == "Ree") {  # ree for type
		
		levelsCategorie <- c("REEGEIT", "REEBOK", "REEKITS")
		
	}
	
	
	# Summary of the data
	summaryData <- count(tableData, vars = names(tableData))
	
	if (categorie == "typePercent") {
		
		summaryData <- merge(x = summaryData, y = tableAssignedData, all = TRUE)
    summaryData$totaal[is.na(summaryData$totaal)] <- 0
		summaryData$percent <- summaryData$freq/summaryData$totaal
		
	} 
	
	
	# Add province/categorie with 0 observations
	fullData <- expand.grid(
			provincie = unique(allData$provincie),
			categorie = levelsCategorie,
			jaar = jaar)
	summaryData <- merge(summaryData, fullData, all = TRUE)
	
	if (categorie == "typePercent")
		summaryVariables <- c("freq", "totaal") else
		summaryVariables <- "freq"
	
	
	summaryTables <- lapply(summaryVariables, function(iVariable) {
				
				# Long to wide table
				summaryTable <- dcast(summaryData, provincie ~ categorie, value.var = iVariable)
				
				# Optimal displaying of the table
				summaryTable[is.na(summaryTable)] <- 0
				summaryTable <- summaryTable[, c("provincie", levelsCategorie)]
				
				# Add row and column sum
				levels(summaryTable$provincie) <- c(levels(summaryTable$provincie), "Vlaanderen")
				summaryTable <- rbind(summaryTable, 
						c(provincie = "Vlaanderen", as.list(apply(summaryTable[, levelsCategorie], 2, sum))))
				summaryTable <- cbind(summaryTable, 
						Totaal = apply(summaryTable[, levelsCategorie], 1, sum))
				
				
				return(summaryTable)
				
			})
	
	
	if (categorie == "typePercent") {
		
		percentages <- summaryTables[[1]][, -1]/summaryTables[[2]][, -1]
		summaryTable <- cbind(provincie = summaryTables[[1]][, 1], percentages)
		
	} else {
		
		summaryTable <- summaryTables[[1]]
		
	}
	
	
	# B. Calculate differences with 1, 5, 10 years ago
	
	finalTable <- summaryTable
	
	
	# Select data
	for (yearsBack in c(1, 5, 10)) {
		
		freqBack <- count(allData[allData$jaar == (jaar - yearsBack), ], 
				vars = "provincie")
		
		# Only calculate trend if relevant
		if (nrow(freqBack) > 0) {
			
			# Add provinces with 0 observations
			freqBack <- merge(x = data.frame(provincie = unique(allData$provincie)),
					y = freqBack, all = TRUE)
			freqBack$freq[is.na(freqBack$freq)] <- 0
			
			# Calculate percentages
			if (categorie == "typePercent") {
				
				freqBackAssigned <- count(assignedData[assignedData$jaar == (jaar - yearsBack), ],
						vars = "provincie", wt_var = "totaal")
				freqBackAssigned$totaal <- freqBackAssigned$freq
				freqBackAssigned$freq <- NULL
				
				freqBack <- merge(x = freqBack, y = freqBackAssigned, all = TRUE)
				freqBack$percent <- freqBack$freq/freqBack$totaal
				
			}
			
			# Add row for Vlaanderen
			levels(freqBack$provincie) <- c(levels(freqBack$provincie), "Vlaanderen")
			if (categorie == "typePercent")
				freqBack <- rbind(freqBack, list(provincie = "Vlaanderen", 
								freq = sum(freqBack$freq, na.rm = TRUE), 
								totaal = sum(freqBack$totaal, na.rm = TRUE), 
								percent = sum(freqBack$freq, na.rm = TRUE)/sum(freqBack$totaal, na.rm = TRUE))) else 
				freqBack <- rbind(freqBack, list(provincie = "Vlaanderen", 
								freq = sum(freqBack$freq, na.rm = TRUE)))
			
			freqBack$totaal <- NULL
			
			
			# Calculate trend
			finalTable <- join(x = finalTable, y = freqBack, by = "provincie")
      finalTable[, paste("Warning", yearsBack, "jaar")] <- c("zwart", "oranje", "rood")[
        ifelse(
        summaryTables[[1]]$Totaal != 0 & finalTable$freq != 0,
        (summaryTables[[1]]$Totaal < minForTrend) + (finalTable$freq < minForTrend),
        0) + 1]
			finalTable[, paste0("Verandering tov ", yearsBack, " jaar (", jaar - yearsBack, ")")] <- 
					ifelse(summaryTables[[1]]$Totaal != 0 & finalTable$freq != 0,
							{ if (categorie == "typePercent") 
									value <- round((finalTable$Totaal - finalTable$percent)*100, 1) else
									value <- round((finalTable$Totaal/finalTable$freq - 1)*100, 1)
								charValue <- sprintf("%.1f", value)
								ifelse(value > 0, paste0("+", gsub(pattern = "\\.", "\\,", charValue), "%"), 
										paste0(gsub(pattern = "\\.", "\\,", charValue), "%"))},
							"")
			
			finalTable$freq <- NULL
			finalTable$percent <- NULL
			finalTable[is.na(finalTable)] <- ""
			
		}
		
	}
	
	
	rowOrder <- match(c(levelsProvincie, "Vlaanderen"), finalTable$provincie)
	toReturn <- finalTable[rowOrder[!is.na(rowOrder)], c("provincie", levelsCategorie, "Totaal",
					names(finalTable)[grep(pattern = "Verandering", x = names(finalTable))],
          names(finalTable)[grep(pattern = "Warning", x = names(finalTable))])]
	
	# If the selected year is not relevant, return NULL
	if (all(toReturn$Totaal == "Inf"))
		return(NULL)
	
	
	if (categorie == "typePercent") {
    toReturn[, c(levelsCategorie, "Totaal")] <- 
      sapply(toReturn[, c(levelsCategorie, "Totaal")], function(x)
          paste0(round(as.numeric(x)*100), "%"))
    
    
    toReturn <- toReturn[toReturn$provincie != "Onbekend", ]
      }
	
	# Rename provincie
	names(toReturn)[names(toReturn) == "provincie"] <- "Provincie"
	
	
	return(toReturn)
	
}



#' Shiny module for creating the plot \code{\link{tableProvince}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams tableProvince
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
tableProvinceServer <- function(id, data, categorie, timeRange) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "tableProvince", 
        data = data,
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "tableProvince",
        plotFunction = "tableProvince", 
        data = data, 
        categorie = categorie)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{tableProvince}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
tableProvinceUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkTableProvince"), 
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkTableProvince % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("tableProvince"), 
            showYear = TRUE, exportData = TRUE),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, 
          tableModuleUI(id = ns("tableProvince"))
        ),
        tags$hr()
      )
    )
  )
  
  
}