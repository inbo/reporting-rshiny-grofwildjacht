# Summary table for species
# 
# Author: mvarewyck
###############################################################################



#' Create summary table with trend over years per \code{categorie} and in total
#' @param data data.frame with raw data for summary table
#' @param categorie character, variable name in \code{data} for 
#' which rows should be created in the table
#' @inheritParams percentageYearlyShotAnimals 
#' @param minForTrend numeric, the minimum number of records needed before
#' a trend is being reported
#' @return data.frame, number or percentage of observations 
#' per province and per \code{categorie}
#' @author mvarewyck
#' @importFrom reshape2 dcast
#' @importFrom plyr count join
#' @export
tableSpecies <- function(data, jaar = NULL, categorie = "leeftijd_comp", 
  minForTrend = 10) {
  
  wildNaam <- unique(data$wildsoort)  
  
  if (is.null(jaar))
    stop("Gelieve jaartal te selecteren")
  
  allData <- data[, c("PartijNummer", categorie, "afschotjaar")]
  names(allData) <- c("locatie", "categorie", "jaar")
  
  
  # Exclude records with jaar = NA
  allData <- allData[with(allData, !is.na(jaar)), ]
  
  if (!jaar %in% allData$jaar)
      stop("Niet beschikbaar: Geen data voor het gekozen jaar")
  
  
  # A. Current Year
  
  tableData <- allData[allData$jaar == jaar, ]
  
  # Summary of the data
  summaryData <- count(tableData, vars = names(tableData))
  nTotal <- sum(summaryData$freq)
  summaryData$percent <- summaryData$freq / nTotal * 100
  summaryData$current <- summaryData$freq
  summaryData$freq <- NULL
  
  # Add 'totaal'
  levelsCategorie <- c(loadMetaEco(species = wildNaam)[[categorie]], "Totaal")
  
  # Add categorie with 0 observations
  fullData <- expand.grid(
    locatie = unique(allData$locatie),
    categorie = levelsCategorie,
    jaar = jaar)
  summaryTable <- merge(summaryData, fullData, all = TRUE)
  summaryTable$current[summaryTable$categorie == "Totaal"] <- nTotal
  summaryTable$percent <- ifelse(is.na(summaryTable$percent), "", 
    paste0(gsub(pattern = "\\.", "\\,", sprintf("%.1f", summaryTable$percent)), "%"))
  summaryTable[is.na(summaryTable)] <- 0
  
  
  # B. Calculate differences with 1, 5, 10 years ago
  
  finalTable <- summaryTable
  
  
  # Select data
  for (yearsBack in c(1, 5, 10)) {
    
    freqBack <- count(allData[allData$jaar == (jaar - yearsBack), ], 
      vars = "categorie")
     
    # Only calculate trend if relevant
    if (nrow(freqBack) > 0) {
      
      # Add categorie with 0 observations
      freqBack <- merge(x = fullData[, "categorie", drop = FALSE],
        y = freqBack, all = TRUE)
      freqBack$freq[is.na(freqBack$freq)] <- 0
      
      # Add row for Totaal
      freqBack$freq[freqBack$categorie == "Totaal"] <- sum(freqBack$freq, na.rm = TRUE)
      
      # Calculate trend
      finalTable <- join(x = finalTable, y = freqBack, by = "categorie")
      finalTable[, paste("Warning", yearsBack, "jaar")] <- c("zwart", "oranje", "rood")[
        ifelse(
        finalTable$current != 0 & finalTable$freq != 0,
        (finalTable$current < minForTrend) + (finalTable$freq < minForTrend),
        0) + 1]
      finalTable[, paste0("Verandering tov ", yearsBack, " jaar (", jaar - yearsBack, ")")] <- 
        ifelse(finalTable$current != 0 & finalTable$freq != 0, {
          value <- round((finalTable$current/finalTable$freq - 1)*100, 1)
            charValue <- sprintf("%.1f", value)
            ifelse(value > 0, paste0("+", gsub(pattern = "\\.", "\\,", charValue), "%"), 
              paste0(gsub(pattern = "\\.", "\\,", charValue), "%"))},
          "")
      
      finalTable[is.na(finalTable)] <- ""
      
      finalTable$freq <- NULL
      
    }
    
  }
  
  
  # Order rows and columns
  toReturn <- finalTable[match(levelsCategorie, finalTable$categorie), c("categorie", "current", "percent", 
      names(finalTable)[grep(pattern = "Verandering", x = names(finalTable))],
      names(finalTable)[grep(pattern = "Warning", x = names(finalTable))])]
  
  
  # Rename columns
  colnames(toReturn)[1:3] <- c("Categorie", "Afschot", "Afschot relatief")
  
  return(list(data = toReturn))
  
}




#' Shiny module for creating the plot \code{\link{countAgeGender}} - server side
#' @inheritParams mapFlandersServer
#' @param data data.frame for the plot function
#' @param timeRange numeric vector of length 2, min and max year to subset data
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
tableSpeciesServer <- function(id, data, timeRange, species) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Gerapporteerd afschot per regio en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "tableSpecies", 
        data = data,
        timeRange = timeRange,
        categories = reactive({
            allChoices <- c("label" = "labeltype", "type" = "type_comp", "geslacht" = "geslacht_comp", "leeftijd" = "leeftijd_comp")
            if (species() != "Ree")
              allChoices[-1] else
              allChoices
          })
      
      )
      callModule(module = plotModuleServer, id = "tableSpecies",
        plotFunction = "tableSpecies", 
        data = data)
           
    })
  
}


#' Shiny module for creating the plot \code{\link{countAgeGender}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
tableSpeciesUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    h3(HTML(uiText$title)),
    
    fixedRow(
      
      column(4,
        optionsModuleUI(id = ns("tableSpecies"), showYear = TRUE, 
          showCategorie = TRUE, exportData = TRUE),
        tags$p(HTML(uiText[, id]))
      ),
      column(8, tableModuleUI(id = ns("tableSpecies")))
    
    ),
    tags$hr()
    
  )
    
}

