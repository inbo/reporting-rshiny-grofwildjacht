# Project: inbo-grofwildjacht_git
# Frequency table for type schade 
# 
# Author: Eva Adriaensen
###############################################################################


#' Create summary table for schadeCode by region.
#' @param type character, defines the region variable of interest in the table
#' @param schadeChoices character, chosen schade types (basisCode) to filter on
#' @param schadeChoicesVrtg character, chosen schade types related to "VRTG" to filter on, optional
#' @param schadeChoicesGewas character, chosen schade types related to "GEWAS" to filter on, optional
#' @inheritParams tableProvince
#' @inheritParams countYearProvince
#' @inheritParams filterSchade
#' @return a list containing a data.frame (\code{data}) and an html table header (\code{header}) 
#' specifying multicolumn column names 
#' @author Eva Adriaensen
#' @importFrom reshape2 dcast
#' @importFrom plyr count
#' @importFrom magrittr "%>%"
#' @importFrom utils tail
#' @export
tableSchadeCode <- function(data, jaartallen = NULL,
        type = c("provinces", "flanders", "faunabeheerzones"), 
        sourceIndicator = NULL, 
        schadeChoices = NULL, schadeChoicesVrtg = NULL, schadeChoicesGewas = NULL) {
  
  if (is.null(schadeChoices) & is.null(schadeChoicesGewas) & is.null(schadeChoicesVrtg)){
    stop("Niet beschikbaar")
  }    
  type = match.arg(type)
  
  if (!"GEWAS" %in% schadeChoices)
    schadeChoicesGewas <- NULL
  
  if (!"VRTG" %in% schadeChoices)
    schadeChoicesVrtg <- NULL

  schadeSubchoices <- c(schadeChoicesVrtg, schadeChoicesGewas)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  # filter for source
  allData <- filterSchade(plotData = data, sourceIndicator = sourceIndicator,
    returnStop = "message")
  
  allData$locatie <- switch(type,
          flanders = "Vlaams Gewest",
          provinces = allData$provincie,
          faunabeheerzones = allData$FaunabeheerZone)
  
  # Force all fbz's in the summary table even if never occured    
  if (type == "faunabeheerzones") {
    allData$locatie <- factor(allData$locatie, levels = as.character(c(1:10)))    
    levelsLocatie <- levels(allData$locatie)
  } else {
    allData$locatie <- as.factor(allData$locatie)    
    levelsLocatie <- levels(allData$locatie)
  }
     
  # Select data
  tableData <- allData[allData$afschotjaar %in% jaartallen, c("afschotjaar", "locatie", "schadeBasisCode", "schadeCode")]
    
  if (nrow(tableData) == 0)
    stop("Niet beschikbaar: Geen data voor de gekozen periode")
  
  # Exclude records with provincie = NA, afschotjaar = NA
  tableData <- tableData[!is.na(tableData$afschotjaar) & !is.na(tableData$locatie), ]
    
  # Summary of the data
  summaryData <- count(tableData, vars = setdiff(names(tableData), "afschotjaar"))
  
  # Include all possible locations and selected schadeCodes
  fullData <- expand.grid(
      locatie = levelsLocatie,
      schadeCode = if ("ANDERE" %in% schadeChoices) {
            unique(c(schadeSubchoices, as.character(unique(allData$schadeCode)), "ANDERE"))
            
      } else {
        unique(c(schadeSubchoices, as.character(unique(allData$schadeCode))))

      }
)
  
  summaryData <- merge(summaryData, fullData, all = TRUE)
  
  # Long to wide table
  summaryTable <- dcast(summaryData, locatie ~ schadeCode, value.var = "freq")
  
  # Optimal displaying of the table
  summaryTable[is.na(summaryTable)] <- 0
  
  # Extract header info and counts
  comb <- data.frame(rbind(
  if ("VRTG" %in% schadeChoices)
    expand.grid("VRTG", schadeChoicesVrtg, stringsAsFactors = FALSE),
  if ("GEWAS" %in% schadeChoices)
    expand.grid("GEWAS", schadeChoicesGewas, stringsAsFactors = FALSE),
  if ("ANDERE" %in% schadeChoices) {
    df2 <- unique(allData[allData$schadeBasisCode == "ANDERE", c("schadeBasisCode", "schadeCode")])
    if (nrow(df2) == 0L)
    	df2 <- data.frame(cbind("ANDERE", "ANDERE"), stringsAsFactors=FALSE)
#    df2 <- unique(allData[c("schadeBasisCode", "schadeCode")])
#    df2 <- df2[df2$schadeBasisCode == "ANDERE",]
    names(df2) <- c("Var1", "Var2")
    df2
  }))

  allSchadeCode <- unique(comb$Var2)
  
  # number of schadeCodes by schadeBasisCode
  columsPerSchadeBasisCode <- rev(table(comb$Var1))

  # group columns together from same schadeBasisCode
  summaryTable <- summaryTable[, c(colnames(summaryTable)[1],fullNames(comb$Var2))]
  
  
  # Add row and column sum
  levels(summaryTable[,"locatie"]) <- c(levels(summaryTable[,"locatie"]),"Vlaanderen")
  
  if (type != "flanders") {
    newRow <- as.list(apply(as.matrix(summaryTable[, allSchadeCode]), 2, sum)) # causes error
    # assign names manually; needed in case of allSchadeCode
    if (is.null(names(newRow)) & length(allSchadeCode) == 1L)
      names(newRow) <- allSchadeCode
    summaryTable <- rbind(summaryTable, 
      c(locatie = "Vlaanderen", newRow))
  }

  summaryTable <- cbind(summaryTable, 
      Totaal = apply(as.matrix(summaryTable[, setdiff(names(summaryTable), "locatie")]), 1, sum))
  
  # Rename locatie
  names(summaryTable)[names(summaryTable) == "locatie"] <- "Locatie"
  
  # Full column names
  columnFullNames <- names(fullNames(colnames(summaryTable)))
  colnames(summaryTable)[!is.na(columnFullNames)] <- columnFullNames[!is.na(columnFullNames)]

  # Manage table header with multiline
  tableHeader <- tags$table(
      class  = 'display',
      tags$thead(
          tags$tr(
            tags$th(rowspan = 2, names(summaryTable)[1]) ,
            lapply(names(columsPerSchadeBasisCode), function(iName) {
           	  tags$th(colspan = columsPerSchadeBasisCode[iName], names(fullNames(iName)))
            }),
            tags$th(rowspan = 2, tail(names(summaryTable), n=1))
          ),
          tags$tr(
              #TODO
              lapply(names(summaryTable)[names(summaryTable) %in% names(fullNames(comb$Var2))], tags$th)
          )
      )
   )


  return(list(data = summaryTable, header = tableHeader))
  


	
}