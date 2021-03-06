# Interactive barplot for wildschade data in function of year
# 
# Author: mvarewyck
###############################################################################



#' Create interactive barplot for wildschade data variable of interest ifo year
#' @inheritParams countYearAge
#' @param type character, variable name in \code{data} of interest 
#' @return list with
#' \itemize{
#' \item{'plot': }{plotly object}
#' \item{'data': }{data.fram used for plot}
#' } 
#' @author mvarewyck
#' @import plotly
#' @importFrom RColorBrewer brewer.pal
#' @export
countYearSchade <- function(data, jaartallen = NULL, type = NULL,
        summarizeBy = c("count", "percent"),
        width = NULL, height = NULL) {
    
    # For R CMD check
    freq <- NULL
    
    typeNaam <- switch(type,
            "wildsoort" = "Wildsoort",
            "SoortNaam" = "Gewas", 
            "schadeCode" = "Type Schade",
            type
    )
    
    summarizeBy <- match.arg(summarizeBy)
    
    if (is.null(jaartallen))
        jaartallen <- unique(data$afschotjaar)
    
    # Select data
    plotData <- data[data$afschotjaar %in% jaartallen, 
            c("afschotjaar", type)]
    names(plotData) <- c("jaar", "variabele")
    
    # Percentage collected
    nRecords <- nrow(plotData)
    
    # Remove some categories
    plotData[is.na(plotData$variabele), "variabele"] <- "Onbekend"
    plotData <- plotData[!is.na(plotData$jaar) & !is.na(plotData$variabele), ]
    
    # Summarize data per year and age category
    summaryData <- count(df = plotData, vars = names(plotData))
    
    
    # Add line for records with 0 observations
    fullData <- cbind(expand.grid(
                    jaar = min(summaryData$jaar):max(summaryData$jaar),
                    variabele = unique(summaryData$variabele)))
    summaryData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
    summaryData$freq[is.na(summaryData$freq)] <- 0
    
    
    # Calculate percentages 
    summaryData <- ddply(summaryData, "jaar", transform, 
            percent = freq / sum(freq) * 100)
    
    # Summarize data per year
    totalCount <- count(df = plotData, vars = "jaar")
    totalCount$totaal <- totalCount$freq
    totalCount$freq <- NULL
    
    summaryData <- merge(summaryData, totalCount)
    
    
    # Make full schade names
    if (type == "schadeCode") {
      summaryData$variabele <- names(fullNames(summaryData$variabele))
    }
    
    # For optimal displaying in the plot
    summaryData$jaar <- as.factor(summaryData$jaar)
    
    if (summarizeBy == "count") {
        
        summaryData$text <- paste0("<b>", summaryData$variabele, " in ", summaryData$jaar, "</b>",
                "<br>Aantal: ", summaryData$freq, 
                "<br>Totaal: ", summaryData$totaal)
        
    } else {
        
        summaryData$text <- paste0("<b>", summaryData$variabele, " in ", summaryData$jaar, "</b>",
                "<br>Percent: ", round(summaryData$percent), "%")
        
    }
    
    # Max. 40 colors
    paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
    colors <- unlist(sapply(paletteNames, function(x)
        suppressWarnings(brewer.pal(n = 12, name = x))))[1:length(unique(summaryData$variabele))]
    names(colors) <- unique(summaryData$variabele)
    if ("onbekend" %in% tolower(unique(summaryData$variabele)))
      colors[tolower(names(colors)) == "onbekend"] <- "gray"
    
    title <- paste0(typeNaam, " ",
            ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
                    paste("in", jaartallen))
    )
    
    
    
    # Create plot
    toPlot <- plot_ly(data = summaryData, x = ~jaar,
                    y = if (summarizeBy == "count") ~freq else ~percent, 
                    color = ~variabele, text = ~text, hoverinfo = "text+name",
                    colors = colors, type = "bar",
                    width = width, height = height) %>%
            layout(title = title,
                    xaxis = list(title = "Jaar"), 
                    yaxis = list(title = if (summarizeBy == "count") "Aantal" else "Percentage"),
                    barmode = if (nlevels(summaryData$jaar) == 1) "group" else "stack",
                    # hardcode graph size to prevent legend overlapping plot
                    autosize = FALSE,
                    width = 800, 
                    height = 600,
                    margin = list(b = 120, t = 100),
                    legend = list(y = 0.1),
                    annotations = list(
                            x = totalCount$jaar, 
                            y = if (summarizeBy == "count") totalCount$totaal else 100, 
                            text = paste(if (length(unique(totalCount$jaar)) == 1) "totaal:" else "", 
                                    totalCount$totaal),
                            xanchor = 'center', yanchor = 'bottom',
                            showarrow = FALSE),
                    showlegend = TRUE
            )
    
    
    
    colsFinal <- colnames(summaryData)[
            !colnames(summaryData) %in% c("text", 
                    if(summarizeBy == "count")	"percent"	else	c("freq", "totaal")
            )
    ]
    

    summaryDataFinal <- summaryData[, colsFinal]
    
    # Change variable name
    if ("freq" %in% names(summaryDataFinal)) {
      names(summaryDataFinal)[names(summaryDataFinal) == "freq"] <- "aantal"
      
    }
    
    # To prevent warnings in UI
    toPlot$elementId <- NULL
    
    
    return(list(plot = toPlot, data = summaryDataFinal))
}