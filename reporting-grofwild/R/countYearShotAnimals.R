# Project: grofWild_git
# 
# Author: dbemelmans
###############################################################################

#' Interactive barplot to show the distribution of the hunted animals in a certain interval
#' @inheritParams countHuntingMethod
#' @param interval character, data shown in intervals monthly, biweekly..
#' @param type character, used to filter the data
#' 
#' @importFrom INBOtheme inbo_palette
#' @author dbemelmans
#' @export 
countYearShotAnimals <- function(data, regionLevel, regio, locaties, jaartallen, width = NULL, height = NULL, interval = NULL, type = NULL) {
  
  ## plotly gives a warning: Warning: 'layout' objects don't have these attributes: 'bargroupgap'
  ## This is save to ignore: https://github.com/ropensci/plotly/issues/994
  
  pl <- NULL
  plotData <- data
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  # regions get alreaddy filtered out in the plotModuleServer 
  
  # Select on years
  plotData <- plotData[plotData$afschotjaar %in% jaartallen, 
      c("afschotjaar", "afschot_datum", 
          "type_comp", "geslacht_comp", "wildsoort")]
  
  plotData <- plotData[!is.na(plotData$afschotjaar), ]
  
  
  # only retains animals of specified type
  specifiedType <- !is.null(type) && type != "all"
  if(specifiedType){
    
    plotData$type <- ifelse(plotData$wildsoort != "Ree",
        "", ifelse(grepl("kits", plotData$type_comp), "kits",
            ifelse(plotData$geslacht_comp == "Mannelijk", "bok", "geit")))
    
    plotData <- plotData[plotData$type %in% type, ]
  }
  
  plotData <- plotData[, 
      c("afschotjaar", "afschot_datum")]
  
  noRecords <- plotData
  plotData <- plotData[!is.na(plotData$afschot_datum), ]
  
  
  # Summarize data per province and year
  plotData$afschotjaar <- with(plotData, factor(afschotjaar, levels = 
              min(jaartallen):max(jaartallen)))
  
  
  if(interval == "Per maand") {
    
    
    plotData$maand <- sapply(plotData$afschot_datum, function(datum) {
          as.numeric(stringr::str_split(datum, patter = "-")[[1]][2])
        })
    plotData <- plotData[, c("afschotjaar", "maand")]
    
    summaryData <- melt(table(plotData), id.vars = c("afschotjaar", "maand"))
    
    # For optimal displaying in the plot
    summaryData$maand <- sapply(summaryData$maand, function(x) {
          switch(as.character(x),
              "1" = "januari",
              "2" = "februari",
              "3" = "maart",
              "4" = "april",
              "5" = "mei",
              "6" = "juni", 
              "7" = "juli",
              "8" = "augustus",
              "9" = "september",
              "10" = "oktober",
              "11" = "november",
              "12" = "december")
        })
    summaryData$maand <- as.factor(summaryData$maand)
    summaryData$maand <- factor(summaryData$maand, levels = rev(levels(summaryData$maand)))
    summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
    
    colors <- rev(inbo_palette(n = 1))
    title <- paste0("Afschot van ",
        ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
            jaartallen))
    
    
    # Create plot
    pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~value, color = ~maand, 
            colors = c(rep(colors, 12)),
            type = "bar",  width = width, height = height) %>%
        layout(title = title,
            xaxis = list(title = "Jaar"), 
            yaxis = list(title = "Aantal"),
            margin = list(b = 120, t = 100), 
            barmode = "group", bargap = 0.15, bargroupgap = 0.1,
            showlegend = FALSE)%>% add_annotations(text = paste0(round(nrow(plotData)/nrow(noRecords), 2)*100, 
                "% met gekende afschotdatum (", nrow(plotData), "/", nrow(noRecords), ")"),
            xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
            y = -0.2, yanchor = "bottom", showarrow = FALSE)
    
    
  } else if (interval == "Per seizoen") {
    
    plotData$maand <- sapply(plotData$afschot_datum, function(datum) {
          as.numeric(stringr::str_split(datum, patter = "-")[[1]][2])
        })
    plotData$dag <- sapply(plotData$afschot_datum, function(datum) {
          as.numeric(stringr::str_split(datum, patter = "-")[[1]][3])
        })
    plotData <- plotData[, c("afschotjaar", "maand", "dag")]
    
    plotData$seizoen <- sapply(1:nrow(plotData), function(x) {
          dag <- plotData$dag[x]
          maand <- plotData$maand[x] 
          if(maand %in% 1:2 | maand == 12 & dag >= 21 | maand == 3 & dag < 21) {
            "winter"
          } else if(maand == 3 & dag >= 21 | maand %in% 4:5 | maand == 6 & dag <= 21) {
            "lente"
          } else if(maand == 6 & dag >= 21 | maand %in% 7:8 | maand == 9 & dag <= 21) {
            "zomer"
          } else {
            "herfst"
          }
          
        })
    
    plotData <- plotData[, c("afschotjaar", "seizoen")]
    
    summaryData <- melt(table(plotData), id.vars = c("afschotjaar", "seizoen"))
    summaryData$seizoen <- as.factor(summaryData$seizoen)
    summaryData$seizoen <- factor(summaryData$seizoen, levels = rev(levels(summaryData$seizoen)))
    summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
    
    colors <- rev(inbo_palette(n = 1))
    title <- paste0("Afschot van ",
        ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
            jaartallen))
    
    # Create plot
    pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~value, color = ~seizoen, 
            colors = c(rep(colors, 4)),
            type = "bar",  width = width, height = height) %>%
        layout(title = title,
            xaxis = list(title = "Jaar"), 
            yaxis = list(title = "Aantal"),
            margin = list(b = 120, t = 100), 
            barmode = "group", bargap = 0.15,  bargroupgap = 0.1,
            showlegend = FALSE) %>% add_annotations(text = paste0(round(nrow(plotData)/nrow(noRecords), 2)*100, 
                "% met gekende afschotdatum (", nrow(plotData), "/", nrow(noRecords), ")"),
            xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
            y = -0.2, yanchor = "bottom", showarrow = FALSE)
  } else if(interval == "Per twee weken") {
    
    plotData$maand <- sapply(plotData$afschot_datum, function(datum) {
          as.numeric(stringr::str_split(datum, patter = "-")[[1]][2])
        })
    plotData$dag <- sapply(plotData$afschot_datum, function(datum) {
          as.numeric(stringr::str_split(datum, patter = "-")[[1]][3])
        })
    plotData <- plotData[, c("afschotjaar", "maand", "dag")]
    
    
    plotData$tweewekelijks <- sapply(1:nrow(plotData), function(x) {
          row <- plotData[x, ]
          base <- (row$maand-1)*2 + 1
          if(row$dag <= 15) {
            base
          } else {
            base + 1
          }
          
        }) 
    
    plotData <- plotData[, c("afschotjaar", "tweewekelijks")]
    summaryData <- melt(table(plotData), id.vars = c("afschotjaar", "tweewekelijks"))
    summaryData$tweewekelijks <- as.factor(summaryData$tweewekelijks)
    summaryData$tweewekelijks <- factor(summaryData$tweewekelijks, levels = rev(levels(summaryData$tweewekelijks)))
    summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
    
    colors <- rev(inbo_palette(n = 1))
    title <- paste0("Afschot van ",
        ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
            jaartallen))
    
    # Create plot
    pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~value, color = ~tweewekelijks, 
            colors = c(rep(colors, 24)),
            type = "bar",  width = width, height = height) %>%
        layout(title = title,
            xaxis = list(title = "Jaar"), 
            yaxis = list(title = "Aantal"),
            margin = list(b = 120, t = 100), 
            barmode = "group", bargap = 0.15, bargroupgap = 0.1,
           showlegend = FALSE) %>% add_annotations(text = paste0(round(nrow(plotData)/nrow(noRecords), 2)*100, 
               "% met gekende afschotdatum (", nrow(plotData), "/", nrow(noRecords), ")"),
           xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
           y = -0.2, yanchor = "bottom", showarrow = FALSE)
       
          
  }
  
  
 
  
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  return(list(plot = pl, data = summaryData))
}


