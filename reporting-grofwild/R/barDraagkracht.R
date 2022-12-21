# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplots for  Maatschappelijk draagvlak (F12_1)
#' 
#' @param data data.frame 
#' @param groupVariable character vector containing the name of the column in plotData
#' for which each unique value a separate subplot is created
#' @param yVar character, column in \code{plotData} for x-axis;
#' default value is 'percentage'
#' @param xVar character, column in \code{plotData} for y-axis
#' @inheritParams barBiotoop
#' 
#' @return list with plotly object and data.frame
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom RColorBrewer brewer.pal
#' @export 
barDraagkracht <- function(data, groupVariable = NULL, 
  yVar = "percentage", xVar = NULL, width = 1000, height = NULL) {
  
  
  if ("percentage" %in% c(xVar, yVar)) {
    
    data$percentage <- as.numeric(data$Aantal_tot/data$totaal) * 100
    data$percentageLabel <- paste0(round(data$percentage, 2), "%")
    
    answerLevels <- list(
      neutral = c('Hetzelfde', 'Neutraal'),
      negative = c('Afgenomen', 'Negatief', 'Nee', 'Groot', 'Onbelangrijk'),
      positive = c('Toegenomen', 'Positief', 'Ja', 'Klein', 'Belangrijk')
    )
    data$Antwoord <- factor(data$Antwoord_reclass, 
      levels = unlist(answerLevels))
    data$Antwoord <- droplevels(data$Antwoord)  
    
    # Modify percentage for the graph
    neutralLevel <- levels(data$Antwoord)[levels(data$Antwoord) %in% answerLevels$neutral]
    negativeLevel <- levels(data$Antwoord)[levels(data$Antwoord)  %in% answerLevels$negative]
    
    if (length(neutralLevel) > 0) {
      dataExtra <- data[data$Antwoord %in% neutralLevel, ]
      data$percentage[data$Antwoord %in% c(negativeLevel, neutralLevel)] <- -1*data$percentage[data$Antwoord %in% c(negativeLevel, neutralLevel)]
      data <- rbind(data, dataExtra)
      data$percentage[data$Antwoord %in% neutralLevel] <- data$percentage[data$Antwoord %in% neutralLevel] / 2
    } else data$percentage[data$Antwoord %in% negativeLevel] <- -1*data$percentage[data$Antwoord %in% negativeLevel]
    
    myColors <- c(
      if (length(neutralLevel) > 0) "lightgrey", 
      inbo_palette(n = 4)[c(4, 2)])
    
  }
  
  
  if (!is.null(groupVariable)) {
    
    groupLevels <- sapply(groupVariable, function(x) unique(data[[x]]), simplify = FALSE)
    
    plotList <- list()
    
    secondGroup <- if (length(groupVariable) > 1)
        groupLevels[[2]] else
        ""
    
    for (jVar in secondGroup) 
      for (iVar in groupLevels[[1]]) {
        
        subData <- if (jVar == "")
            data[data[[groupVariable[1]]] %in% iVar, ] else
            data[data[[groupVariable[1]]] %in% iVar & data[[groupVariable[2]]] %in% jVar, ]
        
        xLabels <- unique(subData[[xVar]])
        totalCounts <- subData$totaal[unique(match(subData[[xVar]], xLabels))]
        
        plotList[[(length(plotList) + 1)]] <- plot_ly(
            data = subData, 
            x = ~as.factor(get(xVar)), 
            y = ~get(yVar), 
            text = ~percentageLabel, textposition = "none",
            type = 'bar', name = ~Antwoord, color = ~Antwoord, colors = myColors, 
            legendgroup = ~Antwoord, 
            showlegend = (iVar == groupLevels[[1]] && jVar == secondGroup[1]),
            hoverinfo = "text+x+name"
          ) %>%
          layout(
            annotations = list(
              # columns
              list(x = 0.5, y = 1, 
                text = if (jVar == secondGroup[1]) gsub(" ", "<br>", as.character(iVar)) else "",
                showarrow = FALSE, font = list(size = 12), 
                textangle = if (length(groupLevels[[1]]) > 8) 30,
                xref = 'paper', yref = 'paper', xanchor = 'center', yanchor = 'bottom')
            ),
            legend = list(title = list(text = "<b>Antwoord</b>")),
            barmode = "relative",
            xaxis = list(
              title = "",
              ticktext = as.list(paste0(xLabels, if (length(groupLevels[[1]]) < 5) "\n", " (n = ", totalCounts, ")")), 
              tickvals = as.list((length(xLabels)-1):0),
              tickmode = "array",
              tickangle = if (length(groupLevels[[1]]) < 5) 0 else 30
            ),
            yaxis = list(title = "Percentage"),
            margin = c(0, 0, 1, 0)
          )
        
      }
    
    myPlot <- do.call(subplot, c(plotList,
        shareY = TRUE, 
        nrows = length(secondGroup),
        margin = 0.005))
    
    extraVars <- c("Antwoord", groupVariable)
    
  } else if (yVar == "percentage") {
    
    # rename x-axis ticktext
    xLabels <- unique(data[[xVar]])
    totalCounts <- data$totaal[unique(match(data[[xVar]], xLabels))]
    xLabels[xLabels == "populatie_evolutie"] <- "Populatie everzwijnen"
    xLabels[xLabels == "schade_landbouw_evolutie"] <- "Schade aan de landbouw"
    xLabels[xLabels == "schade_privpub_evolutie"] <- "Schade aan privéterreinen"
    xLabels[xLabels == "schade_verkeer_evolutie"] <- "Schade in het verkeer"
    
    myPlot <- plot_ly(data, x = ~get(xVar), y = ~get(yVar), 
        type = 'bar', color = ~Antwoord, colors = myColors, text = ~percentageLabel,
#        marker = list(line = list(width = 5, color = "lightgray")),
        hoverinfo = "text+x+name",
        width = width, height = height
#            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
#                '<b>Antwoord</b>: %{text}')
      ) %>%
      layout(
        legend = list(title = list(text = "<b> Antwoord </b>"), traceorder = 'normal'),
        barmode = "relative",
        xaxis = list(
          title = "",
          ticktext = as.list(paste0(xLabels, "\n (n = ", totalCounts, ")")), 
          tickvals = as.list((length(xLabels)-1):0),
          tickmode = "array"),
        yaxis = list(title = "Percentage") 
      )
    
    extraVars <- c("Antwoord")
    
  } else {
    
    colors <- replicateColors(nColors = length(unique(data$Type)))$colors
    names(colors) <- unique(data$Type)
    
    myPlot <- plot_ly(data, x = as.character(data[[xVar]]), y = data[[yVar]], type = 'bar',
        color = ~as.factor(Type), colors = colors) %>%
      layout(
        legend = list(title = list(text = "<b> Type </b>")),
        yaxis = list(title = "Aantal"),
        xaxis = list(title = "Jaar"),
        barmode = "stack"
      )
    
    extraVars <- c("Type")
    
  } 
  
  
  return(list(plot = myPlot, data = data[, c(extraVars, yVar, xVar), with = FALSE]))
  
}



#' Shiny module for creating the plot \code{\link{barDraagkracht}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @inheritParams barDraagkracht
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
barDraagkrachtServer <- function(id, data, groupVariable = NULL, yVar = "percentage", xVar,
  title = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subData <- reactive({
          
          if (!is.null(input$subGroup)) {
            
            switch(input$subGroup,
              "stakeholders" = data()[data()[[xVar]] %in% c('Jagers', 'Landbouwers', 'Natuurvereniging'), ],
              "public" = data()[data()[[xVar]] %in% c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied'), ]
            )
            
          } else {
                        
            data()
            
          } 
            
        })
      
      observeEvent(title(), {
          
          updateActionLink(session = session, inputId = "linkDraagkracht",
            label = paste("FIGUUR:", title()))
          
        })
      
      callModule(module = optionsModuleServer, id = "barDraagkracht", 
        data = subData
      )
      toReturn <- callModule(module = plotModuleServer, id = "barDraagkracht",
        plotFunction = "barDraagkracht", 
        data = subData,
        groupVariable = groupVariable,
        xVar = xVar,
        yVar = yVar
      )
      
      return(reactive(toReturn()))
      
    })
  
}


#' Shiny module for creating the plot \code{\link{barDraagkracht}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
barDraagkrachtUI <- function(id, uiText, subGroups = NULL) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == paste(strsplit(id, "_")[[1]][-1], collapse = "_"), ]
    
  tagList(
    
    actionLink(inputId = ns("linkDraagkracht"),
      label = paste("FIGUUR:", uiText$title), class = "action-h3"),
    conditionalPanel("input.linkDraagkracht % 2 == 0", ns = ns,
      
      if (!is.null(subGroups))
        wellPanel(
          radioButtons(inputId = ns("subGroup"), label = NULL, choices = subGroups)
        ),
      tags$p(HTML(uiText[, strsplit(id, split = "_")[[1]][1]])),
      
      plotModuleUI(id = ns("barDraagkracht")),
      optionsModuleUI(id = ns("barDraagkracht"), exportData = TRUE,
        doWellPanel = FALSE),
      tags$hr()
    )
  )
  
}



