# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate barplots for  Maatschappelijk draagvlak (F12_1)
#' 
#' @param data data.frame 
#' @param groupVariable character vector containing the name of the column in plotData
#' for which each unique value a separate subplot is created
#' @param xVar character, column in \code{plotData} for x-axis;
#' default value is 'percentage'
#' @param yVar character, column in \code{plotData} for y-axis
#' @inheritParams barBiotoop
#' 
#' @return list with plotly object and data.frame
#' 
#' @author wverlinden
#' @import plotly
#' @importFrom RColorBrewer brewer.pal
#' @export 
barDraagkracht <- function(data, groupVariable = NULL, 
  xVar = "percentage", yVar = NULL, width = 1000, height = NULL) {
  
  
  if (xVar == "percentage") {
    
    data$percentage <- as.numeric(data$Aantal_tot/data$totaal) * 100
    data$percentageLabel <- paste0(round(data$percentage, 2), "% (n = ", data$totaal, ")")
    
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
        
        yLabels <- unique(subData[[yVar]])
        totalCounts <- subData$totaal[unique(match(subData[[yVar]], yLabels))]
        
        plotList[[(length(plotList) + 1)]] <- plot_ly(
            data = subData, 
            x = ~get(xVar), 
            y = ~as.factor(get(yVar)), 
            text = ~paste("<b>", get(groupVariable), "</b><br>", get(yVar), "<br>", percentageLabel), 
            textposition = "none",
            type = 'bar', name = ~Antwoord, color = ~Antwoord, colors = myColors, 
            legendgroup = ~Antwoord, 
            showlegend = (iVar == groupLevels[[1]] && jVar == secondGroup[1]),
            hoverinfo = "text"
          ) %>%
          layout(
            annotations = list(
              # groupVariable text
              list(x = 0, y = 1, 
                text = if (jVar == secondGroup[1]) gsub(" ", "<br>", as.character(iVar)) else "",
                showarrow = FALSE, font = list(size = 12),
                textangle = -90,
                xref = 'paper', yref = 'paper', xanchor = 'bottom', yanchor = 'top')
            ),
            # groupVariable gray bar
            shapes = list(
              type = "rect",
              x0 = 0,
              x1 = 40,
              xref = "paper",
              xanchor = 0,
              xsizemode = "pixel",
              y0 = 0, 
              y1 = 1,
              yref = "paper",
              fillcolor = toRGB("gray80"),
              line = list(color = "transparent")
            ),
            legend = list(title = list(text = "<b>Antwoord</b>")),
            barmode = "relative",
            xaxis = list(title = "Percentage"),
            yaxis = list(title = "", ticksuffix = "  "),
            margin = c(200, 0, 0, 0)
          )
        
      }
    
    myPlot <- do.call(subplot, c(plotList,
        shareX = TRUE,
        nrows = length(groupLevels[[1]]),
        margin = 0.005))
    
    extraVars <- c("Antwoord", groupVariable)
    
  } else if (xVar == "percentage") {
    
    # rename x-axis ticktext
    yLabels <- unique(data[[yVar]])
    totalCounts <- data$totaal[unique(match(data[[yVar]], yLabels))]
    yLabels[yLabels == "populatie_evolutie"] <- "Populatie everzwijnen"
    yLabels[yLabels == "schade_landbouw_evolutie"] <- "Schade aan de landbouw"
    yLabels[yLabels == "schade_privpub_evolutie"] <- "Schade aan privéterreinen"
    yLabels[yLabels == "schade_verkeer_evolutie"] <- "Schade in het verkeer"
    
    myPlot <- plot_ly(data, x = ~get(xVar), y = ~get(yVar), 
        type = 'bar', color = ~Antwoord, colors = myColors, text = ~percentageLabel,
#        marker = list(line = list(width = 5, color = "lightgray")),
        hoverinfo = "text+y+name",
        width = width, height = height
#            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
#                '<b>Antwoord</b>: %{text}')
      ) %>%
      layout(
        legend = list(title = list(text = "<b> Antwoord </b>"), traceorder = 'normal'),
        barmode = "relative",
        yaxis = list(title = ""),
        xaxis = list(title = "Percentage") 
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
  
  
  return(list(plot = myPlot, data = data[, c(extraVars, xVar, yVar), with = FALSE]))
  
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
barDraagkrachtServer <- function(id, data, groupVariable = NULL, xVar = "percentage", yVar,
  title = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      sectors <- list(
        stakeholders = c('Jagers', 'Landbouwers', 'Natuurvereniging'),
        public = c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied')
      )
      
      observeEvent(input$sectorMain, {
          
          updateSelectInput(session, inputId = "sectorUsers", 
            choices = sectors[[input$sectorMain]], 
            selected = sectors[[input$sectorMain]])
          
        })
      
      output$groupChoices <- renderUI({
          
          choices <- unique(data()[[groupVariable]])
          
          selectInput(inputId = ns("groups"), label = NULL,
            choices = choices, selected = choices, multiple = TRUE, width = "100%")
          
        })
      
      subData <- reactive({
          
          toReturn <- data()
          
          if (!is.null(input$sectorMain))
            validate(need(input$sectorUsers, "Gelieve groepen te selecteren"))
          
          if (!is.null(input$sectorUsers))
            toReturn <- toReturn[toReturn[[yVar]] %in% input$sectorUsers, ]
          
          if (!is.null(input$groups))
            toReturn <- toReturn[toReturn[[groupVariable]] %in% input$groups, ]
          
          
          toReturn
            
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
#' @param sectorChoices named character vector, choices for sector
#' @param selectGroups boolean, whether user should be able to select groups
#' @param height character, height of the plot
#' 
#' @author mvarewyck
#' @export
barDraagkrachtUI <- function(id, uiText, sectorChoices = NULL, 
  selectGroups = FALSE, height = "600px") {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == paste(strsplit(id, "_")[[1]][-1], collapse = "_"), ]
    
  tagList(
    
    actionLink(inputId = ns("linkDraagkracht"),
      label = paste("FIGUUR:", uiText$title), class = "action-h3"),
    conditionalPanel("input.linkDraagkracht % 2 == 0", ns = ns,
      
      if (!is.null(sectorChoices) | selectGroups)
        wellPanel(
          fluidRow(
            column(3,
              if (!is.null(sectorChoices))
                radioButtons(inputId = ns("sectorMain"), label = NULL, choices = sectorChoices, inline = TRUE),
              selectInput(inputId = ns("sectorUsers"), label = NULL,
                choices = c('Jagers', 'Landbouwers', 'Natuurvereniging'), 
                selected = c('Jagers', 'Landbouwers', 'Natuurvereniging'),
                multiple = TRUE)
            ),
            if (selectGroups)
              column(9, uiOutput(ns("groupChoices")))
          
          )
        ),
      
      tags$p(HTML(uiText[, strsplit(id, split = "_")[[1]][1]])),
      
      plotModuleUI(id = ns("barDraagkracht"), height = height),
      optionsModuleUI(id = ns("barDraagkracht"), exportData = TRUE,
        doWellPanel = FALSE),
      tags$hr()
    )
  )
  
}



