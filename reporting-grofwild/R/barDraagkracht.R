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
    
    data$percentage <- as.numeric(data$percentage)
    data$Antwoord <- factor(data$Antwoord_reclass , 
      levels = c('Toegenomen',
        'Afgenomen',
        'Hetzelfde'
      ))
    
    data$Antwoord <- droplevels(data$Antwoord)  
    nExcept <- sum(c("Hetzelfde") %in% levels(data$Antwoord))
    myColors <- c("darkgreen", "darkred", "gray53")
    
  }
  
  
  # Sample size
  if (is.null(groupVariable)) {
    totalCounts <- sum(data$totaal[!duplicated(data[[yVar]])]) 
  } else if (yVar == "Year" | "Year" %in% groupVariable) {
    myCols <- c(yVar, groupVariable)
    totalCounts <- data[!duplicated(data$totaal), c(myCols, "totaal"), with = FALSE]
    if (length(unique(data$Year)) == 1)
      totalCounts <- totalCounts[!duplicated(totalCounts[[groupVariable[groupVariable != "Year"]]]), ]
  } else {
    totalCounts <- data$totaal[!duplicated(data[[groupVariable]])]  
    names(totalCounts) <- as.character(unique(data[[groupVariable]]))
  }
  
  if (!is.null(groupVariable)) {
    
    groupLevels <- sapply(groupVariable, function(x) unique(data[[x]]), simplify = FALSE)
    yLevels <- levels(as.factor(data[[yVar]]))
    
    plotList <- list()
    
    secondGroup <- if (length(groupVariable) > 1)
        groupLevels[[2]] else
        ""
    
    for (jVar in secondGroup) 
      for (iVar in groupLevels[[1]]) {
        
        subData <- if (jVar == "")
            data[data[[groupVariable[1]]] %in% iVar, ] else
            data[data[[groupVariable[1]]] %in% iVar & data[[groupVariable[2]]] %in% jVar, ]
        
        plotList[[(length(plotList) + 1)]] <- plot_ly(
            data = subData, 
            x = ~get(xVar), 
            y = ~as.factor(get(yVar)), 
            type = 'bar', name = ~Antwoord, color = ~Antwoord, colors = myColors, 
            legendgroup = ~Antwoord, 
            showlegend = (iVar == groupLevels[[1]] && jVar == secondGroup[1]), 
            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
              '<b>Antwoord</b>: %{text}'),
            text = ~Antwoord
          ) %>%
          layout(
            annotations = list(
              # columns
              list(x = 50, y = 1.1, 
                text = if (jVar == secondGroup[1]) paste(as.character(iVar),
                      if (!(yVar == "Year" | "Year" %in% groupVariable) | length(secondGroup) == 1) 
                        paste0("\n(n = ", totalCounts[[iVar]], ")\n")) else "",
                showarrow = FALSE, font = list(size = 16), 
                yref = 'paper'),
              # rows
              list(x = if (iVar == head(groupLevels[[1]], n = 1)) -0.1 else 1.1, y = 0.5, 
                text = paste(as.character(jVar),
                  if ("Year" %in% groupVariable & length(secondGroup) > 1) 
                    paste0("\n(n = ", totalCounts[get(names(groupLevels)[1]) %in% iVar & Year %in% jVar, "totaal"], ")\n")),
                showarrow = FALSE, font = list(size = 16), 
                textangle = if (iVar == head(groupLevels[[1]], n = 1)) -90 else 90,
                xref = 'paper', yref = 'paper')
#              list(x = 0, y = yLevels, 
#                text = paste(as.character(jVar),
#                  if ((yVar == "Year" | "Year" %in% groupVariable)) 
#                    paste0("\n(n = ", totalCounts[get(groupVariable) %in% iVar, totaal], ")\n")),
#                showarrow = FALSE, font = list(size = 16), textangle = 90)
            ),
            legend = list(title = list(text = "<b>Antwoord</b>")),
            barmode = "stack",
            yaxis = if (yVar == "Year")
                list(title = "", 
                  ticktext = as.list(apply(totalCounts[get(names(groupLevels)[1]) %in% iVar, ], 1, function(x) 
                        paste0(x[yVar], "\n(n = ", x["totaal"], ")\n"))),
                  tickvals = as.list(yLevels),
                  tickmode = "array") else
                list(title = ""),
            xaxis = list(title = "Percentage", range = list(0, 100)),
            margin = list(
              t = if (!(yVar == "Year" | "Year" %in% groupVariable)) 50, 
              r = if ("Year" %in% groupVariable & iVar == tail(groupLevels[[1]], n=1)) 50)
          )
        
      }
    
    myPlot <- do.call(subplot, c(plotList, 
        shareY = (!(yVar == "Year" | "Year" %in% groupVariable) | length(secondGroup) == 1),
        shareX = TRUE, nrows = length(secondGroup)))
    
    extraVars <- c("Antwoord", groupVariable)
    
  } else if (xVar == "percentage") {
    
    # rename y-axis ticktext
    yLabels <- unique(data[[yVar]])
    yLabels[yLabels == "populatie_evolutie"] <- "Populatie everzwijnen"
    yLabels[yLabels == "schade_landbouw_evolutie"] <- "Schade aan de landbouw"
    yLabels[yLabels == "schade_privpub_evolutie"] <- "Schade aan privéterreinen"
    yLabels[yLabels == "schade_verkeer_evolutie"] <- "Schade in het verkeer"
    
    myPlot <- plot_ly(data, x = ~get(xVar), y = ~get(yVar), 
        type = 'bar', color = ~Antwoord, colors = myColors, text = ~Antwoord,
        width = width, height = height
#            hovertemplate = paste('<b>Percentage</b>: %{x:.2f}', '<br>',
#                '<b>Antwoord</b>: %{text}')
      ) %>%
      layout(
        legend = list(title = list(text = "<b> Antwoord </b>"), traceorder = 'normal'),
        barmode = "stack",
        yaxis = list(title = yVar,
          ticktext = as.list(yLabels), 
          tickvals = as.list((length(yLabels)-1):0),
          tickmode = "array"),
        xaxis = list(title = paste0("Percentage \n (n = ", totalCounts, ")")) 
      )
    
    extraVars <- c("Antwoord")
    
  } else {
    
    colors <- replicateColors(nColors = length(unique(data$Type)))$colors
    names(colors) <- unique(data$Type)
    
    myPlot <- plot_ly(data, x = as.character(data[[xVar]]), 
        y = data[[yVar]], type = 'bar',
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
      
      subData <- reactive({
          
          if (!is.null(input$subGroup)) {
            
            switch(input$subGroup,
              "stakeholders" = data()[data()[[groupVariable[1]]] %in% c('Jagers', 'Landbouwers', 'Natuurvereniging'), ],
              "public" = data()[data()[[groupVariable[1]]] %in% c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied'), ]
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



