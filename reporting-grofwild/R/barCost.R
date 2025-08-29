# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate stacked bar plot for kost landbouwschade (F09_2)
#' 
#' @param data data.frame with schadeData
#' @param typeMelding character vector, choices for filtering on `typeMelding` in data
#' @param interval character, data shown in intervals
#' should be one of \code{c("Per jaar", "Per seizoen", "Per kwartaal", "Per twee weken")}
#' @inheritParams barDraagkracht
#' @inheritParams reportingGrofwild-common-args
#' @return list with plotly object and data.frame 
#' plot per year (xaxis) and group (color): freq x schadeBedrag (yaxis)
#' @author wverlinden
#' @import plotly
#' @importFrom stats aggregate
#' @export 
barCost <- function(data, 
  unit = NULL, yVar = c("schadeBedrag", "count"), 
  typeMelding = NULL, interval = c("Per jaar", "Per seizoen", "Per kwartaal", "Per twee weken"), 
  regio = "") {
  
  wildNaam <- unique(data$wildsoort)
  interval <- match.arg(interval)
  yVar <- match.arg(yVar)  
  
  yLabel <- switch(yVar,
    schadeBedrag = "Bedrag",
    count = "Aantal"
  )
  groupLabel <- if (!is.null(interval))
      switch(interval,
        "Per jaar" = "Jaar",
        "Per seizoen" = "Seizoen",
        "Per kwartaal" = "Kwartaal",
        "Per twee weken" = "Twee weken"
      ) else 
      NULL
  
  subData <- data[, c(if (yVar != "count") yVar, "season", "afschotjaar", "afschot_datum")] %>%
    mutate(season_num = dplyr::case_when(
        season == "winter" ~ 1,
        season == "lente"  ~ 2,
        season == "zomer"  ~ 3,
        season == "herfst"  ~ 4
      ))
  
  # Extract month/day
  subData$maand <- as.numeric(format(subData$afschot_datum, "%m"))
  subData$dag <- as.numeric(format(subData$afschot_datum, "%d"))
  
  if (interval == "Per jaar") {
    newLevels <- sort(unique(subData$afschotjaar))
  } else if (interval == "Per seizoen") {
    newLevels <- c("winter", "lente", "zomer", "herfst")
    subData$timeGroup <- subData$season_num
  } else if (interval == "Per kwartaal") {
    newLevels <- c("Kwartaal 1 (jan-mrt)", "Kwartaal 2 (apr-jun)", "Kwartaal 3 (jul-sept)", "Kwartaal 4 (okt-dec)")
    subData$timeGroup <- ceiling(subData$maand/3)
  } else if(interval == "Per twee weken") {
    subData$timeGroup <- (subData$maand-1)*2 + (subData$dag > 15) + 1 
    newLevels <- c(
      "01/01-15/01",
      "16/01-31/01",
      "01/02-15/02",
      "16/02-28/02 of 29/02",
      "01/03-15/03",
      "16/03-31/03",
      "01/04-15/04",
      "16/04-30/04",
      "01/05-15/05",
      "16/05-31/05",
      "01/06-15/06",
      "16/06-30/06",
      "01/07-15/07",
      "16/07-31/07",
      "01/08-15/08",
      "16/08-31/08",
      "01/09-15/09",
      "16/09-30/09",
      "01/10-15/10",
      "16/10-31/10",
      "01/11-15/11",
      "16/11-30/11",
      "01/12-15/12",
      "16/12-31/12")
  }
  
  if (interval == "Per jaar") {
    summaryData <- melt(table(subData[, c("afschotjaar", yVar)]), 
      id.vars = c("afschotjaar", yVar))
    summaryData$timeGroup <- as.numeric(as.factor(summaryData$afschotjaar))
  } else 
    summaryData <- melt(table(subData[, c("afschotjaar", "timeGroup", yVar)]), 
      id.vars = c("afschotjaar", "timeGroup", yVar))
  
  
  # For optimal displaying in the plot
  summaryData$timeChar <- factor(newLevels[summaryData$timeGroup], levels = newLevels)
  if (interval == "Per jaar")
    summaryData$timeChar <- as.numeric(as.character(summaryData$timeChar))
  
  summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
  summaryData[[yVar]] <- factor(summaryData[[yVar]], 
    levels = c("meer dan 3000 euro", "van 1000 tot 3000 euro", "van 300 tot 1000 euro",
      "minder dan 300 euro", "het schadebedrag is niet bekend"))
  
  selectedGroups <- unique(summaryData[[yVar]])
  colors <- replicateColors(values = selectedGroups)$colors
  
  # Create plot per year
  if (interval == "Per jaar") {
    allPlots <- plot_ly(data = summaryData,
        x = ~timeChar, y = ~value, type = "bar", 
        color = ~base::get(yVar), colors = colors) %>%
      plotly::layout(
        xaxis = list(title = '',
          tickvals = unique(summaryData$timeChar),
          ticktext = unique(summaryData$timeChar))
    )
  } else {
    allPlots <- lapply(seq_along(levels(summaryData$afschotjaar)), function(i) {
        iYear <- levels(summaryData$afschotjaar)[i]
        plot_ly(data = summaryData[summaryData$afschotjaar %in% iYear, ],
            x = ~timeChar, y = ~value,
            type = "bar", hoverinfo = 'x+y+text+name', 
            color = ~base::get(yVar), colors = colors,
            showlegend = i == 1) %>%
          plotly::layout(xaxis = list(title = "", showticklabels = FALSE)) %>%
          add_annotations(
            text = iYear,
            x = newLevels[round(length(newLevels)/2)], y = 0, xref = paste0("x", if (i != 1) i), yref = "paper", 
            yanchor = "top", textangle = 90, showarrow = FALSE)
      })
  }
  
  title <- paste0(
    yLabel, " kosten voor schadegevallen",
    if(!is.null(typeMelding)) paste(" over", toString(typeMelding)),
    paste(" van", tolower(wildNaam)),
    if(!is.null(groupLabel)) paste(" per", tolower(groupLabel)),
    if (!all(regio == "")) paste0("\n(", toString(regio), ")")
  )
  
  # Combine all plots
  pl <- subplot(allPlots, titleX = TRUE, shareY = TRUE, 
      margin = c(0.01, 0, 0, 0)) %>%
    plotly::layout(barmode = 'stack', showlegend = TRUE,
      title = title,
      yaxis = list(title = "Aantal"),
      margin = list(b = if (interval == "Per jaar") 120 else 150, t = 100))
  
  colnames(summaryData)[colnames(summaryData) == "timeChar"] <- gsub("Per ", "", interval) 
  colnames(summaryData)[colnames(summaryData) == "value"] <- "Aantal"
  
  return(list(plot = pl, data = summaryData[, c(if (interval != "Per jaar") "afschotjaar", gsub("Per ", "", interval) , yVar, "Aantal")]))
  
}




#' Shiny module for creating the plot \code{\link{barCost}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams barDraagkracht
#' @param title reactive, title to be printed above plot
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
barCostServer <- function(id, yVar, data, title = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observe({
          
          req(title())
          updateActionLink(session = session, inputId = "linkBarCost",
            label = paste("FIGUUR:", title()))
          
        })
      
      output$disclaimerBarCost <- renderUI({
          
          req(title())
          
          if (grepl("\\*", title()))
            getDisclaimerLimited()
          
        })      
      
      
      subData <- reactive({
          
          # Type melding
          plotData <- if (!is.null(input$typeMelding) && input$typeMelding != "all")
            data()[data()$typeMelding %in% input$typeMelding, ] else 
            data()
          
          # Bron
          filterDataSource(plotData = plotData,
            sourceIndicator = input$bron, returnStop = "message")
          
        })
      
      
      # Afschot per jaar en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "barCost", 
        data = subData,
        intervals = c("Per jaar", "Per seizoen", "Per kwartaal", "Per twee weken")
      )
      
      toReturn <- callModule(module = plotModuleServer, id = "barCost",
        plotFunction = "barCost", 
        data = subData,
        yVar = yVar,
        typeMelding = reactive(input$typeMelding)
      )
      
      
      return(reactive(c(
            toReturn(),
            isolate(reactiveValuesToList(input))
          )))
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{barCost}} - UI side
#' @inherit welcomeSectionUI
#' @inheritParams getOutputDescription
#' @inheritParams barCost
#' @inheritParams optionsModuleUI
#' @inheritParams reportingGrofwild-common-args
#' @export
barCostUI <- function(id, 
  uiText, context = strsplit(id, split = "_")[[1]][1], 
  specie = NULL,
  typeMelding = NULL, doHide = TRUE,
  regionLevels = NULL) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(
    output = "barCostUI", specie = specie, 
    uiText = uiText)
  description <- getOutputDescription(
    output = "barCostUI", 
    specie = specie, uiText = uiText, context = context
  )
  
  metaSchade <- loadMetaSchade()
  
  tagList(
    
    actionLink(inputId = ns("linkBarCost"), 
      label = title, class = "action-h3"),
    conditionalPanel(
      condition = 
        paste("input.linkBarCost % 2 ==", as.numeric(doHide)), 
      ns = ns,
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("barCost"))
        ),
          
        column(4,
          wellPanel(
            optionsModuleUI(
              id = ns("barCost"), 
              regionLevels = regionLevels, 
              doWellPanel = FALSE
            ),
            if (!is.null(typeMelding))
              selectInput(
                inputId = ns("typeMelding"), 
                label = "Type schade",
                choices = typeMelding
              ),
            selectInput(inputId = ns("bron"), label = "Databron(nen)",
              choices = metaSchade$sources,
              selected = metaSchade$sources,
              multiple = TRUE),
            optionsModuleUI(
              id = ns("barCost"),
              showInterval = TRUE,
              exportData = TRUE, 
              doWellPanel = FALSE
            )
          )
        )
      ),
      uiOutput(ns("disclaimerBarCost")),
      tags$p(HTML(description))
    )    
  )
  
  
}