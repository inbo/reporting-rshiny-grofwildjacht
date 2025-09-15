

#' Function to generate bubble plots for Maatschappelijk draagvlak (F14_1 -- F14_5)
#' 
#' @param data data.frame 
#' @param yVar character, column in \code{plotData} for y-axis
#' 
#' @return ggplot object
#' 
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_y_continuous facet_wrap coord_fixed geom_vline scale_fill_manual scale_color_manual theme_bw theme element_text
#' @importFrom ggforce geom_circle
#' @importFrom INBOtheme inbo_palette
#' @importFrom grid unit
#' @export 
pciDraagvlak <- function(data, yVar = c("Year", "vraag_label")) {
  
  # For R CMD check
  meanAnswer <- pci2 <- Sector <- NULL
  
  yVar <- match.arg(yVar)
  otherVar <- switch(yVar,
    Year = "vraag_label",
    vraag_label = "Year")
  
  xTicks <- switch(data$Antwoord[[1]],
    Wenselijkheid = c("Niet wenselijk", "Wel wenselijk"),
    Impact_grootte = c("Lage impact", "Hoge impact"),
    Belang = c("Niet belangrijk", "Wel belangrijk"))
  
  # Default params
  schaalfactor <- 1
  
  # Custom colors
  inboColors <- inbo_palette()
  colorValues <- c(
    `Binnen everzwijngebied` = inboColors[1],
    `Buiten everzwijngebied` = inboColors[2],
    Landbouwsector = inboColors[3],
    Jachtsector = inboColors[6],
    Natuursector = inboColors[9]    
  )
  
  # Drop levels so that unavailable levels do not take place within the graph 
  data[, yVar] <- droplevels(data[, yVar])
  
  # Plot
  myPlot <- ggplot(data) + 
    geom_circle(aes(x0 = meanAnswer, 
        y0 = as.numeric(data[,yVar])/schaalfactor, 
        r = pci2, colour = Sector, fill = Sector),
      alpha = 0.75) +
    scale_x_continuous(name = data$Antwoord, limits = c(-2,2), breaks = c(-2, 2), labels = c(xTicks[1], xTicks[2])) + 
    scale_y_continuous(name = "", 
      breaks = unique(as.numeric(data[,yVar])/schaalfactor),
      labels = unique(data[,yVar])) + 
    facet_wrap(otherVar, 
      ncol = if (otherVar == "Year") 
          length(unique(data[,yVar])) else
          1) +
    coord_fixed() +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    scale_fill_manual(name = "", values = colorValues) + 
    scale_color_manual(name = "", values = colorValues) + 
    theme_bw() + 
    theme(
      axis.text.x = if (otherVar == "Year" && length(unique(data[,otherVar])) > 1) {
          element_text(angle = 45, hjust = 1, vjust = 1)
        } else {
          element_text()
        },
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.5, "cm")
    )
  
  list(
    plot = myPlot,
    data = data
  )
  
}



#' Shiny module for creating the plot \code{\link{pciDraagvlak}} or
#' \code{\link{barDraagkracht}} - server side
#' @param data reactive data.frame for the plot function
#' @param plotFunction character, function to be called to the data
#' @inheritParams pciDraagvlak
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @export
pciDraagvlakServer <- function(id, data, yVar = NULL, plotFunction) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subData <- reactive({
          
          validate(need(nrow(data()) > 0, "Geen data beschikbaar"))
          req(input$year)
          
          toReturn <- data()
          
          # filter year
          toReturn <- toReturn[toReturn$Year %in% input$year &
              toReturn$Sector %in% c(input$sector1, input$sector2), ]
          
          if (!is.null(input$groups))
            toReturn <- toReturn[toReturn$vraag_label %in% input$groups, ]
          
          
          toReturn
          
        })
      
      
      callModule(module = optionsModuleServer, id = "pciDraagvlak", 
        data = subData
      )
      callModule(module = plotModuleServer, id = "pciDraagvlak",
        plotFunction = plotFunction, 
        data = subData,
        yVar = yVar,
        exportPlotWidth = 12, exportPlotHeight = ifelse(length(input$year) > 1, 10, 12),
      )
      
    })
  
}


#' Shiny module for creating the plot \code{\link{pciDraagvlak}} or
#' \code{\link{barDraagkracht}} - UI side
#' @inherit welcomeSectionUI
#' @param yearChoices character vector, choices for variable 'year'
#' @param sectorChoices list with character vectors, choices for variable 'sector'
#' @param groupChoices character vector, choices for variable 'group'; 
#' default is NULL, then widget is hidden
#' @param groupLabel character, label for the grouping variable choices
#' @param outputFunction character, identifier for matching the plot's title and 
#' description in \code{uiText}
#'
#' @export
pciDraagvlakUI <- function(id, uiText, yearChoices, sectorChoices, 
  groupChoices = NULL, groupLabel = "", outputFunction, doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = outputFunction, uiText = uiText)
  description <- getOutputDescription(output = outputFunction, uiText = uiText,
    context = "description")
  sectorNames <- names(sectorChoices)
  
  tagList(
    
    actionLink(inputId = ns("linkPciDraagvlak"), label = h3(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkPciDraagvlak % 2 ==", 
        as.numeric(doHide)),
      ns = ns,
      wellPanel(
        fluidRow(
          column(3,
            checkboxGroupInput(inputId = ns("year"), label = "Jaartallen", 
              choices = yearChoices, selected = yearChoices, inline = TRUE),
            lapply(seq_along(sectorNames), function(i)
                selectInput(inputId = ns(paste0("sector", i)), label = sectorNames[i], 
                  choices = sectorChoices[[i]], selected = sectorChoices[[i]], 
                  multiple = TRUE))
          ),
          if (!is.null(groupChoices))
            column(9, tags$div(class = "columns-3", 
                checkboxGroupInput(inputId = ns("groups"), 
                  label = groupLabel, choices = groupChoices, 
                  selected = groupChoices, width = "100%"))
            )
        
        )
      ),
      
      tags$p(HTML(description)),
      
      plotModuleUI(id = ns("pciDraagvlak")),
      optionsModuleUI(id = ns("pciDraagvlak"), 
        exportPlot = (outputFunction != "F14_2"), 
        exportData = TRUE, doWellPanel = FALSE),
      tags$hr()
    
    )
  )
  
}



