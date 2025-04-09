

#' Function to generate bubble plots for Maatschappelijk draagvlak (F14_1 -- F14_5)
#' 
#' @param data data.frame 
#' @param yVar character, column in \code{plotData} for y-axis
#' 
#' @return ggplot object
#' 
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_y_continuous facet_wrap coord_fixed geom_vline scale_fill_manual scale_color_manual theme_bw
#' @importFrom ggforce geom_circle
#' @export 
pciDraagvlak <- function(data, yVar = c("Year", "vraag_label")) {
  
  # For R CMD check
  meanAnswer <- pci2 <- Sector <- NULL
  
  yVar <- match.arg(yVar)
  otherVar <- switch(yVar,
    Year = "vraag_label",
    vraag_label = "Year")
  
  
  # Default params
  schaalfactor <- 1
  
  # Custom colors
  colorValues <- c(
    Landbouwsector = "limegreen",
    Jachtsector = "olivedrab1",
    Natuursector = "darkgreen",
    `Binnen everzwijngebied` = "royalblue",
    `Buiten everzwijngebied` = "orange"
  )
  
  # Plot
  myPlot <- ggplot(data) + 
    geom_circle(aes(x0 = meanAnswer, 
        y0 = as.numeric(data[,yVar])/schaalfactor, 
        r = pci2, colour = Sector, fill = Sector),
      alpha = 0.75) +
    scale_x_continuous(name = data$Antwoord, limits = c(-2,2)) + 
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
    theme_bw()
  
  list(
    plot = myPlot,
    data = data
  )
  
}



#' Shiny module for creating the plot \code{\link{pciDraagvlak}} - server side
#' @param data reactive data.frame for the plot function
#' @inheritParams pciDraagvlak
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @export
pciDraagvlakServer <- function(id, data, yVar) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      subData <- reactive({
          
          validate(need(nrow(data()) > 0, "Geen data beschikbaar"))
          req(input$year)
          req(input$sector)
          
          toReturn <- data()
          
          # filter year
          toReturn <- toReturn[toReturn$Year %in% input$year &
              toReturn$Sector %in% input$sector, ]
          
          if (!is.null(input$groups))
            toReturn <- toReturn[toReturn$vraag_label %in% input$groups, ]
          
          
          toReturn
          
        })
      
      
      callModule(module = optionsModuleServer, id = "pciDraagvlak", 
        data = subData
      )
      callModule(module = plotModuleServer, id = "pciDraagvlak",
        plotFunction = "pciDraagvlak", 
        data = subData,
        yVar = yVar
      )
    
    })
  
}


#' Shiny module for creating the plot \code{\link{pciDraagvlak}} - UI side
#' @inherit welcomeSectionUI
#' @param yearChoices character vector, choices for variable 'year'
#' @param sectorChoices character vector, choices for variable 'sector'
#' @param groupChoices character vector, choices for variable 'group'; 
#' default is NULL, then widget is hidden
#' @param groupLabel character, label for the grouping variable choices
#' @param outputFunction character, identifier for matching the plot's title and 
#' description in \code{uiText}
#'
#' @export
pciDraagvlakUI <- function(id, uiText, yearChoices, sectorChoices, 
  groupChoices = NULL, groupLabel = "", outputFunction) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = outputFunction, uiText = uiText)
  description <- getOutputDescription(output = outputFunction, uiText = uiText,
    context = "description")
  
  tagList(
    
    h3(HTML(title)),
      
      wellPanel(
        fluidRow(
          column(3,
            checkboxGroupInput(inputId = ns("year"), label = "Jaartallen", 
              choices = yearChoices, selected = yearChoices, inline = TRUE),
            selectInput(inputId = ns("sector"), label = NULL, 
              choices = sectorChoices, selected = sectorChoices, 
              multiple = TRUE)
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
      optionsModuleUI(id = ns("pciDraagvlak"), exportData = TRUE,
        doWellPanel = FALSE),
      tags$hr()
    
  )
  
}



