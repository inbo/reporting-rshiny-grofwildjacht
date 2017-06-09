# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' User input for controlling specific plot (ui-side)
#' @param id character, module id, unique name per plot
#' @param title character, title for the control panel
#' @param showLegend boolean, whether to show input field for the legend
#' @param showExtraVariables boolean, whether to show input field for 
#' extra variables (popup)
#' @param showTime boolean, whether to show input field for time range
#' @param showRegion boolean, whether to show input fields for region selection
#' @param showGlobe boolean, whether to show input field for map of the globe
#' @return ui object (tagList)
#' @export
figureModuleUI <- function(id, title, 
    showLegend = TRUE, showExtraVariables = TRUE, showTime = TRUE, 
    showRegion = TRUE, showGlobe = TRUE) {
  
  ns <- NS(id)
  
  tagList(
      
      h4(title),
      
      wellPanel(
          if (showLegend)
            selectInput(inputId = "legend", "Legende",
                choices = c("<none>" = "none", 
                    "Bovenaan rechts" = "topright", 
                    "Onderaan rechts" = "bottomright", 
                    "Bovenaan links" = "topleft",
                    "Onderaan links" = "bottomleft")
            ),
          if (showExtraVariables)
            uiOutput(ns("extraVariables")),
          if (showTime)
            uiOutput(ns("showTime")),
          if (showRegion)
            list(selectInput(inputId = "regionLevel", label = "Regio-schaal",
                    choices = c("Vlaanderen" = "flanders", "Provincie" = "provinces", "Gemeente" = "communes")),
                uiOutput(ns("region"))
            ),
          if (showGlobe)
            actionLink(inputId = "globe", label = "Voeg landkaart toe",
                icon = icon("globe"))
      
      )
  )
  
}

#' User input for controlling specific plot (server-side)
#' @param input shiny input variable
#' @param output shiny output variable
#' @param session shiny session variable
#' @param data reactive data.frame, data for specific plot
#' @return no return value; some output objects are created
#' @export
figureModuleServer <- function(input, output, session, data) {
  
  output$extraVariables <- renderUI({
        
        selectInput(inputId = "extraVariables", "Extra variabelen in popup",
            choices = {
              vars <- names(data()); vars[!vars %in% c("counts", "year")]
            }, multiple = TRUE)
        
      })
  
  
  output$time <- renderUI({
        
        sliderInput("time", "Tijdstip(pen)", 
            value = c(min(data()$year), max(data()$year)),
            min = min(data()$year),
            max = max(data()$year),
            step = 1,
            sep = "")
        
      })
  
  
  output$region <- renderUI({
        
        selectInput("region", "Regio('s)",
#            choices = sort(data()$NAAM),
            choices = letters[1:4],
            multiple = TRUE)
        
      })
  
}
