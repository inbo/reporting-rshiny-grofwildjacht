# Specific app functions for the dashboard page
# 
# Author: mvarewyck
###############################################################################




#' Create named choices for the dashboard indicators
#' @param choices character vector, plotFunction names in \code{uiText} 
#' for which to create named choices 
#' @param uiText data.frame as loaded by \code{read.csv(file = file.path(dataDir, "uiText.csv"))}
#' @return named character vector
#' 
#' @author mvarewyck
#' @export
namedChoices <- function(choices, uiText, regionLevel) {
  
  matchId <- sapply(choices, function(x) {
      matchId <- which(uiText$plotFunction == x)
      if (uiText[matchId, regionLevel] == 0)
        NULL else
        matchId
    })
  choices <- choices[!sapply(matchId, is.null)]
  matchId <- as.integer(matchId[!sapply(matchId, is.null)])
  names(choices) <- paste(uiText$title[matchId], 
    ifelse(uiText[matchId, regionLevel] == 1, "*", ""))
  
  choices
  
}


#' List choices for indicatoren - UI side
#' @param id character, unique identifier for module
#' @param choices character vector, indicator choices to list, i.e. F_* codes
#' @param selected character vector, subset of \code{choices} pre-selected choices
#' @param uiText data.frame 
#' @return named character vector
#' 
#' @author mvarewyck
#' @import shiny
#' @export
dashboardChoices <- function(id, choices, selected = NULL, uiText) {
  
  ns <- NS(id)
  
  label <- unique(uiText$Criterium[uiText$plotFunction %in% choices])
  
  checkboxGroupInput(inputId = ns("indicators"),
    label = label,
    choices = namedChoices(choices, uiText = uiText, regionLevel = "provinces"),
    selected = selected,
    width = "100%"
  )
  
}


#' List choices for indicatoren - server side
#' @inheritParams dashboardChoices
#' @param regionLevel reactive object, selected region level
#' @return selected choices by user
#' 
#' @author mvarewyck
#' @import shiny
#' @export
dashboardChoicesServer <- function(id, choices, uiText, regionLevel) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observe({
          
          req(regionLevel())
          
          previousSelected <- input$indicators
          
          updateCheckboxGroupInput(session = session, inputId = "indicators",
            choices = namedChoices(choices, uiText = uiText, regionLevel = regionLevel()),
            selected = previousSelected)
        
        })
      
      return(reactive(input$indicators))
      
    })
  
}