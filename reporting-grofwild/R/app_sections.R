# TODO: Add comment
# 
# Author: mvarewyck
###############################################################################



#' Section for welcoming (top of the page)
#' @param id character, from which page this function is called
#' e.g. 'wbe'
#' @param maxDate date, the last observation date to be replaced in the text
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
welcomeSection <- function(id, uiText, maxDate = NA) {
  
  description <- uiText[uiText$plotFunction == as.character(match.call())[1], id]
  
  # Replace last date
  if (!is.na(maxDate))
    description <- gsub("\\{\\{maxDate\\}\\}", format(maxDate, "%d/%m/%Y"), description)
  
  tags$div(style = "margin-bottom:20px;",
    HTML(description)
  )

}



#' Section title and text for bio-indicator - server side
#' @inheritParams bioindicatorSection
#' @param wildsoort reactive, selected wildsoort in the app
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
bioindicatorSectionServer <- function(id, uiText, wildsoort) {
  
  moduleServer(id,
    function(input, output, session) {
      
      oldText <- uiText[uiText$plotFunction == "bioindicatorSection", id]
      
      output$textBioindicator <- renderUI({
          
          newText <- strsplit(oldText, split = "\\{")[[1]]
          toRetain <- sapply(newText, function(x)
              if (grepl("\\}", x)) {
                if (grepl(wildsoort(), strsplit(x, "\\}")[[1]][1]))
                  strsplit(x, "\\}")[[1]][2] else
                  ""
              } else {
                x
              } 
            )
          HTML(trimws(paste(toRetain, collapse = "")))
          
        })
      
    })
}
  

#' Section title and text for bio-indicator
#' 
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
bioindicatorSection <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    h2(HTML(uiText$title)),
    tags$p(uiOutput(ns("textBioindicator")))
  )

}
