# TODO: Add comment
# 
# Author: mvarewyck
###############################################################################



#' Section for welcoming - UI side (no server side)
#' @param split boolean (FALSE by default), should the output be 
#' returned separately, as a list
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @param ... Extra parameters for \code{\link{getOutputDescription}}
#' @return if \code{split} is FALSE, one single HTML object;
#' otherwise a list with 'title', 'summary' and 'description' 
#' as separate HTML objects
#' @author mvarewyck
#' @import shiny
#' @export
welcomeSectionUI <- function(id, 
  context = id, uiText, category, split = FALSE, ...) {

  outputFunction <- paste("informatie", category, sep = "-")
  
  title <- getOutputTitle(output = outputFunction, uiText = uiText)
  
  summary <- getOutputDescription(output = outputFunction, 
    uiText = uiText, context = "summary")
  
  description <- getOutputDescription(
    output = outputFunction, 
    uiText = uiText, context = context,
    ...
  )
  
  title <- tags$div(align = "center", h1(title))
  summary <- tags$div(style = "margin-bottom:20px;", HTML(summary))
  description <- HTML(description)
  
  result <- if(split){
    list(title = title, summary = summary, description = description)
  }else{tagList(title, summary, description)}
    
  return(result)

}


#' Decode species indicator in description
#' @param text character, input from uiText
#' @param statsMap character, statistics to be printed instead of \code{'{{statsMap}}'}
#' @inheritParams reportingGrofwild-common-args
#' @return character, modified for the conditional species mentioned in the text  
#' @author mvarewyck
#' @export
decodeText <- function(text, species, statsMap = NULL) {
  
  if (grepl("\\{\\{statsMap\\}\\}", text))
    newText <- gsub("\\{\\{statsMap\\}\\}", 
      if (!is.null(statsMap)) paste0(statsMap, ".") else "", text) else
    newText <- text
  
  
  splitText <- strsplit(newText, split = "\\{")[[1]]
  toRetain <- sapply(splitText, function(x)
      if (grepl("\\}", x)) {
        doInvert <- grepl("\\!", strsplit(x, "\\}")[[1]][1])
        doSpecies <- grepl(species, strsplit(x, "\\}")[[1]][1])
        if (!doInvert & doSpecies)
          strsplit(x, "\\}")[[1]][2] else if (doInvert & !doSpecies)
          strsplit(x, "\\}")[[1]][2] else 
          ""
      } else {
        x
      } 
  )
  
  paste(toRetain, collapse = "")
  
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
      
      output$textBioindicator <- renderUI({
          
          description <- getOutputDescription(output = "bioindicatorSection", 
            specie = wildsoort(), uiText = uiText, context = id)
          
          HTML(description)
          
        })
      
    })
}
  

#' Section title and text for bio-indicator
#' 
#' @inherit welcomeSectionUI 
#' @export
bioindicatorSection <- function(id, uiText) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = as.character(match.call())[1],
    uiText = uiText)
  
  
  tagList(
    h2(HTML(title)),
    tags$p(uiOutput(ns("textBioindicator")))
  )

}


#' Link with contact info - UI side
#' 
#' @inherit welcomeSectionUI
#' @export
contactUI <- function(id) {
  
  actionLink(inputId = NS(id, "contact"), 
    label = "Contact",
    class = "version")
  
}


#' Link with contact info - server side
#' @inherit welcomeSectionServer
#' @param currentURL character, url of current page/graph
#' @param subject character, subject of the mail
#' @export
contactServer <- function(id, uiText, currentURL = NULL, subject = "Faunabeheer web applicatie") {
  
  moduleServer(id,
    function(input, output, session) {
      
      observeEvent(input$contact, {
          
          mailto_link <- if (is.null(currentURL)) {
              paste0("mailto:faunabeheer@inbo.be?SUBJECT=", subject)
          } else {
            paste0("mailto:faunabeheer@inbo.be?SUBJECT=", subject, "&body=**Link**:%20faunabeheer.inbo.be/", 
              URLencode(currentURL, reserved = TRUE))
          }
          
          showModal(
            modalDialog(
              title = "Contact",
              footer = modalButton(label = NULL, icon = icon("xmark")),
              easyClose = TRUE,
              
              paste0(getOutputTitle(output = "contactApp", uiText = uiText), ":"),
              tags$a(href=mailto_link, target="_blank", "faunabeheer@inbo.be")              
            )
          )
          
        })
      
    })
}


#' Link with version info - UI side
#' 
#' @inherit welcomeSectionUI
#' @importFrom utils packageVersion
#' @export
versionUI <- function(id) {
  
  actionLink(inputId = NS(id, "version"), 
    label = paste0("v", packageVersion("reportingGrofwild")),
    class = "version")
  
}


#' Link with version info - server side
#' @inherit bioindicatorSectionServer
#' @importFrom utils packageVersion 
#' @export
versionServer <- function(id) {
  
  moduleServer(id,
    function(input, output, session) {
      
      observeEvent(input$version, {
          
          hashCode <- Sys.getenv("GIT_SHA")
          
          showModal(
            modalDialog(
              fluidPage(
                paste("R package:", packageVersion("reportingGrofwild")),
                tags$br(),
                "GIT:", if (hashCode == "") 
                  "Niet beschikbaar" else 
                  tags$a(id = "gitVersion", 
                  href = paste0("https://github.com/inbo/reporting-rshiny-grofwildjacht/commit/",hashCode), 
                  target = "_blank", hashCode)
              ), 
              title = "Versie",
              easyClose = TRUE
            ))
          
        })
    })
}
