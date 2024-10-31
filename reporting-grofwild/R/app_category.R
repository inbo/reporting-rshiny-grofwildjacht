#' Get title for a specific tab of the 'Category' page.
#' 
#' This is used for the title of the tab and the path.
#' @param value character of length 1 with 
#' \code{\link[shiny]{tabPanel}} value
#' @param category character of length 1 with the name of the
#' category page.
#' @return character of length 1 with 
#' \code{\link[shiny]{tabPanel}} title
#' @author lcougnaud
getTabTitle <- function(value, category){
  
  title <- switch(category,
   afschot = 
     switch(value,
       vlaanderen = "Afschot in Vlaanderen",
         regio =  "Afschot per regio",
         leeftijdcategorie = "Afschot per leeftijdscategorie",
         jachtmethode = "Afschot per jachtmethode"
       ),
  schade = 
    switch(value,
      informatie = "Informatie over schadegevallen",
      vlaanderen = "Schadegevallen in Vlaanderen",
      regio =  "Schadegevallen per regio",
      type = "Schadegevallen per type schade",
      seizoen = "Schadegevallen per seizoen",
      kosten = "Inschatting kosten"
    )
  )
  
  return(title)
  
}

#' Get output title
#' @param output character vector of length 1 with output name
#' @param specie character vector of length 1 with specie
#' @param n (optional) integer vector of length 1 with maximum 
#' number of characters to include
#' @param uiText data.frame with plot titles
#' @return character vector of length 1 with plot title
#' @author lcougnaud
getOutputTitle <- function(output, 
  uiText, specie = NULL, type = NULL, n = integer()){
  
  title <- uiText[uiText$plotFunction == output, "title"]
  
  if(!is.null(specie))
    title <- gsub("{wildsoort}", tolower(specie), title, fixed = TRUE)
  
  if(!is.null(type)){
    typeInfo <- switch(type, afschot = "afschot", schade = "schadegevallen")
    title <- gsub("{type}", typeInfo, title, fixed = TRUE)
  }
  
  if(length(n) > 0)
    title <- paste0(substr(x = title, start = 1, stop = n), "...")
  
  return(title)
  
}

#' Get output description
#' @inheritParams getOutputTitle
#' @return character vector of length 1 with output description
#' @author lcougnaud
getOutputDescription <- function(output, 
  uiText, specie = NULL, type = NULL, context = "fauna"){
  
  title <- uiText[uiText$plotFunction == output, context]
  
  if(!is.null(specie))
    title <- gsub("{wildsoort}", specie, title, fixed = TRUE)
  
  if(!is.null(type)){
    typeInfo <- switch(type, afschot = "afschot", schade = "schadegevallen")
    title <- gsub("{type}", typeInfo, title, fixed = TRUE)
  }
  
  return(title)
  
}

#' Get category card for a specific output
#' @param id id character, module id/specie
#' @param output character, output name, e.g. 'trendYearRegionUI'
#' @inherit bslib::card return
#' @author lcougnaud
#' @importFrom bslib card card_header card_image card_body card_footer
#' @importFrom shiny actionButton
categoryCard <- function(id, specie, output, uiText, type = NULL){
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = output, specie = specie, uiText = uiText, type = type)
  
  description <- getOutputDescription(output = output, specie = specie,
    uiText = uiText, type = type)
  
  file <- system.file("ui", "www", paste0("category-", type, "-", output, ".png"), 
    package = "reportingGrofwild")
  
  outputCard <- bslib::card(
      class = "category-card",
      bslib::card_header(title, class = "category-card-header"), 
      br(),
      bslib::card_image(file = file, class = "category-card-image"),
      br(),
      bslib::card_body(description),
      br(), br(),
      bslib::card_footer(
          align = "center",
          shiny::actionButton(
              inputId = ns(paste0(output, "-button")), 
              label = "Bekijk grafiek", class = "category-card-action-button"
          )
      )
  )
  
  return(outputCard)
  
}