#' Get category card for a specific output
#' @param id id character, module id/specie
#' @param output character, output name, e.g. 'trendYearRegionUI'
#' @param category character, category for the card 
#' (e.g.'afschot' or 'schade'), used to extract
#' the picture for the card
#' @inheritParams getOutputTitle
#' @inherit bslib::card return
#' @author lcougnaud
#' @importFrom bslib card card_header card_image card_body card_footer
#' @importFrom shiny actionButton
categoryCard <- function(id, 
    uiText, output, outputFunction = output, 
    category, specie, type = category){
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = outputFunction, specie = specie, 
      uiText = uiText, type = type)
  
  description <- getOutputDescription(
      output = outputFunction, specie = specie,
      uiText = uiText, type = type)
  
  file <- system.file("ui", "www", 
      paste0("category-", category, "-", output, ".png"), 
      package = "reportingGrofwild"
  )
  
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