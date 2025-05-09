#' UI with front page of the application
#' @inherit shiny::fluidPage return
#' @inheritParams reportingGrofwild-common-args
#' @author lcougnaud
#' @importFrom slickR slickROutput
#' @import shiny
#' @export
frontUI <- function(speciesList, uiText){

  img <- list.files(system.file("ui", "www", package = "reportingGrofwild"), 
    pattern = "carousel", full.names = TRUE)
  
  bootstrapPage(
    slickR::slickR(
        obj = img, slideId = "carousel", 
        slideType = 'img',
        width = "100vw", padding = 0
      ) + 
      slickR::settings(dots = TRUE, arrows = FALSE, autoplay = TRUE),
    fluidRow(style = "margin-top: -50vh",
      column(width = 6, offset = 3, align = "center",
        tags$span(
          style = "font-weight: bold;text-align: center;color: white;font-size: 2.1em;",
          HTML(getOutputTitle(output = "frontpage", uiText = uiText)),
        tags$div(style = "margin-top: 10vh;",
          selectInput(
            inputId = "specie", 
            label = tags$span(
              style = "color: white; font-size: 1.5em", 
              "Selecteer een diersoort:"
            ),
            choices = c("", speciesList),
            width = "100%"
          )
        ),
        
        tags$div(class = "frontbox", 
          HTML(getOutputDescription(output = "frontpage", uiText = uiText, context = "description")))
        )
      )
    )
  )

}
