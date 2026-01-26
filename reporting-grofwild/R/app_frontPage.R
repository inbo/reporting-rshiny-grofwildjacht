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
    tags$head(
      tags$style(HTML("
            #specie + .selectize-control .selectize-dropdown .optgroup-header {
            font-size: 20px;
            line-height: 1.428571429;
            }
            "))
    ),
    slickR::slickR(
        obj = img, slideId = "carousel", 
        slideType = 'img',
        width = "100%", padding = 0
      ) + 
      slickR::settings(dots = TRUE, arrows = FALSE, autoplay = TRUE),
    fluidRow(style = "margin-top: -50vh; width: 100%;",
      column(width = 6, offset = 3, align = "center", style = "margin-top: 100px;",
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
        )
      )
    )
  ),
  tags$footer(class = "bottom-banner",
    tags$a(href = "https://www.inbo.be", target = "_blank", 
      tags$img(src = "www/logo-inbo.png", style = "height:40px;")),
    tags$a(href = "https://www.natuurenbos.be", target = "_blank", 
      tags$img(src = "www/logo-anb.png", style = "height:30px;"))
  )
)

}
