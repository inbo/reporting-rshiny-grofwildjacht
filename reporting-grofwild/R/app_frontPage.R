#' UI with front page of the application
#' @inherit shiny::fluidPage return
#' @inheritParams reportingGrofwild-common-args
#' @author lcougnaud
#' @importFrom slickR slickROutput
#' @import shiny
#' @export
frontUI <- function(speciesList){

  img <- system.file(
      "ui", "www", paste0("carousel-", 1:4, ".png"), 
      package = "reportingGrofwild"
  )
  
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
          "Welkom op de pagina faunabeheer van het",
          br(),
          "Instituut voor Natuur- en Bosonderzoek (INBO)"
        ),
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
  )

}
