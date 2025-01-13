#' UI with front page of the application
#' @inherit shiny::fluidPage return
#' @author lcougnaud
#' @importFrom slickR slickROutput
#' @import shiny
#' @export
frontUI <- function(speciesList){

  img <- system.file(
      "ui", "www", paste0("carousel-", 1:4, ".png"), 
      package = "reportingGrofwild"
  )
  
  fluidPage(
    fluidRow(
      slickR::slickR(
        obj = img, slideId = "carousel", 
        slideType = 'img',
        width = "100%", padding = 0,
        height = "95vh;"
      ) + 
      slickR::settings(dots = TRUE, arrows = FALSE, autoplay = TRUE)
    ),
    fluidRow(style = "margin-top: -25em",
      column(
        width = 6, offset = 3, align = "center",
        tags$span(
          style = "font-weight: bold;text-align: center;color: white;font-size: 2.1em",
          "Welkom op de faunabeheer pagina van het",
          br(),
          "Instituut voor Natuur- en Bosonderzoek (INBO)"
        )
      ), 
    ),
    fluidRow(style = "margin-top: 20em", 
      column(
        width = 6, offset = 3, align = "center",
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

}
