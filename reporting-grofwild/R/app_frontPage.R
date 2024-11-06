#' UI with front page of the application
#' @inherit shiny::fluidPage return
#' @author lcougnaud
#' @importFrom slickR slickROutput
#' @import shiny
#' @export
frontUI <- function(){

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
        height = "100vh;"
      ) + 
      slickR::settings(dots = TRUE, arrows = FALSE, autoplay = TRUE)
    ),
    headerUI(style = "margin-top: -30em", color = "white", offset = 9),
    fluidRow(style = "margin-top: 1em",
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
    fluidRow(style = "margin-top: 19em", 
      column(
        width = 6, offset = 3, align = "center",
        selectInput(
          inputId = "wildsoort", 
          label = tags$span(
            style = "color: white; font-size: 1.5em", 
            "Selecteer een diersoort:"
          ),
          choices = c("", schadeWildsoorten),
          width = "100%"
        )
      )
    )
  )

}
