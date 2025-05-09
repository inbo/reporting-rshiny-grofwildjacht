#' UI with front page of the application
#' @inherit shiny::fluidPage return
#' @inheritParams reportingGrofwild-common-args
#' @author lcougnaud
#' @importFrom slickR slickROutput
#' @import shiny
#' @export
frontUI <- function(speciesList){

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
          "Welkom op de faunabeheerpagina van het",
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
        ),
        
        tags$div(class = "frontbox", 
          HTML(paste("Deze pagina is tot stand gekomen in samenwerking met het", 
            "<a href='https://www.natuurenbos.be' target='_blank'>Agentschap voor Natuur en Bos</a>,", 
            "en bundelt gegevens van het e-loket fauna en flora",
            "<a href='https://www.natuurenbos.be/e-loket' target='_blank'>(ANB)</a>,",
            "het INBO, Natuurpunt",
            "<a href='https://waarnemingen.be' target='_blank'>(waarnemingen.be)</a>,",
            "en Wilder",
            "<a href='https://hvv.be' target='_blank'>(Hubertus Vereniging Vlaanderen)</a>")
        ))
      )
    )
  )

}
