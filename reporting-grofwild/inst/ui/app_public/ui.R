shinyUI(
        
  bootstrapPage(
                
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = js_code, functions = 'browseURL'),
                
    ## For debugging
    uiOutput("debug"),
                
    ## Header
    ## ------
                
    tags$head(
#    tags$meta(charset = "utf-8"),
#    tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
      tags$link(rel = "stylesheet",
          href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
          integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
          crossorigin="anonymous"
      ),
      tags$link(rel = "stylesheet", href = "www/style.css")
    ),
                
    ## Body
    ## ------
                
    tags$body(

        fluidPage(
          fluidRow(
            slickR::slickROutput("carouselOutput", 
              width = '100%', height = 'auto')
          ),
          fluidRow(style = "margin-top: -35em", 
            column(
              width = 1, offset = 7,
              tags$p(
                tags$a(
                  id = "contact", 
                  href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", 
                  target="_blank", "Contact",
                  style = "color: white"
                )
              )
            ),
            column(width = 1, versionUI(id = "public"), style = "color: white")
          ),
          fluidRow(style = "margin-top:1em",
            column(
              width = 6, offset = 3, align = "center",
                tags$span(
                  style = "font-weight: bold;text-align: center;color: white;font-size: 1.7em",
                  "Welkom op de faunabeheer pagina van het Instituut",
                   br(),
                  "voor Natuur- en Bosonderzoek (INBO)"
                )
            ), 
          ),
          fluidRow(style = "margin-top: 15em", 
            column(
              width = 4, offset = 4, align = "center",
              selectInput(
                inputId = "wildsoort", 
                label = tags$span(
                 style = "color: white;", "Selecteer een diersoort:"
                ),
               choices = schadeWildsoorten
              )
            )
          )
        )
      )

    )

)
