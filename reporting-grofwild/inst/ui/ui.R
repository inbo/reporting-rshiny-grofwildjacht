shinyUI(
        
        bootstrapPage(
                
                ## For debugging
                uiOutput("debug"),
                
                
                ## Header
                ## ------
                
                tags$head(
                        tags$meta(charset = "utf-8"),
                        tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
                        tags$link(rel = "stylesheet",
                                href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
                                integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
                                crossorigin="anonymous"),
                        tags$link(rel = "stylesheet", href = "style.css")
                ),
                
                
                
                ## Body
                ## ------
                
                tags$body(
                        
                        tags$div(class = "navbar1", 
                                
                                navbarPage(title = list(
                                        tags$div(paste0("v", packageVersion("reportingGrofwild")),
                                                HTML("&emsp;"),
                                                img(src = "logo.png", float = "top", height = "45px"),
                                                style = "margin-top: -13px")
                                ),
                                windowTitle = paste0("INBO v", packageVersion("reportingGrofwild")),
                                fluid = FALSE, id = "tabs",
                                position = "fixed-top",
                                theme = "navbar.css",
                                
                                # Main content
                                tabPanel(title = "Grofwild", id = "tab-grofwild",
                                        uiOutput("grof_content")),
                                tabPanel(title = "Wildschade", id = "tab-wildschade",
                                        uiOutput("schade_content")),
                                tabPanel(title = "Exoten", id = "tab-exoten",
                                        uiOutput("exoten_content")),
                                
                                # Shape data source + contact e-mail
                                header = tags$header(tags$div(align = "right",
                                                tags$p(
                                                        tags$a(href="http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen#", target="_blank", "Geodata bron"),
                                                        "-", 
                                                        tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie", target="_blank", "Contact")
                                                ), style = "margin-top: 55px")
                                )
                        )
        
                )
                
                )
        
        
        ## Footer
        ## ------                        
        
#                        tags$footer(tags$div(class="container", tags$img(src="logo.png")))
        
        )

)
