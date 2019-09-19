shinyUI(
        
        bootstrapPage(
                
                ## For debugging
                uiOutput("debug"),
                                
                
                ## Header
                ## ------
                
                tags$head(
                        tags$meta(charset = "utf-8"),
                        tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
                        tags$title("INBO"),
                        tags$link(rel = "stylesheet",
                                href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
                                integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
                                crossorigin="anonymous"),
                        tags$link(rel = "stylesheet", href = "style.css")
                ),
                
                
                
                ## Body
                ## ------
                
                # Shape data source + contact e-mail
                tags$body(
                        
                        tags$nav(class = "navbar navbar-default", id = "tabs",
                                tags$div(class = "navbar-header",
                                        span(class = "navbar-brand", "INBO")
                                        ),
                                tags$div(class="container", 
                                        tags$ul(class="nav navbar-nav navbar-left",
                                                tags$li(HTML("<a href='#tab-grofwild', data-toggle='tab'>Grofwild</a>")),
                                                tags$li(HTML("<a href='#tab-wildschade', data-toggle='tab'>Wildschade</a>"))
                                        ),
                                        
                                        tags$ul(class="nav navbar-nav navbar-right",
                                                tags$li(tags$a(href="http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen#", target="_blank", "Geodata bron")),
                                                tags$li(tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie", target="_blank", "Contact"))
                                        )
                                )
                        ),
                        
                        # Main content
                        tags$div(class = "container",
                                
                                tags$div(class = "tab-content",
                                        
                                        tabPanel(title = "Grofwild", id = "tab-grofwild",
                                                uiOutput("grof_content")
                                        ),
                                        tabPanel(title = "Wildschade", id = "tab-wildschade",
                                                uiOutput("schade_content")
                                        )
                                
                                
                                )
                        ),
                        
                        
                        
                        ## Footer
                        ## ------                        
                        
                        tags$footer(tags$div(class="container", tags$img(src="logo.png")))
                
                )
        )

)
