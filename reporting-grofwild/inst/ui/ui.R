
shinyUI(
    
    bootstrapPage(
        
        ## For debugging
#        h5(actionLink(inputId = "debug_console", label = "Connect with console"),
#            align = "left"),
#        verbatimTextOutput("print"),
        
        ## Header
        ## ------
        
        tags$head(
            tags$meta(charset = "utf-8"),
            tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
            tags$title("Grofwildjacht"),
            tags$link(rel = "stylesheet", 
                href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
                integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
                crossorigin="anonymous"),
            tags$link(rel = "stylesheet",
                href = "style.css")
        ),
        
        
        
        ## Body
        ## ------
        
        # Shape data source + contact e-mail
        tags$body(
            
            tags$nav(class = "navbar navbar-default",
                tags$div(class="container",
                    tags$ul(class="nav navbar-nav navbar-right",
                        tags$li(tags$a(
                                href="http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen#", target="_blank", "Geodata bron")),
                        tags$li(tags$a(href="mailto:frank.huysentruyt@inbo.be,thomas.scheppers@inbo.be?SUBJECT=Grofwildjacht web applicatie", "Contact"))
                    )
                )
            ),
            
            tags$div(class = "container", 
                
                h1("Welkom op de informatiepagina rond grofwildjacht van het Instituut voor Natuur- en Bosonderzoek"),
                
                tags$p(class = "lead", "Op deze pagina kunt u de afschotgegevens voor elk van de vier bejaagde grofwildsoorten binnen het Vlaams gewest van de laatste jaren raadplegen."),
                
                tags$p("De bron voor de kaarten, figuren en tabellen wordt gevormd door het", 
                    tags$a(href="https://www.natuurenbos.be/e-loket", "E-loket van het Agentschap voor Natuur en Bos"),
                    "gecombineerd met door het INBO uitgevoerde metingen op ingezamelde stalen (onderkaken en baarmoeders)."),
                
                tags$p("Bovenaan deze pagina krijgt u, in functie van de gekozen jachtwildsoort, de regio-schaal en de gewenste jaren en periodes,", tags$br(),"een kaart met de geografische spreiding van het afschot en een figuur met de evolutie van het afschot de laatste jaren.",
                    "Daaronder kunnen verdere grafieken worden aangeklikt, die telkens kunnen worden aangepast naargelang de informatie die u zelf wenst te visualiseren."),
                
                tags$p(paste0("Onderstaande tabellen en figuren zijn gebaseerd op de beschikbare gegevens op ", format(as.Date(attr(ecoData, "Date")), "%d/%m/%Y"), "."))
            
            ),
            
            tags$div(class = "container", 
                
                tags$div(class = "row",
                    tags$div(class="col-xs-6 col-md-3",
                        tags$a(href="#", class="thumbnail action-button shiny-bound-input",
                            id = "showSpecies1",
                            tags$img(src="wildZwijn.jpeg", title="Foto: Vildaphoto"),
                            tags$div(class="caption", "Wild zwijn")
                        )
                    ),
                    tags$div(class="col-xs-6 col-md-3",
                        tags$a(href="#", class="thumbnail action-button shiny-bound-input",
                            id = "showSpecies2",
                            tags$img(src="ree.jpeg", title="Foto: Vildaphoto"),
                            tags$div(class="caption", "Ree"))
                    ),
                    tags$div(class="col-xs-6 col-md-3",
                        tags$a(href="#", class="thumbnail action-button shiny-bound-input",
                            id = "showSpecies3",
                            tags$img(src="damhert.jpeg", title="Foto: Vildaphoto"),
                            tags$div(class="caption", "Damhert"))
                    
                    ),
                    tags$div(class="col-xs-6 col-md-3",
                        tags$a(href="#", class="thumbnail action-button shiny-bound-input",
                            id = "showSpecies4",
                            tags$img(src="edelhert.jpeg", title="Foto: Vildaphoto"),
                            tags$div(class="caption", "Edelhert"))
                    )
                )
            
            ),
            
            
#        fixedRow(column = 12, align = "center",
#            radioButtons(inputId = "showSpecies", label = "", inline = TRUE,
#                choiceValues = list("Wild zwijn", "Ree", "Damhert", "Edelhert"),
#                choiceNames = list(HTML("<b>Wild zwijn</b><br><br><img src='wildZwijn.jpeg' width = '400px' title = 'Foto: Vildaphoto'><br>"),
#                    HTML("<b>Ree</b><br><br><img src='ree.jpeg' width = '400px' title = 'Foto: Vildaphoto'><br>"),
#                    HTML("<b>Damhert</b><br><br><img src='damhert.jpeg' width = '400px' title = 'Foto: Vildaphoto'><br>"),
#                    HTML("<b>Edelhert</b><br><br><img src='edelhert.jpeg' width = '400px' title = 'Foto: Vildaphoto'><br>"))
#            )
#        ),
            
            
            
            # Map with according line plot
            
            tags$div(class = "container", 
                
                h2("Landkaart"),
                
                ## countMap: all species
                wellPanel(
                    fixedRow(
                        column(4, selectInput(inputId = "map_regionLevel", label = "Regio-schaal",
                                choices = c("Vlaanderen" = "flanders",
                                    "Provincie" = "provinces", "Gemeente" = "communes"),
                                selected = "communes")),
                        column(8, uiOutput("map_region"))
                    ),
                    
                    fixedRow(
                        column(6,
                            uiOutput("map_year"),
                            selectInput(inputId = "map_legend", "Legende (kaart)",
                                choices = c("Bovenaan rechts" = "topright",
                                    "Onderaan rechts" = "bottomright",
                                    "Bovenaan links" = "topleft",
                                    "Onderaan links" = "bottomleft",
                                    "<geen>" = "none"))
                        ),
                        column(6, uiOutput("map_time"))
                    ),
                    
                    actionLink(inputId = "map_globe", label = "Voeg landkaart toe",
                        icon = icon("globe"))
                
                ),
                
                
                uiOutput("map_title"),
                leafletOutput("map_spacePlot"),
                tags$br(),
                downloadButton("map_download", "Download figuur"),
                downloadButton("map_downloadData", "Download data"),
                
                fixedRow(
                    column(6, 
                        h3("Referentie (Vlaanderen)"),
                        plotlyOutput("map_timePlotFlanders", height = "400px")
                    ),
                    column(6,
                        uiOutput("map_timeTitle"),
                        plotlyOutput("map_timePlot", height = "400px")
                    )
                
                )
            
            ),
            
            tags$hr(),
            
            
            # Show user input module per plot
            
            tags$div(class = "container", 
                
                h2("Extra Figuren en Tabellen"),
                
                ## tableProvince for "leeftijd": wild zwijn and ree
                uiOutput("showTableProvince"),
                
                ## tableProvince for "type": ree
                uiOutput("showTableProvince2"),
                
                
                ## countYearProvince: all species
                actionLink(inputId = "linkPlot1",
                    label = h3("FIGUUR: Gerapporteerd aantal per jaar en per regio")),
                conditionalPanel("input.linkPlot1 % 2 == 1",
                    fixedRow(
                        
                        column(4,
                            optionsModuleUI(id = "plot1", showTime = TRUE, exportData = TRUE),
                            tags$p("Het gerapporteerd aantal geschoten en dood gevonden dieren per jaar in de verschillende provincies voor de geselecteerde periode (cijfers geven het totaal aantal voor dat jaar weer).")
                        ),
                        column(8, plotModuleUI(id = "plot1"))
                    
                    ),
                    tags$hr()
                ),
                
                
                ## countAgeCheek & countYearAge & percentageYearlyShotAnimals
                ## countAgeGender & boxAgeWeight
                ## - Wild zwijn and Ree
                uiOutput("showPlotsWild_Ree"),
                
                
                ## boxAgeGenderLowerJaw
                ## - Ree
                uiOutput("showPlotsRee")
            
            ),
            
            ## Footer
            ## ------
            
            
            tags$footer(tags$div(class="container", tags$img(src="logo.png")))
        
        )
    )
)
