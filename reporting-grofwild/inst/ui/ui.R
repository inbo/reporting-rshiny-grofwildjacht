library(leaflet)
library(plotly)

shinyUI(
    
    
    fluidPage(
        
        includeCSS("www/style.css"),
        
#        h5(actionLink(inputId = "debug_console", label =` "Connect with console"),
#            align = "right"),
        # Shape data source
        helpText(h5(a(href = "http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen#",
                    target = "_blank", "Geodata bron"), align = "right"),
            h5(a(href = "mailto:frank.huysentruyt@inbo.be,thomas.scheppers@inbo.be?SUBJECT=Grofwildjacht web applicatie", "Contact"), align = "right")
        ),
        
        
        
        
        
        titlePanel(title = div(img(src = "logo.png",
                    float = "top", height = "60px", hspace = "70px"),
                HTML("<nobr><font face = 'verdana', color = '#c04384'>Grofwildjacht in Vlaanderen</font></nobr>")),
            windowTitle = "Grofwildjacht"),
        
        
        
        tags$br(),
        tags$br(),
        
#        downloadButton(outputId = "exportResults", label = "Rapport"),
        
        fluidRow(div(class = "purple", 
                tags$br(), 
                h4("Welkom op de informatiepagina rond grofwildjacht van het Instituut voor Natuur- en Bosonderzoek"), 
                p(tags$br(),
                    "Op deze pagina kunt u de afschotgegevens voor elk van de vier bejaagde grofwildsoorten binnen het Vlaams gewest van de laatste jaren raadplegen",
                    tags$br(),
                    "Bovenaan deze pagina krijgt u, in functie van de gekozen jachtwildsoort en regio-schaal en de gewenste jaren en periodes, zowel een afschotkaart als een kaart met de evolutie van het afschot in de laatste jaren",
                    tags$br(), 
                    "Daaronder kunnen verdere grafieken worden aangeklikt, die telkens kunnen worden aangepast naargelang de informatie die u zelf wenst te visualiseren"),
                tags$br())),
        
        tags$br(),
        
        fluidRow(column = 12, align = "center",
            radioButtons(inputId = "showSpecies", label = "", inline = TRUE,
                choiceValues = list("Wild zwijn", "Ree", "Damhert", "Edelhert"),
                choiceNames = list(HTML("<b>Wild Zwijn</b><br><br><img src='wildZwijn.jpeg' width = '400px'><br>"),
                    HTML("<b>Ree</b><br><br><img src='ree.jpeg' width = '400px'><br>"),
                    HTML("<b>Damhert</b><br><br><img src='damhert.jpeg' width = '400px'><br>"),
                    HTML("<b>Edelhert</b><br><br><img src='edelhert.jpeg' width = '400px'><br>"))
            )
        ),
        
        
        tags$br(),
        tags$br(),
        
        
        # Map with according line plot
        
        h3("Landkaart"),
        
        ## countMap: all species
        wellPanel(
            fluidRow(
                column(4, selectInput(inputId = "map_regionLevel", label = "Regio-schaal",
                        choices = c("Vlaanderen" = "flanders",
                            "Provincie" = "provinces", "Gemeente" = "communes"),
                        selected = "communes")),
                column(8, uiOutput("map_region"))
            ),
            
            fluidRow(
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
        
#        # For debugging
#        dataTableOutput("table1"),
#        dataTableOutput("table2"),
        
        fluidRow(
            column(6,
                uiOutput("map_title"),
                leafletOutput("map_spacePlot", height = "700px"),
                tags$br(),
                downloadButton("map_download", "Download")
            ),
            column(6, 
                h4("Referentie (Vlaanderen)"),
                plotlyOutput("map_timePlotFlanders", height = "400px"),
                tags$hr(),
                uiOutput("map_timeTitle"),
                plotlyOutput("map_timePlot", height = "400px"))
        
        ),
        
        tags$hr(),
        
        
        # Show user input module per plot
        
        h3("Extra Figuren en Tabellen"),
        
        ## tableProvince for "leeftijd": wild zwijn and ree
        conditionalPanel("input.showSpecies == 'Wild zwijn' || input.showSpecies == 'Ree'", {
              
              list(
                  actionLink(inputId = "linkTable1",
                      label = h4("Gerapporteerd aantal per regio en per leeftijdscategorie")),
                  conditionalPanel("input.linkTable1 % 2 == 1",
                      
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "table1", showYear = TRUE, exportData = TRUE),
                              tags$p("Het gerapporteerd aantal geschoten dieren per provincie en per leeftijdscategorie voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.")
                          ),
                          column(8, tableModuleUI(id = "table1"))
                      
                      ),
                      tags$hr()
                  )
              
              )
              
            }),
        
        
        ## tableProvince for "type": ree
        conditionalPanel("input.showSpecies == 'Ree'", {
              
              list(
                  
                  actionLink(inputId = "linkTable2",
                      label = h4("Gerapporteerd aantal afschot per regio en per type")),
                  conditionalPanel("input.linkTable2 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "table2", showYear = TRUE, exportData = TRUE),
                              tags$p("Het gerapporteerd aantal geschoten dieren per provincie en per labeltype voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.")
                          ),
                          column(8, tableModuleUI(id = "table2"))
                      
                      ),
                      tags$hr()
                  ),
                  
                  actionLink(inputId = "linkTable3",
                      label = h4("Percentage gerealiseerd afschot per regio en per type")),
                  conditionalPanel("input.linkTable3 % 2 == 1",
                      
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "table3", showYear = TRUE, exportData = TRUE),
                              tags$p("Realisatiegraad per provincie en per labeltype voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.")
                          ),
                          column(8, tableModuleUI(id = "table3"))
                      
                      ),
                      tags$hr()
                  )
              )
              
            }),
        
        
        ## countYearProvince: all species
        actionLink(inputId = "linkPlot1",
            label = h4("Gerapporteerd aantal per jaar en per regio")),
        conditionalPanel("input.linkPlot1 % 2 == 1",
            fluidRow(
                
                column(4,
                    optionsModuleUI(id = "plot1", showTime = TRUE, exportData = TRUE),
                    tags$p("Het gerapporteerd aantal geschoten dieren per jaar in de verschillende provincies voor de geselecteerde periode (cijfers geven het totale afschot voor dat jaar weer).")
                ),
                column(8, plotModuleUI(id = "plot1"))
            
            ),
            tags$hr()
        ),
        
        
        
        
        ## countAgeCheek & countYearAge & percentageYearlyShotAnimals
        ## countAgeGender & boxAgeWeight
        ## - Wild zwijn and Ree
        conditionalPanel("input.showSpecies == 'Wild zwijn' || input.showSpecies == 'Ree'", {
              
              list(
                  actionLink(inputId = "linkPlot2", label =
                          h4("Leeftijdscategorie op basis van onderkaak")),
                  conditionalPanel("input.linkPlot2 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot2", showTime = TRUE, exportData = TRUE),
                              tags$p("Vergelijking tussen de leeftijd zoals aangeduid op het meldingsformulier en de leeftijd bepaalt op basis van een ingezamelde onderkaak")
                          ),
                          column(8, plotModuleUI(id = "plot2"))
                      
                      ),
                      tags$hr()
                  ),
                  
                  
                  actionLink(inputId = "linkPlot3",
                      label = h4("Afschot per jaar en per leeftijdscategorie (o.b.v. onderkaak)")),
                  conditionalPanel("input.linkPlot3 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot3",
                                  showSummarizeBy = TRUE, showTime = TRUE,
                                  regionLevels = 1:2, exportData = TRUE),
                              tags$p("Evolutie van de verdeling van het afschot over de verschillende leeftijdscategorie�n doorheen de jaren op basis van de ingezamelde onderkaak.")
                          ),
                          column(8, plotModuleUI(id = "plot3"))
                      ),
                      tags$hr()
                  ),
                  
                  
                  actionLink(inputId = "linkPlot4",
                      label = h4("Percentage jaarlijks afschot")),
                  conditionalPanel("input.linkPlot4 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot4",
                                  showTime = TRUE, showYear = TRUE,
                                  showType = TRUE, exportData = TRUE),
                              tags$p("Procentuele verdeling van het afschot per labeltype doorheen het openingsseizoen van het geselecteerde jaar in verhouding tot de verdeling in de geselecteerde referentieperiode.")
                          ),
                          column(8, plotModuleUI(id = "plot4"))
                      
                      ),
                      tags$hr()
                  ),
                  
                  
                  actionLink(inputId = "linkPlot5",
                      label = h4("Percentage per leeftijdscategorie en geslacht")),
                  conditionalPanel("input.linkPlot5 % 2 == 1",
                      
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot5", showTime = TRUE, exportData = TRUE),
                              tags$p("Geslachtsverdeling per leeftijdscategorie voor de geselecteerde periode")
                          ),
                          column(8, plotModuleUI(id = "plot5"))
                      
                      ),
                      tags$hr()
                  ),
                  
                  
                  actionLink(inputId = "linkPlot6",
                      label = h4("Verdeling van leeggewicht per leeftijdscategorie (o.b.v. onderkaak) en geslacht")),
                  conditionalPanel("input.linkPlot6 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot6", showTime = TRUE, regionLevels = 1:2, exportData = TRUE),
                              tags$p("Verdeling van de leeggewichten per leeftijdscategorie en per geslacht voor alle gegevens uit de geselecteerde periode.")
                          ),
                          column(8, plotModuleUI(id = "plot6"))
                      
                      ),
                      tags$hr()
                  )
              
              )
              
            }),
        
        ## boxAgeGenderLowerJaw
        ## - Ree
        conditionalPanel("input.showSpecies == 'Ree'", {
              
              list(
                  
                  actionLink(inputId = "linkPlot7",
                      label = h4("Verdeling van onderkaaklengte per leeftijdscategorie en geslacht")),
                  conditionalPanel("input.linkPlot7 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot7", showTime = TRUE,
                                  regionLevels = 1:2, exportData = TRUE),
                              tags$p("Verdeling van de onderkaaklengte per leeftijdscategorie en per geslacht voor alle gegevens uit de geselecteerde periode.")),
                          column(8, plotModuleUI(id = "plot7"))
                      
                      ),
                      tags$hr()
                  ),
                  
                  h3("Bio-indicatoren"),
                  
                  actionLink(inputId = "linkPlot8",
                      label = h4("Onderkaaklengte per jaar")),
                  conditionalPanel("input.linkPlot8 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot8", showTime = TRUE, showType = TRUE,
                                  regionLevels = 1:2, exportData = TRUE),
                              tags$p("Evolutie van de gemodelleerde onderkaaklengte (met 95% betrouwbaarheidsinterval) doorheen de geselecteerde jaren voor de gekozen regio en types.")
                          ),
                          column(8, plotModuleUI(id = "plot8"))
                      ),
                      tags$hr()
                  ),
                  
                  actionLink(inputId = "linkPlot9", label = h4("Gewicht per jaar")),
                  conditionalPanel("input.linkPlot9 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot9",
                                  showTime = TRUE, showType = TRUE,
                                  regionLevels = 1:2, exportData = TRUE),
                              tags$p("Evolutie van de gemodelleerde leeggewichten (met 95% betrouwbaarheidsinterval) doorheen de geselecteerde jaren voor de gekozen regio en types.")
                          ),
                          column(8, plotModuleUI(id = "plot9"))
                      
                      ),
                      tags$hr()
                  ),
                  
                  actionLink(inputId = "linkPlot10",
                      label = h4("Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar")
                  ),
                  conditionalPanel("input.linkPlot10 % 2 == 1",
                      fluidRow(
                          
                          column(4,
                              optionsModuleUI(id = "plot10", showTime = TRUE, showType = TRUE,
                                  regionLevels = 1:2, exportData = TRUE),
                              tags$p("Evolutie van het gerapporteerd aantal embryo's per geschoten dier doorheen de geselecteerde jaren voor de gekozen regio en types.")),
                          column(8, plotModuleUI(id = "plot10"))
                      
                      )
                  )
              
              )
              
            }),
        
        
        tags$br()
    
    
    )

)