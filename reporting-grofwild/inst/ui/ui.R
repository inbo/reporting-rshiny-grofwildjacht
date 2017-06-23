library(leaflet)
library(rCharts)
library(plotly)

shinyUI(
    
    
    fluidPage(
        
        h5(actionLink(inputId = "debug_console", label = "Connect with console"),
            align = "right"),
        # Shape data source
        helpText(h5(a(href = "http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen#", 
                    target = "_blank", "Geodata bron"), align = "right")
        ),
        
        
        
        titlePanel(title = div(img(src = "logo.png", 
                    float = "top", height = "60px", hspace = "50px"),
                "Grofwildjacht in Vlaanderen"), 
            windowTitle = "Grofwildjacht"),
        
        tags$br(),
        tags$br(),
        
#        downloadButton(outputId = "exportResults", label = "Rapport"),
        
        tags$br(),
        tags$br(),
        
        radioButtons("showSpecies", "Diersoort (species)",
            choices = c("Wild zwijn", "Ree", "Damhert", "Edelhert")
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
                            "Provincie" = "provinces", "Gemeente" = "communes"))),
                column(8, uiOutput("map_region"))
            ),
            
            fluidRow(
                column(6, selectInput(inputId = "map_legend", "Legende (kaart)",
                        choices = c("Bovenaan rechts" = "topright", 
                            "Onderaan rechts" = "bottomright", 
                            "Bovenaan links" = "topleft",
                            "Onderaan links" = "bottomleft",
                            "<geen>" = "none"))),
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
                leafletOutput("map_spacePlot", height = "500px"),
                tags$br(),
                downloadButton("map_download", "Download")
            ),
            column(6, plotlyOutput("map_timePlot", height = "600px"))
        
        ),
        
        tags$hr(),
        
        
        # Show user input module per plot
        
        h3("Extra Figuren en Tabellen"),
        
        ## tableProvince for "leeftijd": wild zwijn and ree
        conditionalPanel("input.showSpecies == 'Wild zwijn' || input.showSpecies == 'Ree'", {
              
              list(
                  actionLink(inputId = "linkTable1", 
                      label = h4("Aantal per regio en per leeftijdscategorie")),
                  conditionalPanel("input.linkTable1 % 2 == 1", 
                      
                      fluidRow(
                          
                          column(4, 
                              optionsModuleUI(id = "table1", showYear = TRUE, exportData = TRUE),
                              tags$b("Extra info"),
                              tags$p("Some text on table1")
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
                      label = h4("Aantal afschot per regio en per type")),
                  conditionalPanel("input.linkTable2 % 2 == 1", 
                      fluidRow(
                          
                          column(4, 
                              optionsModuleUI(id = "table2", showYear = TRUE, exportData = TRUE),
                              tags$b("Extra info"),
                              tags$p("Some text on table2")
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
                              tags$b("Extra info"),
                              tags$p("Some text on table3")
                          ),
                          column(8, tableModuleUI(id = "table3"))
                      
                      ),
                      tags$hr()
                  )
              )
              
            }),
        
        
        ## countYearProvince: all species
        actionLink(inputId = "linkPlot1", 
            label = h4("Aantal per jaar en per regio")),
        conditionalPanel("input.linkPlot1 % 2 == 1", 
            fluidRow(
                
                column(4, 
                    optionsModuleUI(id = "plot1", showTime = TRUE, exportData = TRUE),
                    tags$b("Extra info"),
                    tags$p("Some text on plot1")
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot2")
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot3")
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot4")
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot5")
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot6")
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot7")),
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot8")
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
                              tags$b("Extra info"),
                              tags$p("Some text on plot9")
                          ),
                          column(8, plotModuleUI(id = "plot9"))
                      
                      ),
                      tags$hr()
                  ),
                  
                  actionLink(inputId = "linkPlot10", 
                      label = h4("Aantal embryo's voor vrouwelijke reeën per jaar")
                  ),
                  conditionalPanel("input.linkPlot10 % 2 == 1", 
                      fluidRow(
                          
                          column(4, 
                              optionsModuleUI(id = "plot10", showTime = TRUE, showType = TRUE,
                                  regionLevels = 1:2, exportData = TRUE),
                              tags$b("Extra info"),
                              tags$p("Some text on plot10")),
                          column(8, plotModuleUI(id = "plot10"))
                      
                      )
                  )
              
              )
              
            }),
        
        
        tags$br()
    
    
    )

)