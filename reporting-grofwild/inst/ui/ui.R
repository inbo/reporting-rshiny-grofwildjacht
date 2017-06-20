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
        
        selectInput("showSpecies", "Diersoort (species)",
            choices = c("Wild zwijn", "Ree", "Damhert", "Edelhert")
        ),
        
        
        # Map with according line plot
        
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
        
        ## tableProvince for "leeftijd": wild zwijn and ree
        conditionalPanel("input.showSpecies == 'Wild zwijn' || input.showSpecies == 'Ree'", {
              
              list(
                  h4("Aantal per regio en per leeftijdscategorie"),
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "table1", showYear = TRUE)),
                      column(8, tableModuleUI(id = "table1"))
                  
                  ),
                  tags$hr()
              )
              
            }),
        
        
        ## tableProvince for "type": ree
        conditionalPanel("input.showSpecies == 'Ree'", {
              
              list(
                  h4("Aantal afschot per regio en per type"),
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "table2", showYear = TRUE)),
                      column(8, tableModuleUI(id = "table2"))
                  
                  ),
                  tags$hr(),
                  
                  
                  h4("Percentage gerealiseerd afschot per regio en per type"),
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "table3", showYear = TRUE)),
                      column(8, tableModuleUI(id = "table3"))
                  
                  ),
                  tags$hr()
              )
              
            }),
        
        
        ## countYearProvince: all species
        h4("Aantal per jaar en per regio"),
        fluidRow(
            
            column(4, optionsModuleUI(id = "plot1", showTime = TRUE)),
            column(8, plotModuleUI(id = "plot1"))
        
        ),
        tags$hr(),
        
        
        
        ## countAgeCheek & countYearAge: Wild zwijn and Ree
        conditionalPanel("input.showSpecies == 'Wild zwijn' || input.showSpecies == 'Ree'", {
              
              list(
                  h4("Leeftijdscategorie op basis van onderkaak"),
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "plot2", showTime = TRUE)),
                      column(8, plotModuleUI(id = "plot2"))
                  
                  ),
                  tags$hr(),
                  
                  
                  h4("Afschot per jaar en per leeftijdscategorie (o.b.v. onderkaak)"),
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "plot3", 
                              showSummarizeBy = TRUE, showTime = TRUE, 
                              regionLevels = 1:2)),
                      column(8, plotModuleUI(id = "plot3"))
                  
                  ),
                  tags$hr(),
                  
                  
                  h4("Percentage jaarlijks afschot"),
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "plot4", 
                              showTime = TRUE, showYear = TRUE, showType = TRUE)),
                      column(8, plotModuleUI(id = "plot4"))
                  
                  ),
                  tags$hr(),
                  
                  
                  h4("Percentage per leeftijdscategorie en geslacht"),
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "plot5", 
                              showTime = TRUE)),
                      column(8, plotModuleUI(id = "plot5"))
                  
                  ),
                  tags$hr()
              
              )
              
            }),
        
        
        tags$br()
    
    
    )

)