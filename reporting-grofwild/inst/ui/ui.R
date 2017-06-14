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
        
        verbatimTextOutput("print"),
        
        tags$br(),
        tags$br(),
        
        selectInput("showSpecies", "Diersoort (species)",
            choices = c("Wild zwijn", "Ree", "Damhert", "Edelhert")
        ),
        
        # Show user input module per plot
        
        
        ## countYearProvince: all species
        h4("Aantal van wildsoort per jaar en per regio"),
        fluidRow(
            
            column(4, optionsModuleUI(id = "plot1", showTime = TRUE)),
            column(8, plotModuleUI(id = "plot1"))
        
        ),
        
        ## countMap: all species
        wellPanel(
            fluidRow(
                column(4, selectInput(inputId = "map_regionLevel", label = "Regio-schaal",
                        choices = c("Vlaanderen" = "flanders", 
                            "Provincie" = "provinces", "Gemeente" = "communes"))),
                column(8, uiOutput("map_region"))
            ),
            
            fluidRow(
                column(6, selectInput(inputId = "map_legend", "Legende",
                        choices = c("<none>" = "none", 
                            "Bovenaan rechts" = "topright", 
                            "Onderaan rechts" = "bottomright", 
                            "Bovenaan links" = "topleft",
                            "Onderaan links" = "bottomleft"))),
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
                leafletOutput("map_spacePlot", height = "500px")
            ),
            column(6, showOutput("map_timePlot", "highcharts"))
        
        ),
        
        tags$br(),
        fluidRow(
            column(6, downloadButton("downloadPlotSpace", "Download")),
            column(6, downloadButton("downloadPlotTime", "Download"))
        ),
        
        
        
        # TODO
        conditionalPanel("input.showSpecies == 'wildZwijn'", {
              list(
                  h4("Plot 2"),
                  
                  fluidRow(
                      
                      column(4, optionsModuleUI(id = "plot2", showLegend = TRUE, showGlobe = TRUE)),
                      column(8, plotModuleUI(id = "plot2"))
                  
                  )
              )
              
            }),
        
        
        tags$br()
    
    
    )

)