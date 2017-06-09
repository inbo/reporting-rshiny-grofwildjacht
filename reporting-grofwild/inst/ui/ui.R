library(leaflet)
library(rCharts)

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
        
        downloadButton(outputId = "exportResults", label = "Rapport"),
        
        tags$br(),
        tags$br(),
        
        selectInput("showSpecies", "Diersoort (species)",
            choices = c("wild zwijn" = "wildZwijn", "ree", "damhert", "edelhert"), 
            selected = "wildZwijn"
        ),
        
        # Show user input module per plot
        fluidRow(
            
            column(4, figureModuleUI(id = "plot1", title = "Plot 1")),
            column(8, plotOutput("plot1"))
        
        ),
        
        conditionalPanel("input.showSpecies == 'wildZwijn'", {
              
              fluidRow(
                  
                  column(4, figureModuleUI(id = "plot2", title = "Plot 2", 
                          showLegend = FALSE)),
                  column(8, plotOutput("plot2"))
              
              )
              
            }),
        
#        fluidRow(
#            column(6,
#                uiOutput("titleInteractivePlot"),
#                leafletOutput("showMap", height = "500px")
#            ),
#            column(6, 
#                showOutput("interactiveTime", "highcharts")
#            
#            )
#        ),
#        
#        tags$br(),
#        fluidRow(
#            column(6, 
#                downloadButton("downloadPlotSpace", "Download")
#            ),
#            column(6, 
#                downloadButton("downloadPlotTime", "Download")
#            )
#        ),
#        
        tags$br()
    
    
    )

)