library(leaflet)
library(rCharts)

shinyUI(
    
    
    fluidPage(
        
#        h5(actionLink(inputId = "debug_console", label = "Debug"),
#            align = "right"),
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
        
        wellPanel(
            fluidRow(
                column(4, selectInput(inputId = "spatialLevel", label = "Regio-schaal",
                        choices = c("Provincie" = "provinces", "Gemeente" = "communes"))),
                column(4, selectInput("colorPalette", "Kleurenpalet",
                        choices = c("Rood" = "Reds", "Blauw" = "Blues", "Groen" = "Greens", 
                            "Divergerend" = "RdBu")))                    
            ),
            uiOutput("controlMap")
        ),
        
        fluidRow(
            column(6,
                uiOutput("titleInteractivePlot"),
                leafletOutput("showMap", height = "500px")
            ),
            column(6, 
                showOutput("interactiveTime", "highcharts")
            
            )
        ),
        
        tags$br(),
        fluidRow(
            column(6, 
                downloadButton("downloadPlotSpace", "Download")
            ),
            column(6, 
                downloadButton("downloadPlotTime", "Download")
            )
        ),
        
        tags$br()
    
    
    )

)