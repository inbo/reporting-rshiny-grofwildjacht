library(leaflet)
library(rCharts)

shinyUI(
    
    
    fluidPage(
        
        h5(actionLink(inputId = "debug_console", label = "Debug"),
            align = "right"),
        
        titlePanel(title = div(img(src = "logo.png", 
                    float = "top", height = "60px", hspace = "50px"),
                "Grofwildjacht in Vlaanderen"), 
            windowTitle = "Grofwildjacht"),
        
        tags$br(),
        tags$br(),
        
        
        selectInput(inputId = "spatialLevel", label = "Regio-schaal",
            choices = c("Provincie" = "provinces", "Gemeente" = "communes")),
        selectInput("colorPalette", "Kleurenpalet",
            choices = c("Rood" = "Reds", "Blauw" = "Blues", "Groen" = "Greens", 
                "Divergerend" = "RdBu")),
        
        actionButton(inputId = "doMap", label = "Toon figuren"),
        
        tags$br(), 
        tags$br(),
        
        uiOutput("controlMap"),
        
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
        uiOutput("downloadPlots"),
        tags$br()

        
    )

)