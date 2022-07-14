# UI file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################


tagList(
  
  tags$div(class = "container",
    
    tags$br(),
    
    wellPanel(
      fixedRow(
        column(4, selectInput(inputId = "dash_regionLevel", label = "Regio-schaal",
            choices = c(
              "Vlaanderen" = "flanders",
              "Provincie" = "provinces", 
              "Faunabeheerzones" = "faunabeheerzones",
              "Gemeente" = "communes"
            ),
            selected = "provinces")
        ),
        column(8, uiOutput("dash_region")))   
    ),
    
    welcomeSection(id = "dash", uiText = uiText),
    
    checkboxGroupInput(inputId = "dash_jachtIndicatoren",
      label = "Jacht",
      choices = c(
        "Absoluut afschot" = "F05_1",
        "Samenstelling afschot" = "F05_2"
      ),
      selected = if (doDebug) c("F05_1", "F05_2")
    ),
    
    actionButton(inputId = "dash_submit", label = "Maak dashboard"),
    actionButton(inputId = "dash_createReport", label = "Maak pdf"),
    
    tags$hr(),
    
    # Achtergrond
    tags$h2(toupper("Achtergrondinformatie")),
    mapFlandersUI(id = "dash", showRegion = FALSE, showCombine = FALSE,
      type = "dash",
      unitChoices = c("Aantal" = "absolute", 
        "Aantal/100ha" = "relative", 
        "Aantal/100ha bos & natuur" = "relativeDekking"),
      plotDetails = "biotoop"),
    
    
    
 
    # Jacht
    uiOutput("dash_jachtTitle"),
    
    conditionalPanel("input.dash_jachtIndicatoren.indexOf('F05_1') > -1", 
     trendYearRegionUI(id = "dash", uiText = uiText)
    ),
    
    conditionalPanel("input.dash_jachtIndicatoren.indexOf('F05_2') > -1", 
      countYearAgeUI(id = "dash", uiText = uiText, showRegion = FALSE)
    )
    
  
  )

)
