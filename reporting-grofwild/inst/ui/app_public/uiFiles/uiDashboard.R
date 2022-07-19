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
    
    fixedRow(
      column(3,
        checkboxGroupInput(inputId = "dash_populatieIndicatoren",
          label = "Populatie",
          choices = c(
            "Deelname aan de reproductie *" = "F16_1"
          ),
          selected = if (doDebug) c("F16_1")
        )
      ),
      column(3,
        checkboxGroupInput(inputId = "dash_jachtIndicatoren",
          label = "Jacht",
          choices = c(
            "Absoluut afschot" = "F05_1",
            "Samenstelling afschot" = "F05_2"
          ),
          selected = if (doDebug) c("F05_1", "F05_2")
        )
      ),
      column(3,
        checkboxGroupInput(inputId = "dash_verkeerIndicatoren",
          label = "Verkeer",
          choices = c(
            "Preventieve rasters *" = "F06_1",
            "Preventieve signalisatie *" = "F06_2",
            "Preventieve snelheidsbeperkingen *" = "F06_3"
          ),
          selected = if (doDebug) c("F05_1", "F05_2")
        )
      )
    ),
    
    tags$p(tags$em("* Van deze indicator zijn slechts gedeeltelijke gegevens beschikbaar op het gekozen niveau")),
    
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
    
    
    # Populatie
    uiOutput("dash_populatieTitle"),
    
    conditionalPanel("input.dash_populatieIndicatoren.indexOf('F16_1') > -1", 
      countAgeGroupUI(id = "dash_reproductie", uiText = uiText, groupVariable = "reproductiestatus")
    ),
    
    
    # Jacht
    uiOutput("dash_jachtTitle"),
    
    conditionalPanel("input.dash_jachtIndicatoren.indexOf('F05_1') > -1", 
      trendYearRegionUI(id = "dash", uiText = uiText)
    ),
    
    conditionalPanel("input.dash_jachtIndicatoren.indexOf('F05_2') > -1", 
      countYearAgeUI(id = "dash", uiText = uiText, showRegion = FALSE)
    ),
    
    # Verkeer
    uiOutput("dash_verkeerTitle"),
    
    conditionalPanel("input.dash_verkeerIndicatoren.indexOf('F06_1') > -1", 
      actionLink(inputId = "dash_showVerkeer", label = tags$h3("FIGUUR: Preventieve maatregelen verkeer")),
      conditionalPanel("input.dash_showVerkeer % 2 == 1", 
        leafletOutput(outputId = "dash_verkeer"),
        tags$hr()
      )
    ),
  
    tags$br()
  )

)
