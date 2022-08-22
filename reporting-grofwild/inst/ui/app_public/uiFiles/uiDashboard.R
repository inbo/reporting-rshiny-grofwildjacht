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
            "Deelname aan de reproductie *" = "F16_1",
            "Toekomstig verspreidingsgebied" = "F17_4"
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
            "Preventieve maatregelen *" = "F06_123",
            "Inschatting schade" = "F07_3"
          ),
          selected = if (doDebug) c("F06_123")
        )
      ),
      column(3,
        checkboxGroupInput(inputId = "dash_landbouwIndicatoren",
          label = "Landbouw",
          choices = c(
            "Kosten schade" = "F09_2",
            "Inschatting schade" = "F09_3"
          ),
          selected = if (doDebug) c("F09_2", "F09_3")
        ),
        checkboxGroupInput(inputId = "dash_priveIndicatoren",
          label = "Private en publieke gebieden",
          choices = c(
            "Inschatting schade" = "F11_3"
          )
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
    
#    conditionalPanel("input.dash_populatieIndicatoren.indexOf('F17_4') > -1", 
#      mapSpreadUI(id = "dash_toekomst", uiText = uiText)
#    ),
    
    
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
    
    conditionalPanel("input.dash_verkeerIndicatoren.indexOf('F06_123') > -1", 
      actionLink(inputId = "dash_showVerkeer", label = tags$h3("FIGUUR: Preventieve maatregelen verkeer")),
      conditionalPanel("input.dash_showVerkeer % 2 == 1", 
        wellPanel(
          # TODO make checkbox work
          checkboxGroupInput(inputId = "dash_verkeerLayers", label = "Toon",
            choices = c("Preventieve rasters" = "ecorasters",
              "Preventieve signalisatie/snelheidsbeperkingen" = "oversteek"),
            inline = TRUE)
        ),
        leafletOutput(outputId = "dash_verkeer"),
        tags$hr()
      )
    ),
    
    conditionalPanel("input.dash_verkeerIndicatoren.indexOf('F07_3') > -1", 
      barDraagkrachtUI(id = "dash_verkeer", uiText = uiText)
    ),
    
    
    
    # Landbouw
    uiOutput("dash_landbouwTitle"),
    
    conditionalPanel("input.dash_landbouwIndicatoren.indexOf('F09_2') > -1", 
      barCostUI(id = "dash_landbouw", uiText = uiText)
    ),
    
    conditionalPanel("input.dash_landbouwIndicatoren.indexOf('F09_3') > -1", 
      barDraagkrachtUI(id = "dash_landbouw", uiText = uiText)
    ),
    
    
    # Prive/Publiek
    uiOutput("dash_priveTitle"),
    
    conditionalPanel("input.dash_priveIndicatoren.indexOf('F11_3') > -1", 
      barDraagkrachtUI(id = "dash_prive", uiText = uiText)
    ),
    
    
    # White space at the bottom
    tags$br()
    
  )

)
