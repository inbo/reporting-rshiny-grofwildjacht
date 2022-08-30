# UI file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################



verkeerChoices <- c("F06", "F07_3")
names(verkeerChoices) <- sapply(verkeerChoices, function(x) uiText$title[uiText$plotFunction == x])

landbouwChoices <- c("F09_2", "F09_3")
names(landbouwChoices) <- sapply(landbouwChoices, function(x) uiText$title[uiText$plotFunction == x])

priveChoices <- c("F11_3")
names(priveChoices) <- sapply(priveChoices, function(x) uiText$title[uiText$plotFunction == x])

maatschappijChoices <- c("F12_1", "F14_1", "F14_2", "F14_3", "F14_4", "F14_5")
names(maatschappijChoices) <- sapply(maatschappijChoices, function(x) uiText$title[uiText$plotFunction == x])

populatieChoices <- c("F16_1", "F17_1", "F17_2", "F17_4", "F18_1")
names(populatieChoices) <- sapply(populatieChoices, function(x) uiText$title[uiText$plotFunction == x])

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
          choices = populatieChoices,
          selected = if (doDebug) c("F16_1", "F17_4")
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
          choices = verkeerChoices,
          selected = if (doDebug) c("F06_123")
        ),
        checkboxGroupInput(inputId = "dash_landbouwIndicatoren",
          label = "Landbouw",
          choices = landbouwChoices,
          selected = if (doDebug) c("F09_2", "F09_3")
        ),
        checkboxGroupInput(inputId = "dash_priveIndicatoren",
          label = "Private en publieke gebieden",
          choices = priveChoices
        )
      ),
      column(3, 
        checkboxGroupInput(inputId = "dash_maatschappijIndicatoren",
          label = "Maatschappelijk draagvlak",
          choices = maatschappijChoices,
          selected = if (doDebug) c("F12_1", "F14_1", "F14_2", "F14_3", "F14_4")
        ),
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
      plotDetails = c("biotoop", "biotoopTable")
    ),
    
    
    # Populatie
    uiOutput("dash_populatieTitle"),
    
    conditionalPanel("input.dash_populatieIndicatoren.indexOf('F16_1') > -1", 
      countAgeGroupUI(id = "dash_reproductie", uiText = uiText, groupVariable = "reproductiestatus")
    ),
    
    conditionalPanel("input.dash_populatieIndicatoren.indexOf('F17_1') > -1", 
      actionLink(inputId = "dash_showF17_1", 
        label = h3(HTML(paste("FIGUUR:", uiText$title[uiText$plotFunction == "F17_1"])))),
      conditionalPanel("input.dash_showF17_1 % 2 == 1", 
        mapFlandersUI(id = "F17_1", showCombine = FALSE, type = "dash",  
          regionChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"), 
          unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative"),
          plotDetails = "", showTitle = FALSE
        )
      )
    ),
    
    conditionalPanel("input.dash_populatieIndicatoren.indexOf('F17_2') > -1", 
      actionLink(inputId = "dash_showF17_2", 
        label = h3(HTML(paste("FIGUUR:", uiText$title[uiText$plotFunction == "F17_2"])))),
      conditionalPanel("input.dash_showF17_2 % 2 == 1", 
        mapFlandersUI(id = "F17_2", showCombine = FALSE, type = "dash",  
          regionChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"), 
          unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative"),
          plotDetails = "", showTitle = FALSE
        )
      )
    ),
    
    
    conditionalPanel("input.dash_populatieIndicatoren.indexOf('F17_4') > -1", 
      mapSpreadUI(id = "dash_F17_4", title = uiText$title[uiText$plotFunction == "F17_4"])
    ),
    
    conditionalPanel("input.dash_populatieIndicatoren.indexOf('F18_1') > -1",
      barDraagkrachtUI(id = "dash_F18_1", title = uiText$title[uiText$plotFunction == "F18_1"])
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
    
    conditionalPanel("input.dash_verkeerIndicatoren.indexOf('F06') > -1", 
      mapSpreadUI(id = "dash_F06", title = uiText$title[uiText$plotFunction == "F06"], showLayer = TRUE)
    ),
    
    conditionalPanel("input.dash_verkeerIndicatoren.indexOf('F07_3') > -1", 
      barDraagkrachtUI(id = "dash_verkeer", title = uiText$title[uiText$plotFunction == "F07_3"])
    ),
    
    
    
    # Landbouw
    uiOutput("dash_landbouwTitle"),
    
    conditionalPanel("input.dash_landbouwIndicatoren.indexOf('F09_2') > -1", 
      barCostUI(id = "dash_landbouw", title = uiText$title[uiText$plotFunction == "F09_2"])
    ),
    
    conditionalPanel("input.dash_landbouwIndicatoren.indexOf('F09_3') > -1", 
      barDraagkrachtUI(id = "dash_landbouw", title = uiText$title[uiText$plotFunction == "F09_3"])
    ),
    
    
    # Prive/Publiek
    uiOutput("dash_priveTitle"),
    
    conditionalPanel("input.dash_priveIndicatoren.indexOf('F11_3') > -1", 
      barDraagkrachtUI(id = "dash_prive", title = uiText$title[uiText$plotFunction == "F11_3"])
    ),
    
    # Maatschappelijk draagvlak
    uiOutput("dash_maatschappijTitle"),
    
    conditionalPanel("input.dash_maatschappijIndicatoren.indexOf('F12_1') > -1",
      barDraagkrachtUI(id = "dash_F12_1", title = uiText$title[uiText$plotFunction == "F12_1"])
    ),
    
    conditionalPanel("input.dash_maatschappijIndicatoren.indexOf('F14_1') > -1",
      barDraagkrachtUI(id = "dash_F14_1", title = uiText$title[uiText$plotFunction == "F14_1"])
    ),
    
    conditionalPanel("input.dash_maatschappijIndicatoren.indexOf('F14_2') > -1",
      barDraagkrachtUI(id = "dash_F14_2", title = uiText$title[uiText$plotFunction == "F14_2"])
    ),
    
    conditionalPanel("input.dash_maatschappijIndicatoren.indexOf('F14_3') > -1",
      barDraagkrachtUI(id = "dash_F14_3", title = uiText$title[uiText$plotFunction == "F14_3"],
        subGroups = c("Stakeholders" = "stakeholders", "Publiek" = "public"))
    ),
    
    conditionalPanel("input.dash_maatschappijIndicatoren.indexOf('F14_4') > -1",
      barDraagkrachtUI(id = "dash_F14_4", title = uiText$title[uiText$plotFunction == "F14_4"],
        subGroups = c("Stakeholders" = "stakeholders", "Publiek" = "public"))
    ),
    
    conditionalPanel("input.dash_maatschappijIndicatoren.indexOf('F14_5') > -1",
      barDraagkrachtUI(id = "dash_F14_5", title = uiText$title[uiText$plotFunction == "F14_5"])
    ),
    
    # White space at the bottom
    tags$br()
    
  )

)
