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
      column(4,
        dashboardChoices(id = "dash_populatie", 
          choices = populatieChoices,
          selected = if (doDebug) c("F16_1"),
          uiText = uiText
        ),
        dashboardChoices(id = "dash_jacht",
          choices = jachtChoices,
          selected = if (doDebug) c("F05_1", "F05_2"),
          uiText = uiText
        )
      ),
      column(4,
        dashboardChoices(id = "dash_verkeer",
          choices = verkeerChoices, uiText = uiText),
        dashboardChoices(id = "dash_landbouw",
          choices = landbouwChoices, 
          selected = if (doDebug) c("F09_1", "F09_2"),
          uiText = uiText
        ),
        dashboardChoices(id = "dash_prive",
          choices = priveChoices,
          uiText = uiText)
      ),
      column(4, 
        dashboardChoices(id = "dash_maatschappij",
          choices = maatschappijChoices,
          selected = if (doDebug) c("F14_1"),
          uiText = uiText)
      )
    ),
    
    tags$p(tags$em("\U002A Van deze indicator zijn slechts gedeeltelijke gegevens beschikbaar op het gekozen niveau")),
    
#    actionButton(inputId = "dash_submit", label = "Maak dashboard"),
    actionButton(inputId = "dash_createReport", label = "Maak pdf"),
    
    tags$hr(),
    
    # Achtergrond
    tags$h2(toupper("Achtergrondinformatie")),
    mapFlandersUI(id = "dash_background", showRegion = FALSE, showCombine = FALSE,
      type = "empty",
      showSource = FALSE,
      plotDetails = c("biotoop", "biotoopTable"), 
      showTitle = FALSE
    ),
    
    
    # Populatie
    conditionalPanel("output.dash_populatieIndicatoren.length > 0",
      h2(toupper("Populatie"))),
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F16_1') > -1", 
      countAgeGroupUI(id = "dash_reproductie", uiText = uiText,
        title = paste("FIGUUR:", uiText$title[uiText$plotFunction == "F16_1"]),
        groupVariable = "reproductiestatus", doHide = FALSE)
    ),
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F17_1') > -1", 
      actionLink(inputId = "dash_showF17_1", 
        label = h3(HTML(paste("FIGUUR:", uiText$title[uiText$plotFunction == "F17_1"])))),
      conditionalPanel("output.dash_showF17_1 % 2 == 1", 
        mapFlandersUI(id = "F17_1", showCombine = FALSE, type = "dash",  
          regionChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"), 
          unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative"),
          plotDetails = "", showTitle = FALSE
        )
      )
    ),
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F17_2') > -1", 
      actionLink(inputId = "dash_showF17_2", 
        label = h3(HTML(paste("FIGUUR:", uiText$title[uiText$plotFunction == "F17_2"])))),
      conditionalPanel("output.dash_showF17_2 % 2 == 1", 
        mapFlandersUI(id = "F17_2", showCombine = FALSE, type = "dash",  
          regionChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"), 
          unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative"),
          plotDetails = "", showTitle = FALSE
        )
      )
    ),
    
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F17_4') > -1", 
      mapSpreadUI(id = "dash_F17_4", title = uiText$title[uiText$plotFunction == "F17_4"])
    ),
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F18_1') > -1",
      barDraagkrachtUI(id = "dash_F18_1", title = uiText$title[uiText$plotFunction == "F18_1"])
    ),
    
    
    # Jacht
    conditionalPanel("output.dash_jachtIndicatoren.length > 0",
      h2(toupper("Jacht"))),
    
    conditionalPanel("output.dash_jachtIndicatoren.indexOf('F05_1') > -1", 
      trendYearRegionUI(id = "dash", uiText = uiText, 
        title = paste("FIGUUR:", uiText$title[uiText$plotFunction == "F05_1"]),
        doHide = FALSE
      )
    ),
    
    conditionalPanel("output.dash_jachtIndicatoren.indexOf('F05_2') > -1", 
      countYearAgeUI(id = "dash", uiText = uiText, 
        title = paste("FIGUUR:", uiText$title[uiText$plotFunction == "F05_2"]),
        showRegion = FALSE, doHide = FALSE)
    ),
    
    # Verkeer
    conditionalPanel("output.dash_verkeerIndicatoren.length > 0",
      h2(toupper("Verkeer"))),
    
    # Note: F06_1, F06_2 and F06_3 combined
    conditionalPanel("output.dash_verkeerIndicatoren.indexOf('F06_1') > -1", 
      mapSpreadUI(id = "dash_F06_1", title = uiText$title[uiText$plotFunction == "F06_1"], showLayer = TRUE)
    ),
    
    conditionalPanel("output.dash_verkeerIndicatoren.indexOf('F07_1') > -1", 
      barCostUI(id = "dash_F07_1", title = uiText$title[uiText$plotFunction == "F07_1"])
    ),
    
    conditionalPanel("output.dash_verkeerIndicatoren.indexOf('F07_3') > -1", 
      barDraagkrachtUI(id = "dash_F07_3", title = uiText$title[uiText$plotFunction == "F07_3"])
    ),
    
    
    
    # Landbouw
    conditionalPanel("output.dash_landbouwIndicatoren.length > 0",
      h2(toupper("Landbouw"))),
    
    conditionalPanel("output.dash_landbouwIndicatoren.indexOf('F09_1') > -1", 
      barCostUI(id = "dash_F09_1", title = uiText$title[uiText$plotFunction == "F09_1"],
        summarizeBy = c("Seizoen" = "season", "Soortnaam" = "SoortNaam"))
    ),
    
    conditionalPanel("output.dash_landbouwIndicatoren.indexOf('F09_2') > -1", 
      barCostUI(id = "dash_F09_2", title = uiText$title[uiText$plotFunction == "F09_2"],
        summarizeBy = c("Seizoen" = "season", "Soortnaam" = "SoortNaam"))
    ),
    
    conditionalPanel("output.dash_landbouwIndicatoren.indexOf('F09_3') > -1", 
      barDraagkrachtUI(id = "dash_F09_3", title = uiText$title[uiText$plotFunction == "F09_3"])
    ),
    
    
    # Prive/Publiek
    conditionalPanel("output.dash_priveIndicatoren.length > 0",
      h2(toupper("Private en publieke gebieden"))
    ),
    
    conditionalPanel("output.dash_priveIndicatoren.indexOf('F11_1') > -1", 
      barCostUI(id = "dash_F11_1", title = uiText$title[uiText$plotFunction == "F11_1"])
    ),
    
    conditionalPanel("output.dash_priveIndicatoren.indexOf('F11_3') > -1", 
      barDraagkrachtUI(id = "dash_F11_3", title = uiText$title[uiText$plotFunction == "F11_3"])
    ),
    
    # Maatschappelijk draagvlak
    conditionalPanel("output.dash_maatschappijIndicatoren.length > 0",
      h2(toupper("Maatschappelijk draagvlak"))
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F12_1') > -1",
      barDraagkrachtUI(id = "dash_F12_1", title = uiText$title[uiText$plotFunction == "F12_1"])
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_1') > -1",
      barDraagkrachtUI(id = "dash_F14_1", title = uiText$title[uiText$plotFunction == "F14_1"])
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_2') > -1",
      barDraagkrachtUI(id = "dash_F14_2", title = uiText$title[uiText$plotFunction == "F14_2"])
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_3') > -1",
      barDraagkrachtUI(id = "dash_F14_3", title = uiText$title[uiText$plotFunction == "F14_3"],
        subGroups = c("Stakeholders" = "stakeholders", "Publiek" = "public"))
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_4') > -1",
      barDraagkrachtUI(id = "dash_F14_4", title = uiText$title[uiText$plotFunction == "F14_4"],
        subGroups = c("Stakeholders" = "stakeholders", "Publiek" = "public"))
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_5') > -1",
      barDraagkrachtUI(id = "dash_F14_5", title = uiText$title[uiText$plotFunction == "F14_5"])
    ),
    
    # White space at the bottom
    tags$br()
  
  )
)
