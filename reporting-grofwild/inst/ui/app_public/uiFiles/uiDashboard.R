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
        column(8, uiOutput("dash_region"))),
      uiOutput("dash_regionWarning")
    ),
    
    welcomeSection(id = "dash", uiText = uiText),
    
    fixedRow(
      column(4,
        dashboardChoices(id = "dash_populatie", 
          choices = populatieChoices,
          selected = if (doDebug) populatieChoices,
          uiText = uiText
        ),
        dashboardChoices(id = "dash_jacht",
          choices = jachtChoices,
          selected = if (doDebug) jachtChoices,
          uiText = uiText
        )
      ),
      column(4,
        dashboardChoices(id = "dash_schade",
          choices = schadeChoices, uiText = uiText,
          selected = if (doDebug) schadeChoices
        )
      ),
      column(4, 
        dashboardChoices(id = "dash_maatschappij",
          choices = maatschappijChoices,
          selected = if (doDebug) maatschappijChoices,
          uiText = uiText)
      )
    ),
    
    getDisclaimerLimited(),
        
    tags$hr(),
    
    tags$div(id = "dash_results", 
    
    # Achtergrond
    tags$h2(toupper("Achtergrondinformatie")),
    mapFlandersUI(id = "dash_background", showRegion = FALSE,
      type = "empty", showTitle = FALSE, showCombine = TRUE,
      plotDetails = c("biotoop", "biotoopTable")
    ),
    
    
    # Populatie
    conditionalPanel("output.dash_populatieIndicatoren.length > 0",
      h2(toupper("Populatie"))),
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F16_1') > -1", 
      countAgeGroupUI(id = "dash_F16_1", uiText = uiText, doHide = FALSE)
    ),
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F17_1') > -1", 
      actionLink(inputId = "dash_showF17_1", 
        label = paste("FIGUUR:", uiText$title[uiText$plotFunction == "F17_1"]),
        class = "action-h3"
      ),
      conditionalPanel("input.dash_showF17_1 % 2 == 0", 
        tagList(
          uiOutput("dash_disclaimerF17_1"),
          tags$p(HTML(uiText$dash[uiText$plotFunction == "F17_1"]))),
        mapFlandersUI(id = "dash_F17_1", showCombine = FALSE, type = "dash",  
          regionChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"), 
          unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative"),
          sourceChoices = c("waarnemingen.be", "afschot"),
          plotDetails = "", showTitle = FALSE
        )
      )
    ),
    
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F17_4') > -1",
      mapSpreadUI(id = "dash_F17_4", uiText = uiText)
    ),
    
    conditionalPanel("output.dash_populatieIndicatoren.indexOf('F18_1') > -1",
      barDraagkrachtUI(id = "dash_F18_1", uiText = uiText)
    ),
    
    
    # Jacht
    conditionalPanel("output.dash_jachtIndicatoren.length > 0",
      h2(toupper("Jacht"))),
    
    conditionalPanel("output.dash_jachtIndicatoren.indexOf('F04_3') > -1", 
      countYearProvinceUI(id = "dash", uiText = uiText,
        plotFunction = "F04_3", doHide = FALSE)
    ),
    
    conditionalPanel("output.dash_jachtIndicatoren.indexOf('F05_1') > -1", 
      trendYearRegionUI(id = "dash", uiText = uiText, 
        showCombinatie = TRUE,
        plotFunction = "F05_1", doHide = FALSE
      )
    ),
    
    conditionalPanel("output.dash_jachtIndicatoren.indexOf('F05_2') > -1", 
      countYearAgeUI(id = "dash", uiText = uiText, 
        plotFunction = "F05_2", showRegion = FALSE, doHide = FALSE)
    ),
    
    # Schade
    conditionalPanel("output.dash_schadeIndicatoren.length > 0",
      h2(toupper("Schade"))),
    
    # Note: F06_1, F06_2 and F06_3 combined
    conditionalPanel("output.dash_schadeIndicatoren.indexOf('F06_1') > -1", 
      mapSpreadUI(id = "dash_F06_1", uiText = uiText, showLayer = TRUE)
    ),
    
    conditionalPanel("output.dash_schadeIndicatoren.indexOf('F07_1') > -1", 
      barCostUI(id = "dash_F07_1", uiText = uiText, 
        typeMelding = c("Verkeer" = "verkeersongeluk", "Landbouw" = "landbouw", 
          "Private en publieke gebieden" = "private en publieke gebieden",
          "Andere" = "andere", "Alle" = "all"))
    ),
    
    conditionalPanel("output.dash_schadeIndicatoren.indexOf('F09_2') > -1", 
      barCostUI(id = "dash_F09_2", uiText = uiText,
        typeMelding = c("Landbouw" = "landbouw"))
    ),
    
    conditionalPanel("output.dash_schadeIndicatoren.indexOf('F07_3') > -1", 
      barDraagkrachtUI(id = "dash_F07_3", uiText = uiText)
    ),
    
    
    # Maatschappelijk draagvlak
    conditionalPanel("output.dash_maatschappijIndicatoren.length > 0",
      h2(toupper("Maatschappelijk draagvlak"))
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F12_1') > -1",
      barDraagkrachtUI(id = "dash_F12_1", uiText = uiText)
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_1') > -1",
      barDraagkrachtUI(id = "dash_F14_1", uiText = uiText)
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_2') > -1",
      barDraagkrachtUI(id = "dash_F14_2", uiText = uiText)
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_3') > -1",
      barDraagkrachtUI(id = "dash_F14_3", uiText = uiText,
        sectorChoices = c("Stakeholders" = "stakeholders", "Publiek" = "public"),
        selectGroups = TRUE, height = "1000px")
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_4') > -1",
      barDraagkrachtUI(id = "dash_F14_4", uiText = uiText,
        sectorChoices = c("Stakeholders" = "stakeholders", "Publiek" = "public"), 
        selectGroups = TRUE, height = "800px")
    ),
    
    conditionalPanel("output.dash_maatschappijIndicatoren.indexOf('F14_5') > -1",
      barDraagkrachtUI(id = "dash_F14_5", uiText = uiText, 
        selectGroups = TRUE, height = "1500px")
    ),
    
    # White space at the bottom
    tags$br()
  
  ),
  
  tags$div(class = "footer",
    tags$div(class = "footer-content",
      singleton(
        tags$head(tags$script(src = "www/triggerDownload.js"))
      ),
      actionButton(inputId = "dash_createReport", label = "Maak pdf"),
      downloadLink("dash_downloadReport", " ")
    )
  )

)

)
