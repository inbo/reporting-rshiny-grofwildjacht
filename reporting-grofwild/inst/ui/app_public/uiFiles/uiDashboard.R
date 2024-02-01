# UI file for everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################


tagList(
  
  tags$div(class = "container",
    
    tags$div(align = "center",
      h1("Welkom op het dashboard voor everzwijnen")
    ),
    
    welcomeSectionUI(id = "dash", uiText = uiText, maxDate = max(ecoData$afschot_datum, na.rm = TRUE)),
    
    wellPanel(
      fixedRow(
        column(4, selectInput(inputId = "dash_regionLevel", label = "Schaal",
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
    tags$div(id = "dash_populatieTitle",
      h2(toupper("Populatie"))),
    
    tags$div(id = "dash_populatieF16_1", 
      countAgeGroupUI(id = "dash_F16_1", uiText = uiText, doHide = FALSE)
    ),
    
    tags$div(id = "dash_populatieF17_1", 
      actionLink(inputId = "dash_showF17_1", 
        label = paste("FIGUUR:", uiText$title[uiText$plotFunction == "F17_1"]),
        class = "action-h3"
      ),
      conditionalPanel("input.dash_showF17_1 % 2 == 0", 
        uiOutput("dash_disclaimerF17_1"),
        mapFlandersUI(id = "dash_F17_1", showCombine = FALSE, type = "dash",  
          regionChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"), 
          unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative"),
          plotDetails = "", showTitle = FALSE
        )
      )
    ),
    
    
    tags$div(id = "dash_populatieF17_4",
      mapSpreadUI(id = "dash_F17_4", uiText = uiText)
    ),
    
    tags$div(id = "dash_populatieF18_1",
      barDraagkrachtUI(id = "dash_F18_1", uiText = uiText)
    ),
    
    tags$div(id = "dash_populatieF18_8",
      kencijferModuleUI(id = "dash_F18_8")
    ),
    
    # Jacht
    tags$div(id = "dash_jachtTitle", 
      h2(toupper("Jacht"))),
    
    tags$div(id = "dash_jachtF04_3", 
      countYearProvinceUI(id = "dash", uiText = uiText,
        plotFunction = "F04_3", doHide = FALSE)
    ),
    
    tags$div(id = "dash_jachtF05_1",  
      trendYearRegionUI(id = "dash", uiText = uiText, 
        showCombinatie = TRUE,
        plotFunction = "F05_1", doHide = FALSE
      )
    ),
    
    tags$div(id = "dash_jachtF05_2", 
      countYearAgeUI(id = "dash", uiText = uiText, 
        plotFunction = "F05_2", showRegion = FALSE, doHide = FALSE)
    ),
    
    # Schade
    tags$div(id = "dash_schadeTitle",
      h2(toupper("Schade"))),
    
#    # Note: F06_1, F06_2 and F06_3 combined
#    conditionalPanel("output.dash_schadeIndicatoren.indexOf('F06_1') > -1", 
#      mapSpreadUI(id = "dash_F06_1", uiText = uiText, showLayer = TRUE)
#    ),
    
    tags$div(id = "dash_schadeF07_1", 
      barCostUI(id = "dash_F07_1", uiText = uiText, 
        typeMelding = c("Verkeer" = "verkeersongeluk", "Landbouw" = "landbouw", 
          "Private en publieke gebieden" = "private en publieke gebieden",
          "Andere" = "andere", "Alle" = "all"))
    ),
    
    tags$div(id = "dash_schadeF09_2", 
      barCostUI(id = "dash_F09_2", uiText = uiText,
        typeMelding = c("Landbouw" = "landbouw"))
    ),
    
    tags$div(id = "dash_schadeF07_3", 
      barDraagkrachtUI(id = "dash_F07_3", uiText = uiText)
    ),
    
    
    # Maatschappelijk draagvlak
    tags$div(id = "dash_maatschappijTitle",
      h2(toupper("Maatschappelijk draagvlak"))
    ),
    
    tags$div(id = "dash_maatschappijF12_1",
      barDraagkrachtUI(id = "dash_F12_1", uiText = uiText)
    ),
    
    tags$div(id = "dash_maatschappijF14_1",
      barDraagkrachtUI(id = "dash_F14_1", uiText = uiText)
    ),
    
    tags$div(id = "dash_maatschappijF14_2",
      barDraagkrachtUI(id = "dash_F14_2", uiText = uiText)
    ),
    
    tags$div(id = "dash_maatschappijF14_3",
      barDraagkrachtUI(id = "dash_F14_3", uiText = uiText,
        sectorChoices = c("Stakeholders" = "stakeholders", "Publiek" = "public"),
        selectGroups = TRUE, height = "1000px")
    ),
    
    tags$div(id = "dash_maatschappijF14_4",
      barDraagkrachtUI(id = "dash_F14_4", uiText = uiText,
        sectorChoices = c("Stakeholders" = "stakeholders", "Publiek" = "public"), 
        selectGroups = TRUE, height = "800px")
    ),
    
    tags$div(id = "dash_maatschappijF14_5",
      barDraagkrachtUI(id = "dash_F14_5", uiText = uiText, 
        selectGroups = TRUE, height = "1500px")
    )
  
  ),
  
  tags$div(style = "margin-bottom: 50px;"),
  
  tags$div(class = "footer",
    tags$div(class = "footer-content",
      singleton(
        tags$head(tags$script(src = "www/triggerDownload.js"))
      ),
      actionButton(inputId = "dash_createReport", label = "Maak rapport", 
        icon = icon("file-pdf")),
      downloadLink("dash_downloadReport", " ")
    )
  )

)

)
