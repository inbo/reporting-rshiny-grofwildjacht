# UI file for WBE page
# 
# Author: mvarewyck
###############################################################################



tagList(
  
  tags$div(class = "container",
    
    tags$br(),
    
    if (length(currentKbo) > 1)
      selectInput(inputId = "wbe_kboChoice", label = "WBE Naam", 
        choices = currentKbo, width = "100%"),
    
    tags$div(align = "center",
      uiOutput("wbe_title")
    ),
    
    welcomeSection(id = "wbe", uiText = uiText),
           
           tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
            tags$ul(
                tags$li("Onderstaande tabellen en figuren zijn gebaseerd op de beschikbare gegevens op ", format(max(as.Date(ecoData$afschot_datum), na.rm = T), 
                        "%d/%m/%Y"), ". Een deel van de data van het voorbije kwartaal kunnen dus mogelijk nog niet opgenomen zijn in de dataset."),
                tags$li("De bron voor de kaarten, figuren en tabellen wordt gevormd door het",
                    tags$a(href = "https://www.natuurenbos.be/e-loket", "E-loket fauna en flora (Agentschap voor Natuur en Bos)", target = "_blank"),
                    ", gecombineerd met door het INBO uitgevoerde metingen op ingezamelde stalen (onderkaken en baarmoeders)."),
                tags$li("Deze gegevens m.b.t. schade komen uit het Meldpunt-schaderegistratie van het",
                        tags$a(href = "https://www.natuurenbos.be/e-loket", "E-loket fauna en flora (Agentschap voor Natuur en Bos)", target = "_blank"),
                        ", ",
                        tags$a(href = "https://waarnemingen.be/", "Waarnemingen.be (Natuurpunt)", target = "_blank"),
                        ", ",
                        tags$a(href = "https://hvv.be/wilder/", "Wilder (Hubertus Vereniging Vlaanderen)", target = "_blank"),
                        "en meldingen van verkeersongevallen met wilde dieren uit andere bronnen."),
                tags$li("De data achter de figuren en tabellen kan je steeds downloaden als ruwe data."),
                tags$li("De figuren zelf kan je ook als .png downloaden."),
                tags$li("Indien u fouten zou ontdekken of merkt dat data ontbreken gelieve dit dan te melden via een email naar",
                    tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie", target="_blank", "faunabeheer@inbo.be"), ".")
            )
        )
  
  ),
  
  # Select species
  
  tags$div(class = "container",
    
    # Map
    
    mapFlandersUI(id = "wbe", showRegion = FALSE, showCombine = FALSE,
      type = "wbe", plotDetails = "biotoop"),
    

    # Choose species
    
    uiOutput("wbe_empty"),
    
    h2("Grofwildsoort")),
  
  tags$div(class = "container", 
    
    align = "center", 
    tags$div(class = "noButton",
      radioButtons(inputId = "wbe_species", label = "", inline = TRUE,
        choiceValues = list("Wild zwijn", "Ree", "Damhert", "Edelhert"),
        choiceNames = list(
          HTML("<div class='fotoTitel'>Wild zwijn</div><div id='wildZwijnFoto'></div>"),
          HTML("<div class='fotoTitel'>Ree</div><div id='reeFoto'></div>"),
          HTML("<div class='fotoTitel'>Damhert</div><div id='damhertFoto'></div>"),
          HTML("<div class='fotoTitel'>Edelhert</div><div id='edelhertFoto'></div>"))
      )
    )
  ),
  
  conditionalPanel("output.wbe_emptyAfschot == false",
    
    trendYearRegionUI(id = "wbe", uiText = uiText),
    
    tableSpeciesUI(id = "wbe", uiText = uiText)
  
  ),
  
  tags$div(class = "container",
    
    h2("Extra Figuren en Tabellen"),
    
    conditionalPanel("output.wbe_emptyAfschot == false",
      
      mapAfschotUI(id = "wbe_afschot",
        uiText = uiText,
        filterSource = FALSE, filterAccuracy = TRUE,
        variableChoices = c(
          "Seizoen" = "season",
          "Jaar" = "afschotjaar",
          "Jachtmethode" = "jachtmethode_comp")
      ),
      
      ## tableSpecies: wild zwijn and ree
      conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'", {
          
          countYearShotUI(id = "wbe_labeltype", groupVariable = "labeltype", uiText = uiText)
          
        }),
      
      
      countYearShotUI(id = "wbe_jachtmethode", groupVariable = "jachtmethode_comp", uiText = uiText)
    ),
    
    conditionalPanel("output.wbe_emptySchade == false",
      # When no afschot, might still be schadeData
      
      actionLink(inputId = "wbe_linkMapSchade", label =
          h3(HTML(uiText$title[uiText$plotFunction == "mapSchadeUI"]))),
      conditionalPanel("input.wbe_linkMapSchade % 2 == 1",
        
        mapSchadeUI(id = "wbe",
          uiText = uiText[uiText$plotFunction == "mapSchadeUI", ], 
          filterCode = TRUE, filterSubcode = TRUE,
          plotDetails = "region")
  
      )
    ),
    
    conditionalPanel("output.wbe_emptyAfschot == false",
      countAgeGenderUI(id = "wbe", uiText = uiText),
      countAgeCheekUI(id = "wbe", showAccuracy = TRUE, uiText = uiText),
      
      conditionalPanel("input.wbe_species == 'Ree'",
        ageGenderLowerJawUI(id = "wbe", regionLevels = NULL, uiText = uiText),    
        percentageRealisedShotUI(id = "wbe", showAccuracy = TRUE, uiText = uiText)
      ),

      
      conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'",
        bioindicatorSection(id = "wbe", uiText = uiText),
        conditionalPanel("input.wbe_species == 'Ree'",
          plotBioindicatorUI("wbe_onderkaak", bioindicator = "onderkaaklengte", regionLevels = NULL, uiText = uiText),
          plotBioindicatorUI("wbe_gewicht", bioindicator = "ontweid_gewicht", regionLevels = NULL, uiText = uiText)
        ),
        countEmbryosUI("wbe", regionLevels = NULL)
      )
    )
  )


)
