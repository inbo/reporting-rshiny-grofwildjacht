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
    
    welcomeSectionUI(id = "wbe", uiText = uiText, category = "wbe", 
      maxDate = max(ecoData$afschot_datum, na.rm = TRUE))
          
  ),
  
  # Select species
  
  tags$div(class = "container",
    
    # Map
    
    mapFlandersUI(id = "wbe", showRegion = FALSE, showCombine = FALSE,
      uiText = uiText, type = "wbe", plotDetails = "biotoop"),
    

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
      
      mapSchadeUI(id = "wbe_afschot",
        uiText = uiText,
        filterSource = FALSE, filterAccuracy = TRUE,
        variableChoices = c(
          "Seizoen" = "season",
          "Jaar" = "afschotjaar",
          "Jachtmethode" = "jachtmethode_comp"),
        type = "wbe",
        outputFunction = "mapAfschotUI"
      ),
      
      conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'",
          countYearShotUI(id = "wbe_labeltype", groupVariable = "leeftijd_comp", uiText = uiText)          
        ),
      
      countYearShotUI(id = "wbe_jachtmethode", groupVariable = "jachtmethode_comp", uiText = uiText)
    ),
    
    conditionalPanel("output.wbe_emptySchade == false",
      # When no afschot, might still be schadeData
      
        mapSchadeUI(id = "wbe",
          uiText = uiText, specie = "",
          filterCode = TRUE, filterSubcode = TRUE, type = "",
          plotDetails = "region")
  
    ),
    
    conditionalPanel("output.wbe_emptyAfschot == false",
      countAgeGenderUI(id = "wbe", uiText = uiText),
      countAgeCheekUI(id = "wbe", showAccuracy = TRUE, uiText = uiText),
      
      conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'",
        countYearAgeUI(id = "wbe", uiText = uiText, showRegion = FALSE)
      ),
      
      conditionalPanel("input.wbe_species == 'Ree'",
        ageGenderLowerJawUI(id = "wbe", regionLevels = NULL, uiText = uiText),    
        percentageRealisedShotUI(id = "wbe", showAccuracy = TRUE, uiText = uiText)
      ),

      bioindicatorSection(id = "wbe", uiText = uiText),
      
      conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'",
        conditionalPanel("input.wbe_species == 'Ree'",
          plotBioindicatorUI("wbe_onderkaak", bioindicator = "onderkaaklengte", 
            regionLevels = NULL, showAccuracy = TRUE, uiText = uiText),
          plotBioindicatorUI("wbe_gewicht", bioindicator = "ontweid_gewicht", 
            regionLevels = NULL, uiText = uiText)
        )
      ),
      uiOutput("wbe_embryos")
    
    )
  )


)
