# UI file for WBE page
# 
# Author: mvarewyck
###############################################################################



tagList(
  
  tags$div(class = "container",
    
    tags$br(),
    
    tags$div(align = "center",
      uiOutput("wbe_title")
    ),
    
    welcomeSection(id = "wbe", uiText = uiText)
  
  ),
  
  # Select species
  
  tags$div(class = "container",
    
    # Map
    
    mapFlandersUI(id = "wbe", showRegion = FALSE, showCombine = FALSE,
      type = "wbe", plotDetails = "biotoop"),
    

    # Choose species
    
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
  
  
  trendYearRegionUI(id = "wbe", 
    unitChoices = c("Aantal" = "absolute", 
      "Aantal/100ha" = "relative", 
      "Aantal/100ha bos & natuur" = "relativeDekking")
  ),
  
  tableSpeciesUI(id = "wbe", uiText = uiText),
  
  tags$div(class = "container",
    
    h2("Extra Figuren en Tabellen"),
    
    ## tableSpecies: wild zwijn and ree
    conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'", {
        
        countYearShotUI(id = "wbe_labeltype", groupVariable = "labeltype", uiText = uiText)
        
      }),
    
    
    countYearShotUI(id = "wbe_jachtmethode", groupVariable = "jachtmethode_comp", uiText = uiText),
    
    mapSchadeUI(id = "wbe", filterCode = TRUE, filterSubcode = TRUE, uiText = uiText,
      plotDetails = "region"),
    
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