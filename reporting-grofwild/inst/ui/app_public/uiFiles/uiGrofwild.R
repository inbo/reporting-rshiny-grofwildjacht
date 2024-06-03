# UI file for grofwild summary statistics
# 
# Author: mvarewyck
###############################################################################


tagList(
    
    tags$div(class = "container",
        
        tags$br(),
        
        tags$div(align = "center",
            h1("Welkom op de informatiepagina rond grofwildjacht")
        ),
        
        welcomeSectionUI(id = "wild", uiText = uiText, 
          maxDate = max(ecoData$afschot_datum, na.rm = TRUE))
    
    ),
    
    # Select species
    
    tags$div(class = "container",
        
        h2("Grofwildsoort")),
    
    tags$div(class = "container", 
        
        align = "center", 
        tags$div(class = "noButton",
            radioButtons(inputId = "wild_species", label = "", inline = TRUE,
                choiceValues = list("Wild zwijn", "Ree", "Damhert", "Edelhert"),
                choiceNames = list(
                    HTML("<div class='fotoTitel'>Wild zwijn</div><div id='wildZwijnFoto'></div>"),
                    HTML("<div class='fotoTitel'>Ree</div><div id='reeFoto'></div>"),
                    HTML("<div class='fotoTitel'>Damhert</div><div id='damhertFoto'></div>"),
                    HTML("<div class='fotoTitel'>Edelhert</div><div id='edelhertFoto'></div>"))
            )
        )
    ),
    
    
    mapFlandersUI(id = "wild", plotDetails = c("flanders", "region")),
    
    
    # Show user input module per plot
    
    tags$div(class = "container",
        
        h2("Extra Figuren en Tabellen"),
        
        tableProvinceUI(id = "wild", uiText = uiText),
        
        countYearProvinceUI(id = "wild", uiText = uiText),
        countAgeCheekUI(id = "wild", uiText = uiText),
        
        conditionalPanel("input.wild_species == 'Wild zwijn' || input.wild_species == 'Ree'",
          countYearAgeUI(id = "wild", uiText = uiText)
        ),
        
        countYearShotUI(id = "wild_leeftijd", groupVariable = "leeftijd_comp",
          regionLevels = c(1:2,4), uiText = uiText),
        
        conditionalPanel("input.wild_species == 'Wild zwijn' || input.wild_species == 'Ree'",
          yearlyShotAnimalsUI(id = "wild", uiText = uiText)
        ),
        
        countAgeGenderUI(id = "wild", uiText = uiText),
        boxAgeWeightUI(id = "wild", uiText = uiText),
        
        conditionalPanel("input.wild_species == 'Ree'", 
          ageGenderLowerJawUI(id = "wild", regionLevels = c(1:2, 4), uiText = uiText)
        ),
        
        countYearShotUI(id = "wild_jachtmethode", groupVariable = "jachtmethode_comp",
          regionLevels = c(1:2, 4), uiText = uiText),
        
        conditionalPanel("input.wild_species == 'Ree'",
          percentageRealisedShotUI(id = "wild", uiText = uiText, regionLevels = c(1:2, 4)),
          boxRealisedShotUI(id = "wild", uiText = uiText, regionLevels = c(1:2, 4))
        ),
        
        bioindicatorSection(id = "wild", uiText = uiText),
        
        conditionalPanel("input.wild_species == 'Wild zwijn' || input.wild_species == 'Ree'", {
            
            tagList(
              
              conditionalPanel("input.wild_species == 'Ree'",
                plotBioindicatorUI(id = "wild_onderkaak", bioindicator = "onderkaaklengte", 
                  regionLevels = c(1:2, 4), showAccuracy = TRUE, uiText = uiText),
                plotBioindicatorUI(id = "wild_gewicht", bioindicator = "ontweid_gewicht", 
                  regionLevels = c(1:2, 4), uiText = uiText),
              ),
            
            )
            
          }),
        
        countEmbryosUI(id = "wild", regionLevels = c(1:2, 4))
          
    )

)
