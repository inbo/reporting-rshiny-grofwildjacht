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
    
    tags$p(class = "lead", "Hier vindt u gedetailleerde info mbt tot uw wbe. Deze info is wachtwoord beschermd. U kan een login bekomen via HVV of de wbe secretaris."),
   
  ),
  
  # Select species
  
  tags$div(class = "container",
    
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
          HTML("<div class='fotoTitel'>Edelhert</div><div id='edelhertFoto'></div>")),
        selected = "Ree"
      )
    )
  ),
  
  
  # Map
  
  mapFlandersUI(id = "wbe", showRegion = FALSE, showCombine = FALSE,
    plotDetails = c("region", "biotoop")),
  
  tableSpeciesUI(id = "wbe"),
  
  tags$div(class = "container",
    
    h2("Extra Figuren en Tabellen"),
    
    ## tableSpecies: wild zwijn and ree
    conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'", {
        
        countYearShotUI(id = "wbe")
        
      }),
      
      
      conditionalPanel("input.wbe_species == 'Ree'", {
        
          countHuntingMethodUI(id = "wbe")
      
    }),
  
  mapSchadeUI(id = "wbe", filterCode = TRUE, filterSubcode = TRUE),
  
  countAgeGenderUI(id = "wbe"),
  countAgeCheekUI(id = "wbe", showAccuracy = TRUE),
  
  conditionalPanel("input.wbe_species == 'Ree'", {
      tagList(
        
        ageGenderLowerJawUI(id = "wbe", regionLevels = NULL),    
        percentageRealisedShotUI(id = "wbe", showAccuracy = TRUE),    
        
        h2("Bio-indicatoren"),
        tags$p("Bio-indicatoren zijn ecologische parameters, die betrekking hebben op de relatie tussen een populatie en de draagkracht van het gebied en gevoelig zijn voor veranderingen in populatieaantallen en/of in de draagkracht van het gebied.
            In dit geval dus de relatie tussen het aantal reeën in een gebied en de draagkracht van dat gebied. Voor ree werd aangetoond dat van zodra de draagkracht van een gebied wordt benaderd, dit zich vertaalt in kleinere reekitsen (lichtere gewichten en kortere onderkaken), een lager percentage drachtige geiten en smalreeën en in gemiddeld kleinere worpen."),
        
        plotBioindicatorUI("wbe_onderkaak", bioindicator = "onderkaaklengte", regionLevels = NULL),
        
        plotBioindicatorUI("wbe_gewicht", bioindicator = "ontweid_gewicht", regionLevels = NULL),
        
        countEmbryosUI("wbe", regionLevels = NULL)
      )
    })
  
  
  )


)