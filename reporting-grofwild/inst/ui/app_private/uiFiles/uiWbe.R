# UI file for WBE page
# 
# Author: mvarewyck
###############################################################################



tagList(
  
  tags$div(class = "container",
    
    tags$br(),
    
    tags$div(align = "center",
      h1("Welkom op de wildbeheereenheid pagina.")
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
  
  tags$div(class = "container",
    
    h2("Landkaart"),
    
    ## countMap: all species
    wellPanel(
      
      fixedRow(
        column(6,
          uiOutput("wbe_year")
        ),
        column(6, 
          selectInput(inputId = "wbe_legend", "Legende (kaart)",
            choices = c("Bovenaan rechts" = "topright",
              "Onderaan rechts" = "bottomright",
              "Bovenaan links" = "topleft",
              "Onderaan links" = "bottomleft",
              "<geen>" = "none"))
        )
      ),
      actionLink(inputId = "wbe_globe", label = "Voeg landkaart toe",
        icon = icon("globe"))
    
    ),
    
    
    uiOutput("wbe_title"),
    withSpinner(leafletOutput("wbe_spacePlot")),
    tags$div(align = "center", uiOutput("wbe_stats")),
    tags$br(),
    downloadButton("wbe_download", "Download figuur", class = "downloadButton"),
    downloadButton("wbe_downloadData", "Download data", class = "downloadButton")
  
  ),
  
  tags$hr(),
  
  # Line plot
  fluidRow(
    column(6, 
      wellPanel(
        uiOutput("wbe_period"),
        selectInput(inputId = "wbe_unit", "Eenheid",
          choices = c("Aantal" = "absolute", 
            "Aantal/100ha" = "relative")),
        if (length(currentKbo) > 1)
        checkboxInput(inputId = "wbe_combinatie", 
          label = "Combineer alle geselecteerde regio's"),
      )
    ),
    
    column(6,
      uiOutput("wbe_timeTitle"),
      plotModuleUI(id = "wbe_plot1", height = "400px"),
      optionsModuleUI(id = "wbe_plot1", exportData = TRUE,
        doWellPanel = FALSE)
    )
  ),
  
  tags$hr(),
  
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