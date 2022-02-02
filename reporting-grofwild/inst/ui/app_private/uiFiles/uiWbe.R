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
  
  fixedRow(
    
    column(4,
      optionsModuleUI(id = "wbe_table1", showYear = TRUE, exportData = TRUE),
      tags$p("Het gerapporteerd aantal geschoten dieren per leeftijdscategorie voor het geselecteerde jaar in combinatie met de verandering ten opzichte van de voorbije 1, 5 en 10 jaren. 
          Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald.")
    ),
    column(8, tableModuleUI(id = "wbe_table1"))
  
  ),
  tags$hr(),
  
  tags$div(class = "container",
    
    h2("Extra Figuren en Tabellen"),
    
    ## tableSpecies: wild zwijn and ree
    conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'", {
        
        tagList(
                 
          actionLink(inputId = "wbe_linkPlot2",
            label = h3("FIGUUR: Verdeling afschot over de jaren")),
          conditionalPanel("input.wbe_linkPlot2 % 2 == 1",
            fixedRow(
              
              column(4,
                optionsModuleUI(id = "wbe_plot2",
                  showTime = TRUE,
                  exportData = TRUE,
                  showType = TRUE,
                  showInterval = TRUE),
                tags$p("Verdeling van afschot over de jaren heen opgesplitst per interval"),
              ),
              column(8, plotModuleUI(id = "wbe_plot2"))
            ),
            tags$hr(),
          )
        
        ) 
        
      }),
      
      
      conditionalPanel("input.wbe_species == 'Ree'", {
        
      tagList(
        actionLink(inputId = "wbe_linkPlot3", label =
            h3("FIGUUR: Afschot per jachtmethode")),
        conditionalPanel("input.wbe_linkPlot3 % 2 == 1",
          fixedRow(
            
            column(4,
              optionsModuleUI(id = "wbe_plot3", showTime = TRUE, 
                regionLevels = c(1:2,4), exportData = TRUE),
              tags$p("Aandeel van afschot per jachtmethode over de jaren heen.")
            ),
            column(8, plotModuleUI(id = "wbe_plot3"))
          
          ),
          tags$hr()
        )
      )
      
    }),
  
  mapSchadeUI(id = "wbe", filterCode = TRUE, filterSubcode = TRUE),
  
  countAgeGenderUI(id = "wbe"),
  countAgeCheekUI(id = "wbe"),
  
  conditionalPanel("input.wbe_species == 'Ree'", {
      tagList(
        ageGenderLowerJawUI(id = "wbe", regionLevels = NULL),    
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