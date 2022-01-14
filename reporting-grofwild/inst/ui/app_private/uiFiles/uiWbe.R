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
    
#    tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
#      tags$ul(
#        tags$li("Bovenaan deze pagina krijgt u, in functie van de gekozen wildsoort, de regio-schaal en de gewenste jaren en periodes, een kaart met de geografische 
#            spreiding van het afschot en een figuur met de evolutie van het afschot voor de gekozen periode."), 
#        tags$li("Onder de verspreidingskaart kunnen verdere grafieken en tabellen worden opengeklikt, die telkens kunnen worden aangepast in functie van de informatie 
#            die u wenst te visualiseren."), 
#        tags$li("Onderstaande tabellen en figuren zijn gebaseerd op de beschikbare gegevens op ", format(max(as.Date(ecoData$afschot_datum), na.rm = T), 
#            "%d/%m/%Y"), ". Een deel van de data van het voorbije kwartaal kunnen dus mogelijk nog niet opgenomen zijn in de dataset."),
#        tags$li("De bron voor de kaarten, figuren en tabellen wordt gevormd door het",
#          tags$a(href = "https://www.natuurenbos.be/e-loket", "E-loket van het Agentschap voor Natuur en Bos", target = "_blank"),
#          ", gecombineerd met door het INBO uitgevoerde metingen op ingezamelde stalen (onderkaken en baarmoeders)."),
#        tags$li("De data achter de figuren en tabellen kan je steeds downloaden als ruwe data."),
#        tags$li("De figuren zelf kan je ook als .png downloaden."),
#        tags$li("Indien u fouten zou ontdekken of merkt dat data ontbreken gelieve dit dan te melden via een email naar",
#          tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie", target="_blank", "faunabeheer@inbo.be"), ".")
#      )
#    )
  
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
          HTML("<div class='fotoTitel'>Edelhert</div><div id='edelhertFoto'></div>"))
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
   
    tags$div(class = "container",
      
      h2("Extra Figuren en Tabellen"),
      
    ## tableSpecies: wild zwijn and ree
    conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'", {
        
        tagList(
          actionLink(inputId = "wbe_linkTable1",
            label = h3("TABEL: Gerapporteerd afschot per leeftijdscategorie")),
          conditionalPanel("input.wbe_linkTable1 % 2 == 1",
            
            fixedRow(
              
              column(4,
                optionsModuleUI(id = "wbe_table1", showYear = TRUE, exportData = TRUE),
                tags$p("Het gerapporteerd aantal geschoten dieren per leeftijdscategorie voor het geselecteerde jaar in combinatie met de verandering ten opzichte van de voorbije 1, 5 en 10 jaren. 
                    Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald.")
              ),
              column(8, tableModuleUI(id = "wbe_table1"))
            
            ),
            tags$hr()
          )
        )
        
      }),
  
  )
   
  
  )