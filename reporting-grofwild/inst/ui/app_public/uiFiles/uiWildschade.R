# UI file for wildschade summary statistics
# 
# Author: mvarewyck
###############################################################################



tagList(
    
#        tags$div(class = "watermark container",
#                
#                tags$div(style = "margin-top: 50px",
#                        tags$em("Voorlopige versie"))
#                        
#        ),
    
    
    tags$div(class = "container",
        
        tags$br(),
        
        tags$div(align = "center",
            h1("Welkom op de informatiepagina rond wildschade")
        ),
        
        welcomeSection(id = "schade", uiText = uiText),
        
        tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
            tags$ul(
                tags$li("Met het keuzemenu bovenaan de pagina kan je een keuze maken uit de diersoort(en) en de types schade (voertuig, gewas en/of andere). 
                        Binnen elk type schade kan je verder kiezen uit enkele subcategorieën. Je krijgt na je keuzes meteen een overzicht van alle beschikbare meldingen in de volledige dataset te zien."), 
                tags$li("Opgelet: Pas nadat je voor de diersoort en type schade minstens één categorie hebt aangeduid, zullen de figuren en tabellen aangemaakt worden."), 
                tags$li("Opgelet: De filterkeuzes hebben een impact op alle volgende figuren en tabellen op deze pagina."),
                tags$li("Onderstaande figuren en tabellen zijn gebaseerd op de beschikbare gegevens op ", format(max(as.Date(schadeData$afschot_datum, format = "%d/%m/%Y"), na.rm = TRUE), "%d/%m/%Y"), "."),
                tags$li("Deze gegevens komen uit het Meldpunt-schaderegistratie van het e-loket fauna en flora (Agentschap voor Natuur en Bos), waarnemingen.be (Natuurpunt), Wilder (Hubertus Vereniging Vlaanderen) en schademeldingen uit andere bronnen."),
                tags$li("De data achter de figuren en tabellen kan je steeds downloaden als ruwe data."),
                tags$li("De figuren zelf kan je ook als .png downloaden."),
                tags$li("Opgelet: Deze data en figuren geven de meldingen weer zoals ze ingegeven werden. Er gebeurt op dit moment geen systematische terreincontrole voor deze meldingen, ook wordt er niet gecontroleerd op eventuele dubbele meldingen van hetzelfde schadegeval."),
                tags$li("Indien je fouten ontdekt of merkt dat er data ontbreken, gelieve dit dan te melden door een email te sturen naar: ",
                    tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie-wildschade", target="_blank", "faunabeheer@inbo.be"), " .")
            )
        ),
        
        tags$h2("Keuzemenu"),
        wellPanel(
            fixedRow(
                # Select species
                column(4, selectInput(inputId = "schade_species", label = "Wildsoort",
                        choices = schadeWildsoorten,
                        selected = NULL, #"wild zwijn"
                        multiple = TRUE,
                        width = "100%"
                    )),
                
                # Select type schade
                column(4, selectInput(inputId = "schade_code", label = "Type Schade",
                        choices = schadeTypes,
                        selected = NULL, #schadeTypes
                        multiple = TRUE,
                        width = "100%"
                    )),
                
                # Select gewas & voertuig
                column(4, uiOutput("schade_subcode"))
            )
        ),
        
        h2("Schademeldingen"),
        
        uiOutput("schade_summary"),
        
        tags$hr()
        
    ),
    
    
    # Summary map
    
  shinyjs::hidden(tags$div(id = "schade_results", class = "container",
      
      
      h2("Landkaart 1"),
      
      ## mapFlanders
      mapFlandersUI(id = "schade", showSource = TRUE, type = "wildschade", 
        showCombine = FALSE,
        plotDetails = c("flanders", "region")),
      
      h2("Landkaart 2"),
      
      ## mapSchade
      mapSchadeUI(id = "schade", 
        uiText = uiText[uiText$plotFunction == "mapSchadeUI", ]),
      
      h2("Extra Figuren en Tabellen"),
      
      ## countYearSchadeProvince: all species
      countYearProvinceUI(id = "schade", uiText = uiText, showType = TRUE, 
        showDataSource = "schade"),
      
      ## countYearSchade: all species
      countYearSchadeUI(id = "schade", uiText = uiText),
      
      ## tableSchadeCode
      tableSchadeUI(id = "schade", uiText = uiText),
      
      ## tableGewas
      tableGewasUI(id = "schade", uiText = uiText)
    
    ))


)