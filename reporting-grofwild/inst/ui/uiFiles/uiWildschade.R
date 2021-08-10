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
                
                tags$p(class = "lead", "Op deze pagina kan je de beschikbare gegevens over faunaschade raadplegen. Onder schade verstaan we verkeersongelukken met wilde dieren en schade aan landbouwgewassen, tuinen of sportvelden."),
                
                tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
                    tags$ul(
                        tags$li("Met het keuzemenu bovenaan de pagina kan je een keuze maken uit de diersoort(en) en de types schade (voertuig, gewas en/of andere). 
																 Binnen elk type schade kan je verder kiezen uit enkele subcategorieën. Je krijgt na je keuzes meteen een overzicht van alle beschikbare meldingen in de volledige dataset te zien."), 
                        tags$li("Opgelet: Pas nadat je voor de diersoort en type schade minstens één categorie hebt aangeduid, zullen de figuren en tabellen aangemaakt worden."), 
                        tags$li("Opgelet: De filterkeuzes hebben een impact op alle volgende figuren en tabellen op deze pagina."),
                        tags$li("Onderstaande figuren en tabellen zijn gebaseerd op de beschikbare gegevens op ", format(max(as.Date(schadeData$afschot_datum, format = "%d/%m/%Y"), na.rm = TRUE), "%d/%m/%Y"), "."),
                        tags$li("Deze gegevens komen uit het Meldpunt-schaderegistratie van het Agentschap voor Natuur en Bos (E-loket), het project Dieren onder de wielen (Natuurpunt) en meldingen van verkeersongevallen met wilde dieren uit andere bronnen."),
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
                                                choices = fullNames(x = schadeTypes),
                                                selected = NULL, #schadeTypes
                                                multiple = TRUE,
                                                width = "100%"
                                        )),
                                
                                # Select gewas & voertuig
                                column(4, uiOutput("schade_subcode"))
                        )
                ),
                
                h2("Schademeldingen"),
                
                uiOutput("schade_summary")
        
        ),
        
        tags$hr(),
        
        
        # Summary map
        
        tags$div(class = "container",
                
            
                h2("Landkaart 1"),
                
                tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
                    tags$ul(
                        tags$li("De eerste kaart toont de geografische spreiding van de gemelde schadegevallen in Vlaanderen. De schadegevallen zijn hier telkens gegroepeerd weergegeven voor een bepaalde regio-schaal (Vlaanderen, provincies, faunabeheerzones, gemeenten, 5x5 km UTM-hok, ...)."), 
                        tags$li("De getoonde schadetypes hangen af van je keuze in het hoofdmenu."), 
                        tags$li("Je kan zelf nog kiezen welk jaar, schaalniveau en eenheid (absoluut aantal of aantal per 100 ha) je op de kaart weergegeven wil zien."),
                        tags$li("Je kan op de kaart interactief een of meerdere deelgebieden selecteren waarvoor de gegevens dan in de onderstaande grafiek worden weergegeven. Ook de periode waarvoor informatie in de grafieken wordt weergegeven, kan je hier selecteren.")
                    )
                ),
                                
                ## mapFlanders
                wellPanel(
                        fixedRow(
                                column(4, selectInput(inputId = "schade_regionLevel", label = "Regio-schaal",
                                                choices = c(
                                                        "Vlaanderen" = "flanders",
                                                        "Provincie" = "provinces", 
                                                        "Faunabeheerzones" = "faunabeheerzones",
                                                        "Gemeente (binnen provincie)" = "communes",
                                                        "Gemeente (binnen faunabeheerzone)" = "fbz_gemeentes",
                                                        "5x5 UTM" = "utm5"
                                                ),
                                                selected = "communes")),
                                column(8, uiOutput("schade_region"))
                        ),
                        
                        fixedRow(
                                column(6, uiOutput("schade_year")),
                                column(6, uiOutput("schade_time"))
                        ),
                        
                        fixedRow(
                                column(6,
                                        selectInput(inputId = "schade_legend", "Legende (kaart)",
                                                choices = c("Bovenaan rechts" = "topright",
                                                        "Onderaan rechts" = "bottomright",
                                                        "Bovenaan links" = "topleft",
                                                        "Onderaan links" = "bottomleft",
                                                        "<geen>" = "none"))
                                ),
                                column(6, 
                                        selectInput(inputId = "schade_unit", "Eenheid",
                                                choices = c("Aantal" = "absolute", 
                                                        "Aantal/100ha" = "relative"))
                                )
                        ),
                        
                        actionLink(inputId = "schade_globe", label = "Voeg landkaart toe",
                                icon = icon("globe"))
                
                ),
                
                
                uiOutput("schade_title"),
                withSpinner(leafletOutput("schade_spacePlot")),
                tags$br(),
                downloadButton("schade_downloadMap", "Download figuur", class = "downloadButton"),
                downloadButton("schade_downloadData", "Download data", class = "downloadButton"),
                
                fixedRow(
                        column(6,
                                h3("Evolutie schademeldingen Vlaanderen"),
                                plotModuleUI(id = "schade_timePlotFlanders", height = "400px"),
                                optionsModuleUI(id = "schade_timePlotFlanders", exportData = TRUE,
                                        doWellPanel = FALSE)
                        ),
                        column(6,
                                uiOutput("schade_timeTitle"),
                                plotModuleUI(id = "schade_timePlot", height = "400px"),
                                optionsModuleUI(id = "schade_timePlot", exportData = TRUE,
                                        doWellPanel = FALSE)
                        )
                )
        
        ),
        
        tags$hr(),
        
        h2("Landkaart 2"),
        
        tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
            tags$ul(
                tags$li("De onderstaande kaart geeft de geografische spreiding van de individuele schadegevallen per variabele weer. Mogelijke variabelen zijn ", tags$i("seizoen, jaar")," en ", tags$i("type schade.")," Welke schademeldingen getoond worden, hangt af van je filterkeuzes in het keuzemenu."), 
                tags$li("Voor deze kaart kan je bovendien zelf de periode bepalen die je wil weergeven. Door met de muis een bepaalde schademelding te selecteren, krijg je verdere informatie over deze schademelding (jaar, wildsoort, gemeente, schadetype en eventueel seizoen).")
            )
        ),
            
        wellPanel(
                fixedRow(
                        column(6, uiOutput("schade_time2")),
                        column(6, selectInput(inputId = "schade_variable2", label = "Variabele",
                                        choices = c("Seizoen" = "season",
                                                    "Jaar" = "afschotjaar",
                                                    "Type schade" = "schadeCode")),
                                  selectInput(inputId = "schade_legend2", "Legende (kaart)",
                                        choices = c("Bovenaan rechts" = "topright",
                                                "Onderaan rechts" = "bottomright",
                                                "Bovenaan links" = "topleft",
                                                "Onderaan links" = "bottomleft",
                                                "<geen>" = "none"))
                        )
                ),
                actionLink(inputId = "schade_globe2", label = "Verberg landkaart",
                        icon = icon("globe"))
        ),
        
        uiOutput("schade_titlePerceel"),        
        withSpinner(leafletOutput("schade_perceelPlot")),
        tags$br(),
        actionButton("schade_genereerMap", "Download figuur", icon = icon("download"), class = "downloadButton"),
        singleton(
            tags$head(tags$script(src = "triggerDownload.js"))
        ),
        downloadButton("schade_downloadPerceelmapData", "Download data", class = "downloadButton"),
        downloadLink("schade_downloadPerceelMap", " "),
        tags$hr(),
        
        h2("Extra Figuren en Tabellen"),
        
        ## countYearSchadeProvince: all species
        actionLink(inputId = "schade_linkPlot1",
                label = h3("FIGUUR: Aantal schademeldingen per jaar en per regio")),
        conditionalPanel("input.schade_linkPlot1 % 2 == 1",
                fixedRow(
                        
                        column(4,
                                optionsModuleUI(id = "schade_plot1", 
                                        showTime = TRUE, showType = TRUE, exportData = TRUE),
                                tags$p("Deze figuur geeft op basis van de filterkeuzes uit het keuzemenu bovenaan de pagina, het aantal schadegevallen weer per gekozen schaalniveau (keuzevak regio: ", tags$i("Vlaanderen, provincies"), " of ", tags$i("faunabeheerzones"), ")."),
                                tags$p("Wanneer je als periode meerdere jaren kiest, worden de verschillende deelgebieden per jaar in een balk boven elkaar weergegeven. Wanneer je slechts 1 jaar kiest, worden de verschillende geografische deelgebieden naast elkaar weergegeven.")
                        ),
                        column(8, plotModuleUI(id = "schade_plot1"))
                
                ),
                tags$hr()
        ),
        
        ## countYearSchade: all species
        actionLink(inputId = "schade_linkPlot2",
                label = h3("FIGUUR: Aantal schademeldingen per jaar en variabele")),
        conditionalPanel("input.schade_linkPlot2 % 2 == 1",
                fixedRow(
                        
                        column(4,
                                optionsModuleUI(id = "schade_plot2", 
                                        summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
                                        showTime = TRUE, showType = TRUE, exportData = TRUE),
                                tags$p("Deze figuur geeft op basis van de filterkeuzes uit het keuzemenu bovenaan de pagina het aantal schademeldingen weer in functie van het veld dat je kiest bij Variabele. Je kan kiezen tussen het ", tags$i("gewas"), "(indien gekozen werd voor gewasschade), de ", tags$i("wildsoort(en)"), " binnen de gekozen dataset, of de ", tags$i("subcategorie type schade"), "(bv. verkeersongelukken met of zonder personenletsels). Je kan een keuze maken tussen het absoluut aantal schademeldingen per categorie en het relatief aantal schademeldingen."),
                                tags$p("Wanneer je als periode meerdere jaren kiest, worden de verschillende subcategorie\u00EBn per jaar in een balk boven elkaar weergegeven. Wanneer je slechts 1 jaar kiest, worden ze naast elkaar weergegeven.")
                        ),
                        column(8, plotModuleUI(id = "schade_plot2"))
                
                ),
                tags$hr()
        ),
        
        ## tableSchadeCode
        actionLink(inputId = "schade_linkTable2",
                label = h3("TABEL: Aantal schademeldingen per type schade")),
        conditionalPanel("input.schade_linkTable2 % 2 == 1",
                fixedRow(
                        
                        column(4,
                                optionsModuleUI(id = "schade_table2", showTime = TRUE, showType = TRUE, exportData = TRUE),
                                tags$p("Op basis van de filterkeuzes uit het keuzemenu bovenaan de pagina geeft deze tabel, opgedeeld per subcategorie, het aantal schademeldingen weer. Je kan hierbij zelf de periode en het schaalniveau kiezen", tags$i("(Vlaanderen, provincie, faunabeheerzone)"), ".")
                        ),
                        column(8, datatableModuleUI(id = "schade_table2"))
                
                ),
                tags$hr()
        ),
        
        ## tableGewas
        actionLink(inputId = "schade_linkTable1",
                label = h3("TABEL: Aantal schademeldingen per gewas")),
        conditionalPanel("input.schade_linkTable1 % 2 == 1",
                fixedRow(
                        
                        column(4, 
                                optionsModuleUI(id = "gewas", showTime = TRUE, showType = TRUE, exportData = TRUE),
                                tags$p("Indien je in het keuzemenu bovenaan de pagina de optie ", tags$i("Gewasschade"), " selecteerde, geeft deze tabel het aantal schademeldingen per gewas en per regio, voor de gekozen periode, weer.")
                        ),
                        column(8, tableModuleUI(id = "gewas"))
                ),
                
                tags$hr()
        )


)