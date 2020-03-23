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
                
                
                
                tags$div(align = "center",
                        h1("Welkom op de informatiepagina rond wildschade"),
                        h1("van het Instituut voor Natuur- en Bosonderzoek (INBO)")
                ),
                
                tags$p(class = "lead", "Op deze pagina kan je de beschikbare gegevens over faunaschade raadplegen. Onder schade verstaan we verkeersongelukken met wilde dieren en schade aan landbouwgewassen, tuinen of sportvelden."),
                
                tags$p("Deze gegevens komen uit het Meldpunt-schaderegistratie van het Agentschap voor Natuur en Bos (E-loket), het project Dieren onder de wielen (Natuurpunt) en meldingen van verkeersongevallen met wilde dieren uit andere bronnen."),
                
                tags$p("Met het keuzemenu bovenaan de pagina kan je een keuze maken uit de diersoort(en) en de types schade (voertuig, gewas en/of andere). Binnen elk type schade  kan je verder kiezen uit enkele subcategorie\u00EBn. Je krijgt na je keuzes meteen een overzicht van alle beschikbare meldingen in de volledige dataset te zien."),
                
                tags$p("Opgelet: De gekozen filterkeuzes hebben een impact op alle volgende figuren en tabellen op deze pagina."),
                
                tags$p(paste0("Onderstaande figuren en tabellen zijn gebaseerd op de beschikbare gegevens op ", format(max(as.Date(schadeData$afschot_datum, format = "%d/%m/%Y"), na.rm = TRUE), "%d/%m/%Y"), ".")),
                
                tags$p("De data achter de figuren en tabellen kan je steeds downloaden als ruwe data. De figuren zelf kan je ook als .jpg downloaden."),
                
                tags$p("Indien je fouten ontdekt of merkt dat er data ontbreken, gelieve dit dan te melden door een email te sturen naar: ",
                        tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie-wildschade", target="_blank", "faunabeheer@inbo.be"), " ."),
                
                tags$p("Opgelet deze data en figuren geven de doorgegeven meldingen weer. Er gebeurt geen systematische terreincontrole voor deze meldingen."),
                
                tags$h2("Filter Data"),
                wellPanel(
                        fixedRow(
                                # Select species
                                column(4, selectInput(inputId = "schade_species", label = "Wildsoort",
                                                choices = schadeWildsoorten,
                                                selected = "wild zwijn",
                                                multiple = TRUE,
                                                width = "100%"
                                        )),
                                
                                # Select type schade
                                column(4, selectInput(inputId = "schade_code", label = "Type Schade",
                                                choices = fullNames(x = schadeTypes),
                                                selected = schadeTypes,
                                                multiple = TRUE,
                                                width = "100%"
                                        )),
                                
                                # Select gewas
                                column(4, uiOutput("schade_subcode"))
                        )
                ),
                
                uiOutput("schade_summary")
        
        ),
        
        
        # Summary map
        
        tags$div(class = "container",
                
                h2("Landkaart 1"),
                
                tags$p("De eerste kaart toont de geografische spreiding van de gemelde schadegevallen in Vlaanderen. De schadegevallen zijn hier telkens gegroepeerd weergegeven voor een bepaalde regio-schaal (Vlaanderen, provincies, faunabeheerzones, gemeenten, 5x5 km UTM-hok, ...)."),
                tags$p("De getoonde schadetypes hangen af van je keuze in het hoofdmenu."),
                tags$p("Je kan zelf nog kiezen welk jaar, schaalniveau en eenheid (absoluut aantal of aantal per 100 ha) je op de kaart weergegeven wil zien."),
                tags$p("Je kan op de kaart interactief een of meerdere deelgebieden selecteren waarvoor de gegevens dan in de onderstaande grafiek worden weergegeven. Ook de periode waarvoor informatie in de grafieken wordt weergegeven, kan je hier selecteren."),
                
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
                downloadButton("schade_downloadMap", "Download figuur"),
                downloadButton("schade_downloadData", "Download data"),
                
                fixedRow(
                        column(6,
                                h3("Referentie (Vlaanderen)"),
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
        
        tags$p("De onderstaande kaart geeft de geografische spreiding van de individuele schadegevallen per variabele weer. Mogelijke variabelen zijn ", tags$i("seizoen")," en ", tags$i("type schade.")," Welke schadegevallen getoond worden, hangt af van je filterkeuzes in het hoofdmenu."),
        tags$p("Voor deze kaart kan je bovendien zelf de periode bepalen die je wil weergeven. Door met de muis een bepaald schadegeval te selecteren, krijg je verdere informatie over dit schadegeval (jaar, wildsoort, gemeente, schadetype en eventueel seizoen)."),
        
        wellPanel(
                fixedRow(
                        column(6, uiOutput("schade_time2")),
                        column(6, selectInput(inputId = "schade_variable2", label = "Variabele",
                                        choices = c("Seizoen" = "season",
                                                    "Type schade" = "schadeCode")),
                                  selectInput(inputId = "schade_legend2", "Legende (kaart)",
                                        choices = c("Bovenaan rechts" = "topright",
                                                "Onderaan rechts" = "bottomright",
                                                "Bovenaan links" = "topleft",
                                                "Onderaan links" = "bottomleft",
                                                "<geen>" = "none"))
                        )
                ),
                actionLink(inputId = "schade_globe2", label = "Voeg landkaart toe",
                        icon = icon("globe"))
        ),
        
        uiOutput("schade_titlePerceel"),        
        withSpinner(leafletOutput("schade_perceelPlot")),
        tags$br(),
        downloadButton("schade_downloadPerceelMap", "Download figuur"),
        downloadButton("schade_downloadPerceelmapData", "Download data"),
        
        ## countYearSchadeProvince: all species
        actionLink(inputId = "schade_linkPlot1",
                label = h3("FIGUUR: Gerapporteerd aantal schadegevallen per jaar en per regio")),
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
                label = h3("FIGUUR: Gerapporteerd aantal schadegevallen per jaar en variabele")),
        conditionalPanel("input.schade_linkPlot2 % 2 == 1",
                fixedRow(
                        
                        column(4,
                                optionsModuleUI(id = "schade_plot2", 
                                        summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
                                        showTime = TRUE, showType = TRUE, exportData = TRUE),
                                tags$p("Deze figuur geeft op basis van de filterkeuzes uit het keuzemenu bovenaan de pagina het aantal schadegevallen weer in functie van het veld dat je kiest bij Variabele. Je kan kiezen tussen het ", tags$i("gewas"), "(indien gekozen werd voor gewasschade), de ", tags$i("wildsoort(en)"), " binnen de gekozen dataset, of de ", tags$i("subcategorie type schade"), "(bv. verkeersongelukken met of zonder personenletsels). Je kan een keuze maken tussen het absoluut aantal schadegevallen per categorie en het relatief aantal schadegevallen."),
                                tags$p("Wanneer je als periode meerdere jaren kiest, worden de verschillende subcategorie\u00EBn per jaar in een balk boven elkaar weergegeven. Wanneer je slechts 1 jaar kiest, worden ze naast elkaar weergegeven.")
                        ),
                        column(8, plotModuleUI(id = "schade_plot2"))
                
                ),
                tags$hr()
        ),
        
        ## tableSchadeCode
        actionLink(inputId = "schade_linkTable2",
                label = h3("TABEL: Gerapporteerde aantal schadegevallen per type schade ")),
        conditionalPanel("input.schade_linkTable2 % 2 == 1",
                fixedRow(
                        
                        column(4,
                                optionsModuleUI(id = "schade_table2", showTime = TRUE, showType = TRUE, exportData = TRUE),
                                tags$p("Op basis van de filterkeuzes uit het keuzemenu bovenaan de pagina geeft deze tabel, opgedeeld per subcategorie, het aantal schadegevallen weer. Je kan hierbij zelf de periode en het schaalniveau kiezen", tags$i("(Vlaanderen, provincie, faunabeheerzone)"), ".")
                        ),
                        column(8, datatableModuleUI(id = "schade_table2"))
                
                ),
                tags$hr()
        ),
        
        ## tableGewas
        actionLink(inputId = "schade_linkTable1",
                label = h3("TABEL: Gerapporteerd aantal schadegevallen per gewas")),
        conditionalPanel("input.schade_linkTable1 % 2 == 1",
                fixedRow(
                        
                        column(4, 
                                optionsModuleUI(id = "gewas", showTime = TRUE, showType = TRUE, exportData = TRUE),
                                tags$p("Indien je in het keuzemenu bovenaan de pagina de optie ", tags$i("Gewasschade"), " selecteerde, geeft deze tabel het aantal schadegevallen per gewas en per regio, voor de gekozen periode, weer.")
                        ),
                        column(8, tableModuleUI(id = "gewas"))
                ),
                
                tags$hr()
        )


)