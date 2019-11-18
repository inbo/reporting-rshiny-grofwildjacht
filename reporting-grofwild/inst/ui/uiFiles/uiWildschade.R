# UI file for wildschade summary statistics
# 
# Author: mvarewyck
###############################################################################


# TODO debug
# TODO module for map?


tagList(
        
        tags$div(class = "container",
                
                tags$div(align = "center",
                        h1("Welkom op de informatiepagina rond wildschade"),
                        h1("van het Instituut voor Natuur- en Bosonderzoek (INBO)")
                ),
                
                tags$p(class = "lead", "intro tekstje"),
                
                
                # Select species
                selectInput(inputId = "schade_species", label = "Wildsoort",
                        choices = list("Grof wild" = c("wild zwijn", "edelhert", "ree"),
                                "Klein wild" = c("haas", "fazant", "konijn"),
                                "Waterwild" = c("wilde eend", "smient"),
                                "Overig" = c("houtduif", "vos")),
                        selected = "wild zwijn",
                        multiple = TRUE,
                        width = "100%"
                )
        
        ),
        
        
        
        # Summary map
        
        tags$div(class = "container",
                
                h2("Landkaart 1"),
                
                ## mapFlanders
                wellPanel(
                        fixedRow(
                                column(4, selectInput(inputId = "schade_regionLevel", label = "Regio-schaal",
                                                choices = c(
                                                        "Vlaanderen" = "flanders",
                                                        "Provincie" = "provinces", 
#                                                        "Faunabeheerzones" = "faunabeheerzones",
                                                        "Gemeente (binnen provincie)" = "communes"
#                                                        "Gemeente (binnen faunabeheerzone)" = "fbz_gemeentes"
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
        
        uiOutput("schade_titlePerceel"),        
        withSpinner(leafletOutput("schade_perceelPlot"))


)