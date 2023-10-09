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
        
        welcomeSectionUI(id = "schade", uiText = uiText, maxDate = max(schadeData$afschot_datum, na.rm = TRUE)),
        
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
            ),
            uiOutput("schade_warning")
        ),
        
        
        h2("Schadegevallen"),
        
        uiOutput("schade_summary"),
        
        tags$hr()
        
    ),
    
    
    # Summary map
    

  shinyjs::hidden(tags$div(id = "schade_results", class = "container",
      
      
      h2("Landkaart 1"),
      
      ## mapFlanders
      mapFlandersUI(id = "schade", 
        sourceChoices = names(loadMetaSchade()$sources), 
        type = "wildschade", 
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
