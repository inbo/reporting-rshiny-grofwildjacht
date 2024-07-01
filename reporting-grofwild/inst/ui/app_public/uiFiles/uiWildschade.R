# UI file for wildschade summary statistics
# 
# Author: mvarewyck
###############################################################################

gewasChoices <- metaSchade$codes[["GEWAS"]]
voertuigChoices <- metaSchade$codes[["VRTG"]]

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
                column(4, 
                  shinyjs::hidden(
                    selectInput(inputId = "schade_gewas", label = "Filter Gewas Schade",
                      choices = gewasChoices,
                      selected = gewasChoices,
                      multiple = TRUE,
                      width = "100%"
                    )),
                  shinyjs::hidden(
                    selectInput(inputId = "schade_voertuig", label = "Filter Voertuig Schade",
                      choices = voertuigChoices,
                      selected = voertuigChoices,
                      multiple = TRUE,
                      width = "100%"
                    ))
                )
            ),
            uiOutput("schade_warning")
        ),
        
        shinyjs::hidden(tags$div(id = "schade_summary",
          h2("Schadegevallen"),
          fixedRow(
            column(4, tableModuleUI(id = "wildsoort", includeTotal = TRUE)),
            column(4, tableModuleUI(id = "schade", includeTotal = TRUE)),
            conditionalPanel("input.schade_code.includes('GEWAS') || input.schade_code.includes('VRTG')", 
              column(4, tableModuleUI(id = "subschade",
                  includeTotal = TRUE))
            )
          )
        )),
        
        tags$hr()
        
    ),
    
    
    # Summary map
    

  shinyjs::hidden(tags$div(id = "schade_results", class = "container",
      
      
      h2("Landkaart 1"),
      
      ## mapFlanders
      mapFlandersUI(id = "schade", 
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
      conditionalPanel("input.schade_code.indexOf('GEWAS') > -1",
          tableGewasUI(id = "schade", uiText = uiText)
      )
    
    ))

)
