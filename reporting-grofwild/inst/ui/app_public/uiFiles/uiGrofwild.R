# UI file for grofwild summary statistics
# 
# Author: mvarewyck
###############################################################################


tagList(
    
    tags$div(class = "container",
        
        tags$br(),
        
        tags$div(align = "center",
            h1("Welkom op de informatiepagina rond grofwildjacht")
        ),
        
        welcomeSection(id = "wild", uiText = uiText),
        
        tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
            tags$ul(
                tags$li("Bovenaan deze pagina krijgt u, in functie van de gekozen wildsoort, de regio-schaal en de gewenste jaren en periodes, een kaart met de geografische 
                        spreiding van het afschot en een figuur met de evolutie van het afschot voor de gekozen periode."), 
                tags$li("Onder de verspreidingskaart kunnen verdere grafieken en tabellen worden opengeklikt, die telkens kunnen worden aangepast in functie van de informatie 
                        die u wenst te visualiseren."), 
                tags$li("Onderstaande tabellen en figuren zijn gebaseerd op de beschikbare gegevens op ", format(max(as.Date(ecoData$afschot_datum), na.rm = T), 
                        "%d/%m/%Y"), ". Een deel van de data van het voorbije kwartaal kunnen dus mogelijk nog niet opgenomen zijn in de dataset."),
                tags$li("De bron voor de kaarten, figuren en tabellen wordt gevormd door het",
                    tags$a(href = "https://www.natuurenbos.be/e-loket", "E-loket van het Agentschap voor Natuur en Bos", target = "_blank"),
                    ", gecombineerd met door het INBO uitgevoerde metingen op ingezamelde stalen (onderkaken en baarmoeders)."),
                tags$li("De data achter de figuren en tabellen kan je steeds downloaden als ruwe data."),
                tags$li("De figuren zelf kan je ook als .png downloaden."),
                tags$li("Indien u fouten zou ontdekken of merkt dat data ontbreken gelieve dit dan te melden via een email naar",
                    tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie", target="_blank", "faunabeheer@inbo.be"), ".")
            )
        )
    
    
    ),
    
    # Select species
    
    tags$div(class = "container",
        
        h2("Grofwildsoort")),
    
    tags$div(class = "container", 
        
        align = "center", 
        tags$div(class = "noButton",
            radioButtons(inputId = "wild_species", label = "", inline = TRUE,
                choiceValues = list("Wild zwijn", "Ree", "Damhert", "Edelhert"),
                choiceNames = list(
                    HTML("<div class='fotoTitel'>Wild zwijn</div><div id='wildZwijnFoto'></div>"),
                    HTML("<div class='fotoTitel'>Ree</div><div id='reeFoto'></div>"),
                    HTML("<div class='fotoTitel'>Damhert</div><div id='damhertFoto'></div>"),
                    HTML("<div class='fotoTitel'>Edelhert</div><div id='edelhertFoto'></div>"))
            )
        )
    ),
    
    
    mapFlandersUI(id = "wild", plotDetails = c("flanders", "region")),
    
    
    # Show user input module per plot
    
    tags$div(class = "container",
        
        h2("Extra Figuren en Tabellen"),
        
        ## tableProvince for "leeftijd": wild zwijn and ree
        conditionalPanel("input.wild_species == 'Wild zwijn' || input.wild_species == 'Ree'", {
              
             tableProvinceUI(id = "wild", uiText = uiText)
              
            }),
        

        ## countYearProvince: all species
        countYearProvinceUI(id = "wild", uiText = uiText),
        
        ## countAgeCheek & countYearAge & percentageYearlyShotAnimals
        ## countAgeGender & boxAgeWeight
        ## - Wild zwijn and Ree
        conditionalPanel("input.wild_species == 'Wild zwijn' || input.wild_species == 'Ree'", {
              
              tagList(
                  countAgeCheekUI(id = "wild", uiText = uiText),
                  
                  countYearAgeUI(id = "wild", uiText = uiText),
                  
                  countYearShotUI(id = "wild_leeftijd", groupVariable = "leeftijd_comp",
                    regionLevels = c(1:2,4), uiText = uiText),
                  
                  yearlyShotAnimalsUI(id = "wild", uiText = uiText),
                  
                  countAgeGenderUI(id = "wild", uiText = uiText),
                  
                  boxAgeWeightUI(id = "wild", uiText = uiText)
              )
              
            }),
        
        
        ## boxAgeGenderLowerJaw
        ## - Ree
        conditionalPanel("input.wild_species == 'Ree'", 
          ageGenderLowerJawUI(id = "wild", regionLevels = 1:2, uiText = uiText)
        ),
        
        ## plot 11: Afschot per jachtmethode
        countYearShotUI(id = "wild_jachtmethode", groupVariable = "jachtmethode_comp",
          regionLevels = c(1:2,4), uiText = uiText),
        
        conditionalPanel("input.wild_species == 'Ree'",
          percentageRealisedShotUI(id = "wild", uiText = uiText, regionLevels = 1:2),
          boxRealisedShotUI(id = "wild", uiText = uiText, regionLevels = 1:2)
        ),
        
        conditionalPanel("input.wild_species == 'Wild zwijn' || input.wild_species == 'Ree'", {
            
            tagList(
              
              bioindicatorSection(id = "wild", uiText = uiText),
              
              conditionalPanel("input.wild_species == 'Ree'",
                plotBioindicatorUI(id = "wild_onderkaak", bioindicator = "onderkaaklengte", regionLevels = 1:2, uiText = uiText),
                plotBioindicatorUI(id = "wild_gewicht", bioindicator = "ontweid_gewicht", regionLevels = 1:2, uiText = uiText),
              ),
              
              countEmbryosUI(id = "wild", regionLevels = 1:2)
            
            )
          
            })
          
    )

)
