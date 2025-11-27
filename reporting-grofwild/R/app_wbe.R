#' Shiny server module for WBE page
#' 
#' @inheritParams reportingGrofwild-common-args
#' @param currentKbo integer, defines the KBO for which results are shown
#' @param toekenningsData data.frame, as returned by \code{loadToekenningen}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
wbeServer <- function(id, currentKbo, ecoData, geoData, schadeData,
  toekenningsData, biotoopData, spatialData, defaultYear, uiText) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns

      # For R CMD check
      wildsoort <- KboNummer_Toek <- NULL
      
      
      results <- reactiveValues()
      
      
# ------------------ #
# Filter data on KBO #
# ------------------ #

results$wbe_currentKbo <- reactive({
    
    req(input$wbe_kboChoice)
    
    if (length(currentKbo) > 1)
      req(input$wbe_kboChoice) else
      currentKbo
    
  })

results$wbe_geoDataKbo <- reactive({
    
    if (results$wbe_currentKbo() %in% geoData$KboNummer_Toek)
      subset(geoData, KboNummer_Toek %in% results$wbe_currentKbo()) else 
      # when no afschot data, still show map & biotoop
      createEmptyGeo(geoData[1, ], years = 2014:max(geoData$afschotjaar),
        kbo = results$wbe_currentKbo())
  
  })

results$wbe_currentPartij <- reactive({
    
    toReturn <- unique(results$wbe_geoDataKbo()$PartijNummer)
    toReturn[!is.na(toReturn)]
    
  })

results$wbe_schadeData <- reactive({
    
    schadeData[schadeData$KboNummer %in% results$wbe_currentKbo(), ]
    
  })

results$preSelected <- reactive({
  list(
    schade_code = reactive({
        if (subcategory() %in% gewasSubCat)
          "GEWAS" else 
          req(input$schade_code)
      }),
    schade_gewas = reactive(req(input$schade_gewas)),
    schade_voertuig = reactive(req(input$schade_voertuig)),
    time = reactive(input$time),
    year = reactive(input$year),
    interval = reactive(input$interval),
    type = reactive(input$type),
    regionLevel = reactive(input$regionLevel),
    region = reactive(input$region),
    unit = reactive(input$unit),
    summarizeBy = reactive(input$summarizeBy),
    dataSource_schade = reactive(input$dataSource_schade),
    dataSource_onderkaak = reactive(input$dataSource_onderkaak),
    dataSource_embryos = reactive(input$dataSource_embryos),
    dataSource_leeftijd = reactive(input$dataSource_leeftijd),
    dataSource_geslacht = reactive(input$dataSource_geslacht)
  )
})


## Disable species without data
# https://stackoverflow.com/a/58310568
observe({
    
    req(input$wbe_species)
    speciesChoices <- c("Wild zwijn", "Ree", "Damhert", "Edelhert")
    
    isPresent <- sapply(speciesChoices, function(iSpecies) 
        iSpecies %in% results$wbe_geoDataKbo()$wildsoort | 
          iSpecies %in% results$wbe_schadeData()$wildsoort,
      simplify  = FALSE)
    
    for (iSpecies in speciesChoices) {
      
      jsSelector <- sprintf('[type=radio][name=wbe_species][value="%s"]', iSpecies)
      
      if (!isPresent[[iSpecies]]) {
        
        if (input$wbe_species == iSpecies)
          updateRadioButtons(session = session, inputId = "wbe_species",
            selected = names(isPresent)[unlist(isPresent)][1])
        
        shinyjs::disable(selector = jsSelector)
        shinyjs::runjs(paste0("$('", jsSelector, "').parent().addClass('disabled').css('opacity', 0.4)"))
      } else {
        shinyjs::enable(selector = jsSelector)
        shinyjs::runjs(paste0("$('", jsSelector, "').parent().removeClass('disabled').css('opacity', 1)"))
      }
      
    }
    
  })

output$wbe_title <- renderUI({
    
    h1("Welkom op de wildbeheereenheid pagina voor",
      paste(unique(results$wbe_geoDataKbo()$WBE_Naam_Toek), collapse = ","))
    
  })


# Message when no data available for 
output$wbe_empty <- renderUI({
    
    errorMessage <- NULL
    
    if (!results$wbe_currentKbo() %in% geoData$KboNummer_Toek)
      errorMessage <- tags$p("Momenteel zijn er voor deze WBE geen afschotgegevens van de grofwildsoorten beschikbaar.", 
        "Hierdoor kunnen er geen figuren/tabellen worden getoond m.b.t. afschot.", 
        "Indien dit niet klopt, kijkt u best eerst na of de gegevens juist in het e-loket van ANB zitten.", 
        "Zijn uw gegevens toch ingegeven in het e-loket en hier worden ze hier niet weergegeven dan laat u best iets weten op", 
        tags$a(id = "wbe_contact", href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", target="_blank", "faunabeheer@inbo.be")) 
    
    if (!results$wbe_currentKbo() %in% schadeData$KboNummer)
      errorMessage <- tags$p(errorMessage, 
        "Momenteel zijn er voor deze WBE geen schadegegevens van de grofwildsoorten beschikbaar.", 
        "Hierdoor kunnen er geen figuren/tabellen worden getoond m.b.t. schadegevallen.",
        "Indien u denkt dat dit niet klopt, raden wij u aan gebruik te maken van het", 
        tags$a(href = "https://natuurenbos.vlaanderen.be/e-loketten", target = "_blank", "e-loket"), 
        "van ANB, de Wilderapp (", 
        tags$a(href = "https://apps.apple.com/be/app/wilder/id1478282738", target = "_blank", "ios"), 
        "of", 
        tags$a(href = "https://play.google.com/store/apps/details?id=com.wilderpg.wilder&hl=en&gl=US&pli=1", target = "_blank", "android"), 
        ") van HVV of", 
        tags$a(href = "https://waarnemingen.be/", target = "_blank", "waarnemingen.be"),
        "van Natuurpunt. Bent u zeker dat er toch gegevens bij \u00E9\u00E9n van de partners ingevoerd werden die hier niet worden weergegeven, dan laat u best iets weten op",
        tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", target="_blank", "faunabeheer@inbo.be")
      )
    
    tags$em(errorMessage)
    
  })


output$wbe_emptyAfschot <- reactive({
    !input$wbe_species %in% results$wbe_geoDataKbo()$wildsoort
  })
outputOptions(output, "wbe_emptyAfschot", suspendWhenHidden = FALSE)



# ---------------------- #
# Filter data on species #
# ---------------------- #

results$wbe_geoData <- reactive({
    
    subset(results$wbe_geoDataKbo(), wildsoort == req(input$wbe_species) & 
        KboNummer_Toek %in% results$wbe_currentKbo())
    
  })

results$wbe_combinedData <- reactive({
    
    ecoData <- subset(ecoData, wildsoort == req(input$wbe_species))
    
    # Combine data
    commonNames <- names(ecoData)[names(ecoData) %in% names(results$wbe_geoData())]
    combinedData <- merge(results$wbe_geoData(), ecoData, 
      by = commonNames, all.x = TRUE)
    
    combinedData
    
  })

results$wbe_toekenningsData <- reactive({
    
    toekenningsData[toekenningsData$KboNummer_Toek %in% results$wbe_currentKbo(), ]
    
  })

results$wbe_timeRange <- reactive({
    
    req(nrow(results$wbe_combinedData()) > 0)
    
    range(results$wbe_combinedData()$afschotjaar)
    
  })  

results$leeftijdtypes <- reactive({
    
    c(loadMetaEco(species = input$wbe_species)$leeftijd_comp_inbo, "Onbekend")
    
  })

results$jachttypes <- reactive({
    
    choices <- unique(results$wbe_combinedData()$jachtmethode_comp)
    choices[is.na(choices)] <- "onbekend"
    
    sort(choices)
    
  })






### The MAP
### -------------


mapFlandersServer(id = "wbe",
  uiText = uiText,
  defaultYear = defaultYear,
  species = reactive(""),
  currentWbe = results$wbe_currentPartij,
  type = "wbe",
  hideGlobeDefault = FALSE,
  geoData = results$wbe_geoDataKbo,  # independent of species
  biotoopData = biotoopData,
  allSpatialData = spatialData,
  preSelected = results$preSelected
)




### Extra Graphs/Tables
### -------------


## Plot1: Trend over time

trendYearRegionServer(id = "wbe",
  species = reactive(input$wbe_species),
  allSpatialData = spatialData,
  biotoopData = reactive(biotoopData),
  geoData = results$wbe_geoData, 
  locaties = reactive(unique(results$wbe_geoData()$WBE_Naam_Toek[
        match(results$wbe_geoData()$PartijNummer, results$wbe_currentPartij())])),
  preSelected = results$preSelected
)

## User input for controlling the plots and create plotly
# Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
tableSpeciesServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  species = reactive(input$wbe_species),
  uiText = uiText,
  preSelected = results$preSelected)


# Plot2: Verdeling afschot over de jaren
countYearShotServer(id = "wbe_labeltype",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  groupVariable = "leeftijd_comp",
  types = results$leeftijdtypes,
  preSelected = results$preSelected)


# Plot3: Afschot per jachtmethode
countYearShotServer(id = "wbe_jachtmethode",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  groupVariable = "jachtmethode_comp",
  types = results$jachttypes,
  preSelected = results$preSelected)


# Plot4: Schademeldingen
mapSchadeServer(id = "wbe",
  schadeData = results$wbe_schadeData, 
  allSpatialData = reactive(
    filterSpatialWbe(allSpatialData = spatialData, partijNummer = results$wbe_currentPartij())
  ), 
  timeRange = reactive({
      schadeRange <- range(results$wbe_schadeData()$afschotjaar)
      c(max(2014, schadeRange[1]), schadeRange[2])
    }), 
  defaultYear = defaultYear, 
  species = reactive(input$wbe_species),
  uiText = uiText, 
  type = "wbe",
  borderRegion = "WBE_buitengrenzen",
  preSelected = results$preSelected
)


output$wbe_mapSchade <- renderUI({
    
    req(input$wbe_species %in% results$wbe_schadeData()$wildsoort)
    
    mapSchadeUI(id = ns("wbe"),
      filterCode = TRUE, filterSubcode = TRUE, 
      plotDetails = "region")
    
  })


# Plot 5: Geslachtsverdeling binnen het afschot per leeftijdscategorie
countAgeGenderServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  preSelected = results$preSelected)

# Plot 6: Leeftijdscategorie op basis van onderkaak & meldingsformulier
countAgeCheekServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  preSelected = results$preSelected
)

countYearAgeServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  preSelected = results$preSelected)


# Plot 7: Onderkaaklengte per jaar
results$wbe_typesGender <- reactive({
    
    types <- levels(droplevels(results$wbe_combinedData()$type_comp))
    types[types != ""]
    
  })

results$wbe_typesDefaultGender <- reactive({
    grep("kits", results$wbe_typesGender(), value = TRUE)
  })

plotBioindicatorServer(id = "wbe_onderkaak",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  types = results$wbe_typesGender,
  typesDefault = results$wbe_typesDefaultGender,
  bioindicator = "onderkaaklengte",
  preSelected = results$preSelected)

# Plot 8: Gewicht per jaar
plotBioindicatorServer(id = "wbe_gewicht",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  types = results$wbe_typesGender,
  typesDefault = results$wbe_typesDefaultGender,
  bioindicator = "ontweid_gewicht",
  preSelected = results$preSelected,
  isWBE = TRUE)


## Bio indicatoren ##

bioindicatorSectionServer(
  id = "wbe", 
  uiText = uiText, 
  wildsoort = reactive(input$wbe_species)
)

# Plot 9: Gerapporteerd aantal embryo's voor vrouwelijke reeen per jaar
results$typesFemale <- reactive({
    
    types <- levels(droplevels(results$wbe_combinedData()$type_comp))
    
    types <- if (input$wbe_species == "Ree") {
      types[types %in% c("Reegeit", "Smalree")] 
    } else if (input$wbe_species == "Wild zwijn"){
      types[types %in% c("Zeug", "Overloper (v)", "Frisling (v)")]      
    } else {
      types[types %in% c("Kalf (v)", "Smaldier", "Hinde")]        
    }
  
    c(types, "Onbekend")
    
  })

countEmbryosServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = reactive(range(results$wbe_combinedData()$afschotjaar[results$wbe_combinedData()$geslacht_comp %in% "Vrouwelijk"])),
  types = results$typesFemale,
  uiText = uiText,
  wildsoort = reactive(input$wbe_species),
  preSelected = results$preSelected
)

# Plot 10: Onderkaaklengte per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
ageGenderLowerJawServer(id = "wbe",
  data = results$wbe_combinedData,
  types = reactive(switch(input$wbe_species,
      "Wild zwijn" = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
      Ree = c("Kits", "Jongvolwassen", "Volwassen")									
    )),
  timeRange = reactive(if (input$wbe_species == "Ree")
        c(2014, max(results$wbe_timeRange())) else 
        results$wbe_timeRange()),
  preSelected = results$preSelected
)


# Plot 11: Gerealiseerd afschot
percentageRealisedShotServer(id = "wbe",
  data = results$wbe_toekenningsData,
  types = reactive(unique(results$wbe_toekenningsData()$labeltype)),
  timeRange = reactive(range(results$wbe_toekenningsData()$labeljaar)),
  preSelected = results$preSelected
)


# Plot 12: Afschot locaties
mapSchadeServer(id = "wbe_afschot",
  schadeData = results$wbe_combinedData, 
  allSpatialData = reactive(filterSpatialWbe(allSpatialData = spatialData, partijNummer = results$wbe_currentPartij())),
  timeRange = results$wbe_timeRange, 
  defaultYear = defaultYear, 
  species = reactive(input$wbe_species),
  uiText = uiText,
  type = "afschot",
  borderRegion = "WBE_buitengrenzen",
  preSelected = results$preSelected
)

output$wbe_embryos <- renderUI({
    
    countEmbryosUI("wbe", regionLevels = NULL, uiText = uiText, specie = input$wbe_species,
      showTime = TRUE, showType = TRUE, showDataSource = c("embryos", "leeftijd", "geslacht"))
    
  })

# Afschot aanvraag Reewild
requestAfschotReewildServer(id = "wbe_afschotAanvraag", 
  data = results$wbe_combinedData)

return(
  list(specie = reactive(input$wbe_species))
)

})


}


#' Shiny UI module for WBE page
#' 
#' @inheritParams reportingGrofwild-common-args
#' @inheritParams wbeServer
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
wbeUI <- function(id, uiText, currentKbo, ecoData) {
  
  ns <- NS(id)
  
  tagList(
    
    tags$div(class = "container",
      
      tags$br(),
      
      if (length(currentKbo) != 1)
        selectizeInput(inputId = ns("wbe_kboChoice"), label = "WBE Naam", 
          choices = currentKbo, width = "100%", multiple = TRUE, options = list(maxItems = 1)),
      
      tags$div(align = "center",
        uiOutput(ns("wbe_title"))
      ),
      
      welcomeSectionUI(id = ns("wbe"), uiText = uiText, category = "wbe", 
        maxDate = max(ecoData$afschot_datum, na.rm = TRUE))
    
    ),
    
    # Select species
    
    tags$div(class = "container",
      
      # Map
      
      mapFlandersUI(id = ns("wbe"), showRegion = FALSE, showCombine = FALSE,
        type = "wbe", plotDetails = "biotoop", showTitle = FALSE, uiText = uiText),
      
      
      # Choose species
      
      uiOutput(ns("wbe_empty")),
      
      h2("Grofwildsoort")),
    
    tags$div(class = "container", 
      
      align = "center", 
      tags$div(class = "noButton",
        radioButtons(inputId = ns("wbe_species"), label = "", inline = TRUE,
          choiceValues = list("Wild zwijn", "Ree", "Damhert", "Edelhert"),
          choiceNames = list(
            HTML("<div class='fotoTitel'>Wild zwijn</div><div id='wildZwijnFoto'></div>"),
            HTML("<div class='fotoTitel'>Ree</div><div id='reeFoto'></div>"),
            HTML("<div class='fotoTitel'>Damhert</div><div id='damhertFoto'></div>"),
            HTML("<div class='fotoTitel'>Edelhert</div><div id='edelhertFoto'></div>"))
        )
      )
    ),
    
    conditionalPanel("output.wbe_emptyAfschot == false",
      
      trendYearRegionUI(id = ns("wbe"), uiText = uiText),
      
      tableSpeciesUI(id = ns("wbe"), uiText = uiText)
    
    ),
    
    tags$div(class = "container",
      
      h2("Extra Figuren en Tabellen"),
      
      conditionalPanel("output.wbe_emptyAfschot == false",
        
        mapSchadeUI(id = ns("wbe_afschot"),
          filterSource = FALSE, filterAccuracy = TRUE,
          variableChoices = c(
            "Seizoen" = "season",
            "Jaar" = "afschotjaar",
            "Jachtmethode" = "jachtmethode_comp"),
          
        ),
        
        conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'",
          countYearShotUI(id = ns("wbe_labeltype"), groupVariable = "leeftijd_comp", uiText = uiText,
            showDataSource = c("leeftijd"), showInterval = TRUE, showType = TRUE, showTime = TRUE)          
        ),
        
        countYearShotUI(id = ns("wbe_jachtmethode"), groupVariable = "jachtmethode_comp", uiText = uiText,
          showInterval = TRUE, showType = TRUE, showTime = TRUE)
      ),
      
      # When no afschot, might still be schadeData
      uiOutput(ns("wbe_mapSchade")),
      
      conditionalPanel("output.wbe_emptyAfschot == false", ns = ns,
        countAgeGenderUI(id = ns("wbe"), uiText = uiText),
        countAgeCheekUI(id = ns("wbe"), showAccuracy = TRUE, showTime = TRUE, uiText = uiText),
        
        conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'", ns = ns,
          countYearAgeUI(id = ns("wbe"), uiText = uiText, showRegion = FALSE)
        ),
        
        conditionalPanel("input.wbe_species == 'Ree'", ns = ns,
          ageGenderLowerJawUI(id = ns("wbe"), regionLevels = NULL, uiText = uiText),    
          percentageRealisedShotUI(id = ns("wbe"), showAccuracy = TRUE, uiText = uiText)
        ),
        
        bioindicatorSection(id = ns("wbe"), uiText = uiText),
        
        conditionalPanel("input.wbe_species == 'Wild zwijn' || input.wbe_species == 'Ree'", ns = ns,
          conditionalPanel("input.wbe_species == 'Ree'", ns = ns,
            plotBioindicatorUI(id = ns("wbe_onderkaak"), bioindicator = "onderkaaklengte", 
              regionLevels = NULL, showAccuracy = TRUE, uiText = uiText, showTime = TRUE,
              showType = TRUE, showDataSource = c("leeftijd", "geslacht", "onderkaak")),
            plotBioindicatorUI(id = ns("wbe_gewicht"), bioindicator = "ontweid_gewicht", 
              regionLevels = NULL, uiText = uiText, showTime = TRUE,
              showType = TRUE, showDataSource = c("leeftijd", "geslacht"))
          )
        ),
        uiOutput("wbe_embryos"),
        
        conditionalPanel("input.wbe_species == 'Ree'", ns = ns,
          requestAfschotReewildUI(id = ns("wbe_afschotAanvraag"), uiText = uiText)
        )
      )
    )
  
  
  )
  
}
