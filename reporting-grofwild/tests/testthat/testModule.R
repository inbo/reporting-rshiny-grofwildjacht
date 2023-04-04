# Test shiny plot/table modules
# 
# Author: mvarewyck
###############################################################################


context("Test Shiny Modules")

# Load all data
readS3(file = "spatialData_sf.RData")

ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")
biotoopData <- loadHabitats(spatialData = spatialData)


test_that("Module mapFlanders", {
    shiny::testServer(mapFlandersServer, 
      args = list(
        
        defaultYear = 2021,
        species = reactive("Wild zwijn"),
        type = "empty",
        regionLevel = reactive("provinces"),
        locaties = reactive(c("Antwerpen", "Limburg")),
        geoData = reactive(geoData[geoData$wildsoort == "Wild zwijn", ]),
        biotoopData = biotoopData,
        allSpatialData = spatialData,
        hideGlobeDefault = FALSE
      
      ), {
        session$setInputs(linkYearNativerange = 1)
        expect_true(TRUE)
        
      })
  })


test_that("Debug specific shiny module", {
    
    skip("Only used for debugging")
    
    ui <- tagList(
      
      mapFlandersUI(id = "empty", showRegion = FALSE, showCombine = FALSE,
        type = "dash",
        showSource = FALSE,
        plotDetails = c("biotoop", "biotoopTable"), 
        showTitle = FALSE
      )
    
    )
    
    server <- function(input, output, session) {
      
      mapFlandersServer(id = "dash",
        defaultYear = defaultYear,
        species = results$dash_species,
        type = "empty",
        regionLevel = reactive(input$dash_regionLevel),
        locaties = reactive(input$dash_locaties),
        geoData = reactive(everGeoData),
        biotoopData = biotoopData,
        allSpatialData = spatialData,
        hideGlobeDefault = FALSE)
      
    }
    
    shinyApp(ui, server)
    
  })