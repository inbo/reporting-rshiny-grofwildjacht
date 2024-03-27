# Test shiny plot/table modules
# 
# Author: mvarewyck
###############################################################################


context("Test Shiny Modules")

# Load all data
readS3(file = "spatialData_sf.RData")



geoData <- loadRawData(type = "geo")
biotoopData <- loadHabitats()


test_that("Module mapFlanders", {
    shiny::testServer(mapFlandersServer, 
      args = list(
        
        defaultYear = 2022,
        species = reactive("Wild zwijn"),
        type = "grofwild",
        regionLevel = reactive("communes"),
        locaties = reactive(NULL),
        geoData = reactive(geoData[geoData$wildsoort == "Wild zwijn", ]),
        biotoopData = biotoopData,
        allSpatialData = spatialData,
        hideGlobeDefault = FALSE
      
      ), {
        session$setInputs(linkYearNativerange = 1)
        expect_true(TRUE)
        
      })
    
  })


test_that("Profile Shiny Module", {
    
    skip("Only used for debugging")
    
    profvis(
      runApp(shinyApp(
          fluidPage(
            mapFlandersUI(id = "test",
              plotDetails = c()
#          plotDetails = c("flanders", "region")
            )
          ),
          function(input, output, session) {
            mapFlandersServer(id = "test", 
              defaultYear = 2022,
              species = reactive("Wild zwijn"),
              type = "grofwild",
              geoData = reactive(geoData[geoData$wildsoort == "Wild zwijn", ]),
              biotoopData = biotoopData,
              allSpatialData = spatialData,
              hideGlobeDefault = FALSE)
          }
        ))
    )
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