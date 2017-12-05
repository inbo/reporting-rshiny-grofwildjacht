library(reportingGrofwild)


library(leaflet)
library(plotly)


`%then%` <- shiny:::`%OR%`


# Specify directory with data
dataDir <- system.file("extdata", package = "reportingGrofwild")


### Load all data
### -------------

openingstijdenData <- loadOpeningstijdenData(dataDir = dataDir)
toekenningsData <- loadToekenningen(dataDir = dataDir)

# Load object called spatialData
load(file = file.path(dataDir, "spatialData.RData"))

# Data with observations and geographical information
ecoData <- loadRawData(dataDir = dataDir, type = "eco")
geoData <- loadRawData(dataDir = dataDir, type = "geo", 
    shapeData = spatialData)


## Reactive values
results <- reactiveValues(
    showSpecies = "Wild zwijn" # For which species should summaries be shown?
)


