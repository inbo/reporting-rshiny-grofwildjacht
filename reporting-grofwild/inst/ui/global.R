library(reportingGrofwild)


library(leaflet)           # for interactive map
library(plotly)            # for interactive graphs
library(shinycssloaders)   # for busy indicator

# Other packages needed, but not loaded
# mapview



### General
### ------------

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

# TODO temporary fix
if (!is.null(attr(ecoData, "excluded")))
    geoData <- geoData[!geoData$ID %in% attr(ecoData, "excluded"), ]







### Debugging
### -----------

onStop(function() {
			if (file.exists(".RDuetConsole"))
				file.remove(".RDuetConsole")
		})

if (!exists("doDebug"))
	doDebug <- FALSE