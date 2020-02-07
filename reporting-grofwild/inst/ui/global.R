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

# Specify currently used wildsoorten
schadeSoorten <- list("Grof wild" = c("wild zwijn", "edelhert", "ree"),
                      "Klein wild" = c("haas", "fazant", "konijn"),
                      "Waterwild" = c("wilde eend", "smient", "grauwe gans"),
                      "Overig" = c("houtduif", "vos", "wolf"))



### Load all data
### -------------

openingstijdenData <- loadOpeningstijdenData(dataDir = dataDir)
toekenningsData <- loadToekenningen(dataDir = dataDir)

# Load object called spatialData
load(file = file.path(dataDir, "spatialData.RData"))

# Data with observations and geographical information
ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")
schadeData <- loadRawData(type = "wildschade")

# TODO temporary fix
if (!is.null(attr(ecoData, "excluded")))
    geoData <- geoData[!geoData$ID %in% attr(ecoData, "excluded"), ]

# check for wildsoorten to add to schadeSoorten
if (any(!unique(schadeData$wildsoort) %in% unlist(schadeSoorten))) {
	warning("Nieuwe wildsoorten gedetecteerd in raw data: ", 
          paste0(setdiff(unique(schadeData$wildsoort), unlist(schadeSoorten)), collapse = ", "),
          "\nUpdate schadeSoorten aub")
}





### Debugging
### -----------

onStop(function() {
			if (file.exists(".RDuetConsole"))
				file.remove(".RDuetConsole")
		})

if (!exists("doDebug"))
	doDebug <- FALSE