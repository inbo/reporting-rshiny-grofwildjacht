library(reportingGrofwild)


library(leaflet)           # for interactive map
library(plotly)            # for interactive graphs
library(shinycssloaders)   # for busy indicator

# Other packages needed, but not loaded
# mapview



### General
### ------------

`%then%` <- function(x, y) {
  
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
  
}
`%<>%` <- magrittr::`%<>%`


# Specify directory with data
dataDir <- system.file("extdata", package = "reportingGrofwild")

# Link to www folder
addResourcePath("www", system.file("ui/www", package = "reportingGrofwild"))

# Specify default year to show (and default max to show in time ranges)
#defaultYear <-  as.integer(format(Sys.Date(), "%Y")) - 1
defaultYear <- 2020

# create temp html file to store grofwild landkaart
outTempFileName <- tempfile(fileext = ".html")



### WBE configuration
### -----------

if (Sys.getenv("SHINYPROXY_USERNAME") == "") {
  
  currentKbo <- "445465768"  ## 441 - fixed AREA
#  currentKbo <- "450506996"   ## 101 - evolving AREA
  
} else {
  # Inside shinyProxy
  
  currentKbo <- Sys.getenv("SHINYPROXY_USERNAME")
  
}



### Load all data
### -------------


# Load object called spatialData
load(file = file.path(dataDir, "spatialData.RData"))

# Data with observations and geographical information
ecoData <- loadRawData(type = "eco")
ecoData <- ecoData[ecoData$doodsoorzaak == "afschot", ]

geoData <- loadRawData(type = "geo")
geoData <- geoData[geoData$KboNummer_Toek %in% currentKbo, ]

# TODO filter on KBO
schadeData <- loadRawData(type = "wildschade")
schadeData <- schadeData[schadeData$KboNummer %in% currentKbo, ]

wbeData <- loadWbeHabitats(dataDir = dataDir)

currentWbe <- unique(geoData$PartijNummer)
currentWbe <- currentWbe[!is.na(currentWbe)]



### Debugging
### -----------

onStop(function() {
			if (file.exists(".RDuetConsole"))
				file.remove(".RDuetConsole")
		})

if (!exists("doDebug"))
	doDebug <- FALSE
