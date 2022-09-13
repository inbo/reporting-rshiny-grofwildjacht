library(reportingGrofwild)


library(leaflet)           # for interactive map
library(plotly)            # for interactive graphs
library(shinycssloaders)   # for busy indicator
library(shinyjs)

# Other packages needed, but not loaded
# mapview



### General
### ------------

`%<>%` <- magrittr::`%<>%`


# Specify directory with data
dataDir <- system.file("extdata", package = "reportingGrofwild")

# Link to www folder
addResourcePath("www", system.file("ui/www", package = "reportingGrofwild"))

# Specify default year to show (and default max to show in time ranges)
#defaultYear <-  as.integer(format(Sys.Date(), "%Y")) - 1
defaultYear <- as.numeric(format(Sys.Date(), "%Y")) - 1

# create temp html file to store grofwild landkaart
outTempFileName <- tempfile(fileext = ".html")



### WBE configuration
### -----------

if (Sys.getenv("SHINYPROXY_USERNAME") == "") {
  
  currentKbo <- "445465768"  ## 441 - fixed AREA
#  currentKbo <- "450506996"   ## 101 - evolving AREA
#  currentKbo <- "446912355"
  
} else {
  # Inside shinyProxy
  
  currentKbo <- Sys.getenv("SHINYPROXY_USERNAME")
  
}



### Load all data
### -------------

# Load object called spatialData
load(file = file.path(dataDir, "spatialDataWBE.RData"))
spatialData <- spatialDataWBE
rm(spatialDataWBE)
gc()

# Data with observations and geographical information
ecoData <- loadRawData(type = "eco")
ecoData <- ecoData[ecoData$doodsoorzaak == "afschot", ]

geoData <- loadRawData(type = "geo")
geoData <- geoData[geoData$KboNummer_Toek %in% currentKbo, ]

schadeData <- loadRawData(type = "wildschade")
schadeData <- schadeData[schadeData$KboNummer %in% currentKbo, ]

currentWbe <- unique(geoData$PartijNummer)
currentWbe <- currentWbe[!is.na(currentWbe)]

biotoopData <- loadHabitats(dataDir = dataDir, spatialData = spatialData,
  regionLevels = "wbe")[["wbe"]]
biotoopData <- biotoopData[biotoopData$regio %in% currentWbe, ]

toekenningsData <- loadToekenningen(dataDir = dataDir)
toekenningsData <- toekenningsData[toekenningsData$KboNummer_Toek %in% currentKbo, ]

spatialData <- filterSpatialWbe(allSpatialData = spatialData, partijNummer = currentWbe)

gc()

uiText <- read.csv(file = file.path(dataDir, "uiText.csv"))[, c("plotFunction", "title", "wbe")]
uiFunctions <- sapply(strsplit(uiText$plotFunction, split = "-"), function(x) x[1])
if (!all(uiFunctions %in% ls("package:reportingGrofwild")))
  warning("Please update the file 'uiText.csv' as some functions are no longer present in the R package reportingGrofwild.",
    paste(uiFunctions[!uiFunctions %in% ls("package:reportingGrofwild")], collapse = ","))



### Debugging
### -----------

onStop(function() {
			if (file.exists(".RDuetConsole"))
				file.remove(".RDuetConsole")
		})

if (!exists("doDebug"))
	doDebug <- FALSE
