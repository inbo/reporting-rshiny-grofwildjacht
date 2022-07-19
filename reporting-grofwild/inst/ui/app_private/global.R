library(reportingGrofwild)


library(leaflet)           # for interactive map
library(plotly)            # for interactive graphs
library(shinycssloaders)   # for busy indicator

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


### Debugging
### -----------

if (!exists("doDebug"))
  doDebug <- FALSE



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
if (!doDebug | !exists("spatialData")) {
  load(file = file.path(dataDir, "spatialDataWBE.RData"))
  spatialData <- spatialDataWBE
  rm(spatialDataWBE)
}
gc()

# Load grid data
if (!doDebug | !exists("gridData")) {
  gridData <- readCubeData()[[1]]
}
gc()

# Data with observations and geographical information
if (!doDebug | !exists("ecoData")) {
  ecoData <- loadRawData(type = "eco")
  ecoData <- ecoData[ecoData$doodsoorzaak == "afschot", ]
}

if (!doDebug | !exists("geoData")) {
  geoData <- loadRawData(type = "geo")
  geoData <- geoData[geoData$KboNummer_Toek %in% currentKbo, ]
}

if (!doDebug | !exists("schadeData")) {
  schadeData <- loadRawData(type = "wildschade")
  schadeData <- schadeData[schadeData$KboNummer %in% currentKbo, ]
}

currentWbe <- unique(geoData$PartijNummer)
currentWbe <- currentWbe[!is.na(currentWbe)]

if (!doDebug | !exists("biotoopData")) {
  biotoopData <- loadHabitats(dataDir = dataDir, spatialData = spatialData,
    regionLevels = "wbe")[["wbe"]]
  biotoopData <- biotoopData[biotoopData$regio %in% currentWbe, ]
}

if (!doDebug | !exists("toekenningsData")) {
  toekenningsData <- loadToekenningen(dataDir = dataDir)
  toekenningsData <- toekenningsData[toekenningsData$KboNummer_Toek %in% currentKbo, ]
}
gc()

if (!doDebug | !exists("uiText")) {
  uiText <- read.csv(file = file.path(dataDir, "uiText.csv"))[, c("plotFunction", "title", "wbe")]
  uiFunctions <- sapply(strsplit(uiText$plotFunction, split = "-"), function(x) x[1])
  if (!all(uiFunctions %in% ls("package:reportingGrofwild")))
    warning("Please update the file 'uiText.csv' as some functions are no longer present in the R package reportingGrofwild.",
      paste(uiFunctions[!uiFunctions %in% ls("package:reportingGrofwild")], collapse = ","))
  rm(uiFunctions)
}



