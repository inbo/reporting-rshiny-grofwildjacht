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

metaSchade <- loadMetaSchade()

schadeWildsoorten <- metaSchade$wildsoorten
schadeTypes <- names(metaSchade$codes)
schadeCodes <- unlist(metaSchade$codes)
sourcesSchade <- metaSchade$sources

             
# Specify default year to show (and default max to show in time ranges)
defaultYear <-  as.integer(format(Sys.Date(), "%Y")) - 1

# create temp html file to store grofwild landkaart
outTempFileName <- tempfile(fileext = ".html")

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
biotoopData <- loadHabitats(dataDir = dataDir, spatialData = spatialData)

gc()


# TODO temporary fix
if (!is.null(attr(ecoData, "excluded")))
    geoData <- geoData[!geoData$ID %in% attr(ecoData, "excluded"), ]

# check for wildsoorten to add to schadeWildsoorten
if (any(!unique(schadeData$wildsoort) %in% unlist(schadeWildsoorten))) {
	warning("Nieuwe wildsoorten gedetecteerd in schade data: ", 
          paste0(setdiff(unique(schadeData$wildsoort), unlist(schadeWildsoorten)), collapse = ", "),
          "\nUpdate schadeWildsoorten in loadMetaSchade() functie.")
}

# check for schadeTypes (basiscode) to add to schadeTypes
if (any(!unique(schadeData$schadeBasisCode) %in% schadeTypes)) {
  warning("Nieuwe schade basiscode gedetecteerd in schade data: ", 
      paste0(setdiff(unique(schadeData$schadeBasisCode), schadeTypes), collapse = ", "),
      "\nUpdate schadeTypes in loadMetaSchade() en fullNames() functie.")
}

# check for schadeCodes (schadeCode) to add to schadeCodes
if (any(!unique(schadeData$schadeCode) %in% schadeCodes)) {
  warning("Nieuwe schadeCode gedetecteerd in schade data: ", 
      paste0(setdiff(unique(schadeData$schadeCode), schadeCodes), collapse = ", "),
      "\nUpdate schadeCodes in loadMetaSchade() en fullNames() functie.")
}

# check for schadeSources (indieningType) to add to schadeSources
indieningTypes <- unique(schadeData$indieningType)
isPresent <- grepl(paste(metaSchade$sourcesSchade, collapse = "|"), indieningTypes)
if (!all(isPresent)) {
  warning("Nieuw indieningType gedetecteerd in schade data: ", 
    paste0(indieningTypes[!isPresent], collapse = ", "),
    "\nUpdate loadMetaSchade() functie.")
}
rm(list = c("indieningTypes", "isPresent"))

# UI text for each plot/table
uiText <- read.csv(file = file.path(dataDir, "uiText.csv"))[, c("plotFunction", "title", "wild", "schade")]
uiFunctions <- sapply(strsplit(uiText$plotFunction, split = "-"), function(x) x[1])
if (!all(uiFunctions %in% ls("package:reportingGrofwild")))
  warning("Please update the file 'uiText.csv' as some functions are no longer present in the R package reportingGrofwild.",
    paste(uiFunctions[!uiFunctions %in% ls("package:reportingGrofwild")], collapse = ","))



### Debugging
### -----------

if (!exists("doDebug"))
	doDebug <- FALSE
