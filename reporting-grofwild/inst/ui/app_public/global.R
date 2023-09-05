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

# define js function for opening urls in new tab/window
js_code <- "
  shinyjs.browseURL = function(url) {
  window.open(url, '_parent');
  }
  "


# Specify directory with data
dataDir <- system.file("extdata", package = "reportingGrofwild")

# Link to www folder
addResourcePath("www", system.file("ui/www", package = "reportingGrofwild"))


### Debugging
### -----------

if (!exists("doDebug"))
  doDebug <- FALSE



### Meta data
### ------------

if (!doDebug | !exists("metaSchade"))
  metaSchade <- loadMetaSchade()

schadeWildsoorten <- metaSchade$wildsoorten
schadeTypes <- metaSchade$types
schadeCodes <- metaSchade$codes
names(schadeCodes) <- NULL
schadeCodes <- unlist(schadeCodes)
sourcesSchade <- metaSchade$sources
fullNames <- c(schadeTypes, schadeCodes, schadeWildsoorten)

tmpWildsoorten <- schadeWildsoorten
names(tmpWildsoorten) <- NULL
tmpWildsoorten <- unlist(tmpWildsoorten)
names(tmpWildsoorten) <- tmpWildsoorten

tmpWildsoorten <- schadeWildsoorten
names(tmpWildsoorten) <- NULL
tmpWildsoorten <- unlist(tmpWildsoorten)
names(tmpWildsoorten) <- tmpWildsoorten
fullNames <- c(schadeTypes, schadeCodes, schadeWildsoorten)



# Specify default year to show (and default max to show in time ranges)
defaultYear <-  as.integer(format(Sys.Date(), "%Y")) - 1

# create temp html file to store grofwild landkaart
outTempFileName <- tempfile(fileext = ".html")




### Load all data
### -------------

if (!doDebug | !exists("openingstijdenData"))
  openingstijdenData <- loadOpeningstijdenData()
if (!doDebug | !exists("toekenningsData"))
  toekenningsData <- loadToekenningen()

# Load object called spatialData
if (!doDebug | !exists("spatialData"))
  readS3(file = "spatialData.RData")

# Data with observations and geographical information
if (!doDebug | !exists("ecoData"))
  ecoData <- loadRawData(type = "eco")
if (!doDebug | !exists("geoData"))
  geoData <- loadRawData(type = "geo")
if (!doDebug | !exists("schadeData"))
  schadeData <- loadRawData(type = "wildschade")
if (!doDebug | !exists("biotoopData"))
  biotoopData <- loadHabitats(spatialData = spatialData)


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
      "\nUpdate schadeTypes in loadMetaSchade() functie.")
}

# check for schadeCodes (schadeCode) to add to schadeCodes
if (any(!unique(schadeData$schadeCode) %in% schadeCodes)) {
  warning("Nieuwe schadeCode gedetecteerd in schade data: ", 
      paste0(setdiff(unique(schadeData$schadeCode), schadeCodes), collapse = ", "),
      "\nUpdate schadeCodes in loadMetaSchade() functie.")
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
uiText <- read.csv(file = file.path(dataDir, "uiText.csv"), sep = ";")
if (config::get("datacheck", file = system.file("config.yml", package = "reportingGrofwild"))) {
  uiFunctions <- sapply(strsplit(uiText$plotFunction, split = "-"), function(x) x[1])
  uiFunctions <- uiFunctions[!is.na(uiFunctions)] 
  uiCheck <- uiFunctions[!startsWith(uiFunctions, "F")]
  if (!all(uiCheck %in% ls("package:reportingGrofwild")))
    warning("Please update the file 'uiText.csv' as some functions are no longer present in the R package reportingGrofwild.",
      paste(uiCheck[!uiCheck %in% ls("package:reportingGrofwild")], collapse = ","))
  rm(uiFunctions, uiCheck)
}

# Availability (Dashboard page)
availableData <- read.csv(file.path(dataDir, "Data_beschikbaarheid.csv"))
names(availableData)[3:6] <- c("flanders", "provinces", "communes", "faunabeheerzones")

uiText <- merge(uiText, availableData, by.x = "plotFunction", by.y = "Code",
  all.x = TRUE)



# Choices for Dashboard
## Ideally these are read from availableData
populatieChoices <- c("F16_1", "F17_1", "F17_4", "F18_1")
jachtChoices <- c("F04_3", "F05_1", "F05_2")
schadeChoices <- c("F07_1", "F09_2", "F07_3")
maatschappijChoices <- c("F14_1", "F14_2", "F14_3", "F14_4", "F14_5")


