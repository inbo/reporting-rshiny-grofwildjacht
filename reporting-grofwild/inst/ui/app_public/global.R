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

# Specify currently used wildsoorten
schadeWildsoorten <- list("Grof wild" = c("wild zwijn", "edelhert", "ree", "damhert"),
                      "Klein wild" = c("haas", "fazant", "konijn", "patrijs"),
                      "Waterwild" = c("wilde eend", "smient", "grauwe gans", "Canadese gans", "kievit"),
                      "Overig" = c("houtduif", "vos", "verwilderde kat"))

# Specify currently used type schades
schadeTypes <- c("GEWAS", "VRTG", "ANDERE")

# Specify currently used types schade subcodes
schadeCodes <- c(c("GNPERSLTSL", "PERSLTSL", "ONBEKEND"),           #VRTG
                 c("WLSCHD", "VRTSCHD", "GEWASANDR", "VGSCHD", "GRFSCHD"),     #GEWAS
                 c("ANDERE", "VALWILD"))                            #ANDERE


             
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

# TODO temporary fix
if (!is.null(attr(ecoData, "excluded")))
    geoData <- geoData[!geoData$ID %in% attr(ecoData, "excluded"), ]

# check for wildsoorten to add to schadeWildsoorten
if (any(!unique(schadeData$wildsoort) %in% unlist(schadeWildsoorten))) {
	warning("Nieuwe wildsoorten gedetecteerd in raw data: ", 
          paste0(setdiff(unique(schadeData$wildsoort), unlist(schadeWildsoorten)), collapse = ", "),
          "\nUpdate schadeWildsoorten aub")
}

# check for schadeTypes (basiscode) to add to schadeTypes
if (any(!unique(schadeData$schadeBasisCode) %in% schadeTypes)) {
  warning("Nieuwe schade basiscode gedetecteerd in raw data: ", 
      paste0(setdiff(unique(schadeData$schadeBasisCode), schadeTypes), collapse = ", "),
      "\nUpdate schadeTypes aub en ook de fullnames() functie.")
}

# check for schadeCodes (schadeCode) to add to schadeCodes
if (any(!unique(schadeData$schadeCode) %in% schadeCodes)) {
  warning("Nieuwe schadeCode gedetecteerd in raw data: ", 
      paste0(setdiff(unique(schadeData$schadeCode), schadeCodes), collapse = ", "),
      "\nUpdate schadeCodes aub en ook de fullnames() functie.")
}


### Debugging
### -----------

if (!exists("doDebug"))
	doDebug <- FALSE
