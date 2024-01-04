library(reportingGrofwild)



### General
### ------------

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


# Specify default year to show (and default max to show in time ranges)
defaultYear <-  as.integer(format(Sys.Date(), "%Y")) - 1




### Load all data
### -------------

if (!doDebug | !exists("openingstijdenData"))
  openingstijdenData <- loadOpeningstijdenData()
if (!doDebug | !exists("toekenningsData"))
  toekenningsData <- loadToekenningen()

# Load object called spatialData
if (!doDebug | !exists("spatialData"))
  readS3(file = "spatialData_sf.RData")

# Data with observations and geographical information
if (!doDebug | !exists("ecoData"))
  ecoData <- loadRawData(type = "eco")
if (!doDebug | !exists("geoData"))
  geoData <- loadRawData(type = "geo")
if (!doDebug | !exists("schadeData"))
  schadeData <- loadRawData(type = "wildschade")
if (!doDebug | !exists("biotoopData"))
  biotoopData <- loadHabitats()


# TODO temporary fix
if (!is.null(attr(ecoData, "excluded")))
    geoData <- geoData[!geoData$ID %in% attr(ecoData, "excluded"), ]

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
populatieChoices <- c("F16_1", "F17_1", "F17_4", "F18_1", "F18_8")
jachtChoices <- c("F04_3", "F05_1", "F05_2")
schadeChoices <- c("F07_1", "F09_2", "F07_3")
maatschappijChoices <- c("F14_1", "F14_2", "F14_3", "F14_4", "F14_5")


