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

if (grepl("WBE_ADMIN", Sys.getenv("SHINYPROXY_USERGROUPS"))) {
  
  currentKbo <- "admin"
  
} else if (Sys.getenv("SHINYPROXY_KBO_NUMMERS") != "") {
  # Inside shinyProxy
  
  currentKbo <- Sys.getenv("SHINYPROXY_KBO_NUMMERS")
  
} else {
  # Local testing
  
  currentKbo <- '["445465768"]'  ## 441 - fixed AREA
#  currentKbo <- '["450506996"]'   ## 101 - evolving AREA
#  currentKbo <- '["454472813"]'  # including Damhert data
#  currentKbo <- '["417187694"]'   # including Edelhert data
#  currentKbo <- '["454472813"]'   # no data Wild zwijn
#  currentKbo <- "admin"
  currentKbo <- '["445465768","450506996","454472813"]'
  
}




### Load all data
### -------------

# Load object called spatialData
readS3(file = "spatialDataWBE.RData")
spatialData <- spatialDataWBE
rm(spatialDataWBE)
gc()

# Data with observations and geographical information
ecoData <- loadRawData(type = "eco")
ecoData <- ecoData[ecoData$doodsoorzaak == "afschot", ]

geoData <- loadRawData(type = "geo")
schadeData <- loadRawData(type = "wildschade")
biotoopData <- loadHabitats(spatialData = spatialData, regionLevels = "wbe")[["wbe"]]
toekenningsData <- loadToekenningen()


# Manipulate kbo: admin, multiple kbo
if (currentKbo == "admin")
  currentKbo <- unique(geoData$KboNummer_Toek) else
  currentKbo <- as.integer(gsub('\\"', "", strsplit(gsub("\\[|\\]", "", currentKbo), split = ",")[[1]]))

names(currentKbo) <- geoData$WBE_Naam_Toek[match(currentKbo, geoData$KboNummer_Toek)]
currentKbo <- currentKbo[order(names(currentKbo))]

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

if (doDebug)
  checkS3()
