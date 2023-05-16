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
  
  # Testing empty WBE
  currentKbo <- '["463156093","463385430","463455805","463711963","463729383","463758087","463955948","464274959","464623268","464931490","465825969","469134659","469697853","473485407","474369986","507757188","536538969","542964626","642955590","726995994","809734224","810429258","820241106","860852333","863669291","865106475","865893363","873552603","899311150","462285568","461628443","461283302","461281025","461206987","461205603","461138196","460969932","460735053","460600342","460341214","460236888","460118708","460075948","460028735","459598668","459255309","459126833","459112183","458866418","458856421","458642031","458500984","458453573","457994507","457896616","457883253","457799319","457721917","457687867","457613237","457496639","457333719","457323326","457310359","457285318","457159218","457100424","456791410","456763694","456736574","456736475","456595925","456415880","456346002","456345705","456339171","456337290","456262759","456259195","456226137","456087466","455928407","455765683","455554362","455481910","455470725","455452216","455424601","455415493","455232282","455217832","455000868","454996613","454700267","454685817","454550017","454542592","454537347","454521016","454488253","454472813","454284355","454112131","454096293","454090157","454066304","453997018","453903778","453902986","453896157","453876955","453820339","453754122","453686519","453684341","453684143","453391856","453377208","453353947","453313563","453121840","453096797","453059482","452989406","452987030","452952782","452934669","452893097","452846379","452810351","452808074","452614074","452562012","452434229","452400377","452247355","452108387","451933589","451891227","451864503","451858266","451749091","451356737","451318927","451273593","451245087","451162242","451133439","451131855","451091471","451011101","450961215","450797404","450516104","450506996","450282710","450141762","450048722","449899559","449662306","449226301","449178195","449013790","448881158","448656969","448439215","448077840","446912355","446637290","446348369","445486455","445465768","444094605","439537187","436787634","417187694","416410508","412958494"]'
#  currentKbo <- '["863669291"]'  # empty afschot, with schade
#  currentKbo <- '["463729383"]'  # empty schade, with afschot
#  currentKbo <-  '["463758087]'  # empty afschot, empty schade

}


# Manipulate kbo: admin, multiple kbo
if (currentKbo == "admin")
  currentKbo <- unique(c(geoData$KboNummer_Toek, schadeData$KboNummer, matchingWbeData$KboNummer_Partij)) else
  currentKbo <- as.integer(gsub('\\"', "", strsplit(gsub("\\[|\\]", "", currentKbo), split = ",")[[1]]))


### Load all data
### -------------

matchingWbeData <- loadRawData(type = "kbo_wbe")


# Load object called spatialData
if (!doDebug | !exists("spatialData"))
  spatialData <- loadShapeData(WBE_NR = matchingWbeData$PartijNummer[match(currentKbo, matchingWbeData$KboNummer_Partij)])

# Data with observations and geographical information
if (!doDebug | !exists("ecoData"))
  ecoData <- loadRawData(type = "eco")

if (!doDebug | !exists("geoData"))
  geoData <- loadRawData(type = "geo")
if (!doDebug | !exists("schadeData")) {
  schadeData <- loadRawData(type = "wildschade")
  schadeData <- schadeData[schadeData$wildsoort %in% c("Wild zwijn", "Ree", "Damhert", "Edelhert"), ]
}
if (!doDebug | !exists("biotoopData"))
  biotoopData <- loadHabitats(spatialData = spatialData, regionLevels = "wbe")[["wbe"]]
if (!doDebug | !exists("toekenningsData"))
  toekenningsData <- loadToekenningen()

# Naming currentKbo
matchingKbo <- match(currentKbo, geoData$KboNummer_Toek)
names(currentKbo) <- ifelse(is.na(matchingKbo), 
  matchingWbeData$WBE.officieel[match(currentKbo, matchingWbeData$KboNummer_Partij)], 
  geoData$WBE_Naam_Toek[matchingKbo])
currentKbo <- currentKbo[order(names(currentKbo))]

toekenningsData <- toekenningsData[toekenningsData$KboNummer_Toek %in% currentKbo, ]

gc()


uiText <- read.csv(file = file.path(dataDir, "uiText.csv"), sep = ";")[, c("plotFunction", "title", "wbe")]
if (config::get("datacheck", file = system.file("config.yml", package = "reportingGrofwild"))) {
  uiFunctions <- sapply(strsplit(uiText$plotFunction, split = "-"), function(x) x[1])
  uiFunctions <- uiFunctions[!is.na(uiFunctions)] 
  uiCheck <- uiFunctions[!startsWith(uiFunctions, "F")]
  if (!all(uiCheck %in% ls("package:reportingGrofwild")))
    warning("Please update the file 'uiText.csv' as some functions are no longer present in the R package reportingGrofwild.",
      paste(uiCheck[!uiCheck %in% ls("package:reportingGrofwild")], collapse = ","))
  rm(uiFunctions, uiCheck)
}


