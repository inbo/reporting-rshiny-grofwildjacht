# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################

library(reportingGrofwild)

# Load all data
spatialData <- loadShapeData()

ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo", shapeData = spatialData)

wildEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]


## PLOT 1: Counts per year and province ##

allPlots <- lapply(levels(ecoData$wildsoort), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      countYearProvince(data = plotData, wildNaam = wildsoort)
      
    })
allPlots

# Some special cases
countYearProvince(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016, doodsoorzaak = "afschot")
countYearProvince(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017, doodsoorzaak = "afschot")



## PLOT 2: Table per age and province ##

# TODO we wait for extra data on "ree"


## PLOT 3: Map with counts and corresponding line plot ##

wildGeoData <- geoData[geoData$wildsoort == "Wild zwijn", ]

# Check province names
provinceNames <- levels(spatialData$provinces@data$NAAM)
levels(wildGeoData$provincie)[which(!levels(wildGeoData$provincie) %in% provinceNames)]

provinceNames <- levels(spatialData$provincesVoeren@data$NAAM)
levels(wildGeoData$provincie)[which(!levels(wildGeoData$provincie) %in% provinceNames)]


# Check commune names
communeNames <- levels(spatialData$communes@data$NAAM)
levels(wildGeoData$gemeente_afschot_locatie)[
    which(!levels(wildGeoData$gemeente_afschot_locatie) %in% communeNames)]

# Check differences with reported map by INBO
geoData[which(as.character(geoData$gemeente_afschot_locatie) == "Dessel" & geoData$wildsoort == "Wild zwijn"), ]


plot(spatialData$flanders)
plot(spatialData$provinces)
plot(spatialData$provincesVoeren)
plot(spatialData$communes)




## PLOT 4: Counts age based on cheek ##

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      countAgeCheek(data = plotData, wildNaam = wildsoort)
      
    })
allPlots

# Some special cases
countAgeCheek(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016)
countAgeCheek(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017)



## PLOT 5: Counts per year and age ##

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      countYearAge(data = plotData, wildNaam = wildsoort)
      
    })
allPlots

# Some special cases
countYearAge(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016)
countYearAge(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017)

## PLOT: yearly percentage of shot animals

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
			
		plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
		
		lapply(sort(unique(plotData$afschotjaar)), function(jaar)
	
			percentageYearlyShotAnimals(
				data = plotData, 
				wildNaam = wildsoort, 
				jaar = jaar
			)

		)
	
})
