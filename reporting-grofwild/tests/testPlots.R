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

reeEcoData <- ecoData[ecoData$wildsoort == "Ree", ]

openingstijdenData <- loadOpeningstijdenData()


## TABLES: Summary tables for Wild zwijn and Ree

# For age
allTables <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      tableProvince(data = plotData, categorie = "leeftijd")
      
    })
allTables


# For count with type
tableProvince(data = ecoData[ecoData$wildsoort == "Ree", ], categorie = "typeAantal")
tableProvince(data = ecoData[ecoData$wildsoort == "Ree", ], categorie = "typeAantal",
    jaar = 2016)

# For percent shot of assigned with type
toekenningsData <- loadToekenningen()
tableProvince(data =  ecoData[ecoData$wildsoort == "Ree", ],
    assignedData = toekenningsData, categorie = "typePercent")



## PLOT 1: Counts per year and province ##

allPlots <- lapply(unique(ecoData$wildsoort), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      res <- countYearProvince(data = plotData, wildNaam = wildsoort)
      
    })
allPlots

# Some special cases
countYearProvince(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016, doodsoorzaak = "afschot")
countYearProvince(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017, doodsoorzaak = "afschot")



## PLOT 2: Map with counts and corresponding line plot ##

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

#leaflet(spatialData$provinces) %>% 
#    addProviderTiles("Hydda.Full") %>%
#    addPolygons(
#        weight = 1, 
#        color = "gray",
#        fillColor = "red",
#        fillOpacity = 0.8,
#        group = "region"
#    ) 


## PLOT 3: Counts age based on cheek ##

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      countAgeCheek(data = plotData, wildNaam = wildsoort)
      
    })
tmp <- sapply(allPlots, function(x) print(x$plot))

# Some special cases
countAgeCheek(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016)
countAgeCheek(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017)



## PLOT 4: Counts per year and age ##

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      lapply(c("count", "percent"), function(summarizeBy)
            countYearAge(data = plotData, wildNaam = wildsoort, 
                summarizeBy = summarizeBy))
      
    })
allPlots

# Some special cases
countYearAge(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016)
countYearAge(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017)


## PLOT 5: yearly percentage of shot animals

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
			
		message("Plot for specie ", wildsoort, " in progress")
			
		plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
		
		openingSeasonData <- openingstijdenData[
			openingstijdenData$Soort == wildsoort, ]
	
		openingstijdRange <- c(
				max(
						min(plotData$afschotjaar), 
						min(openingSeasonData$Jaar)
				),
				min(
						max(plotData$afschotjaar), 
						max(openingSeasonData$Jaar)
				)-1
		)
		
		openingstijd <- seq(openingstijdRange[1], openingstijdRange[2])
		
		types <- unique(openingSeasonData$Type)
		types[types == ""] <- "all"
		
		lapply(types, function(type){
					
				message("Plot for type ", type, " in progress")
					
										
			lapply(openingstijd, function(jaar){
						
						message("Plot for year ", jaar, " in progress")
		
				percentageYearlyShotAnimals(
					data = plotData, 
					openingstijdenData = openingSeasonData,
					wildNaam = wildsoort, 
					type = type,
					jaar = jaar,
					jaartallen = openingstijd
				)
		
			}
	
			)
			
		})
	
})


## PLOT 6: Percentages per age and gender

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      countAgeGender(data = plotData, wildNaam = wildsoort)
      
    })
allPlots

countAgeGender(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016)
countAgeGender(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017)



## PLOT 7: Distribution of weight ifo age ##

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
      
      plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
      boxAgeWeight(data = plotData, wildNaam = wildsoort)
      
    })
allPlots

# Some special cases
boxAgeWeight(data = wildEcoData, wildNaam = "wild zwijn", 
    jaartallen = 2016)
boxAgeWeight(data = wildEcoData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017)

## PLOT 9: Distribution of cheek length vs class ##

res <- boxAgeGenderLowerJaw(data = reeEcoData, wildNaam = "Ree", 
		jaartallen = unique(reeEcoData$afschotjaar))

## PLOT 10: bioindicators

indicators <- c("onderkaaklengte", "ontweid_gewicht", "aantal_embryos")

tmp <- lapply(indicators, function(bioindicator){
	message("Plot for ", bioindicator, " in progress")
	pl <- plotBioindicator(
		data = reeEcoData, 
#		type = "Geitkits",	
		wildNaam = "Ree", 
		jaartallen = unique(reeEcoData$afschotjaar),
		bioindicator = bioindicator
	)
#	print(pl)
})

# 2017 for aantal embryos
	

