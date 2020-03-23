# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################

library(reportingGrofwild)
library(testthat)



# Load all data
#readShapeData()  # create shape data
dataDir <- system.file("extdata", package = "reportingGrofwild")
load(file = file.path(dataDir, "spatialData.RData"))

ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")
geoData <- geoData[!geoData$ID %in% attr(ecoData, "excluded"), ]

expect_equal(nrow(ecoData), nrow(geoData))


species <- c("Ree", "Wild zwijn", "Damhert", "Edelhert")

getCategories <- function(wildSoort) {
    
    switch(wildSoort,
            "Wild zwijn" = c("Frisling", "Overloper", "Volwassen"),
            "Ree" = c("Kits", "Jongvolwassen", "Volwassen")
    )
}

wildEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
reeEcoData <- ecoData[ecoData$wildsoort == "Ree", ]

openingstijdenData <- loadOpeningstijdenData()


## TABLES: Summary tables for Wild zwijn and Ree

# For age
allTables <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
            
            plotData <- subset(ecoData, wildsoort == iSoort & doodsoorzaak == "afschot")
            
            expectedNames <- c("Provincie", getCategories(iSoort), "Onbekend")
            timeRange <- range(plotData$afschotjaar)
            
            wildTables <- lapply(timeRange[1]:timeRange[2], function(jaar) {
                        
                        myTable <- tableProvince(data = plotData, jaar = jaar, categorie = "leeftijd")
                        
                        expect_true(all(expectedNames %in% colnames(myTable)))
                        
                        myTable
                        
                    })
            
            expect_equal(length(wildTables), diff(timeRange) + 1)
            
        })


## -- outdated: not shown anymore -- ##
if (FALSE) {
    
    # For count with type 
    tableProvince(data = reeEcoData[reeEcoData$doodsoorzaak == "afschot", ], 
            categorie = "typeAantal", jaar = 2017)
    
    # For percent shot of assigned with type
    toekenningsData <- loadToekenningen()
    tableProvince(data =  ecoData[ecoData$wildsoort == "Ree", ],
            assignedData = toekenningsData, categorie = "typePercent", jaar = 2017)
    
    # This will produce informative error
    expect_error(tableProvince(data =  ecoData[ecoData$wildsoort == "Ree", ],
                    assignedData = toekenningsData, categorie = "typePercent", jaar = 2019))
    
}


## PLOT 1: Counts per year and province ##

allPlots <- lapply(species, function(iSoort) {
            
            plotData <- subset(ecoData, wildsoort == iSoort)
            if (iSoort == "Edelhert")
                timeRange <- 2008:max(plotData$afschotjaar) else
                timeRange <- min(plotData$afschotjaar):max(plotData$afschotjaar)
            
            res <- countYearProvince(data = plotData, jaartallen = timeRange)
            
            expect_equal(names(res), c("plot", "data"))
            expect_equal(names(res$data), c("afschotjaar", "locatie", "aantal"))
            
            res
            
        })


# Some special cases
countYearProvince(data = wildEcoData, jaartallen = 2016)
countYearProvince(data = wildEcoData, jaartallen = 2016:2017)



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

allPlots <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
            
            categories <- getCategories(iSoort)
            plotData <- ecoData[ecoData$wildsoort == iSoort, ]
            
            if (iSoort == "Ree")
                timeRange <- 2004:max(plotData$afschotjaar) else
                timeRange <- min(plotData$afschotjaar):max(plotData$afschotjaar)
            
            res <- countAgeCheek(data = plotData, jaartallen = timeRange)
            
            expect_equal(levels(res$data$jager), categories)
            expect_equal(levels(res$data$kaak), categories)
            
            res
            
        })


# Some special cases
countAgeCheek(data = wildEcoData, jaartallen = 2016)
countAgeCheek(data = wildEcoData, jaartallen = 2016:2017)



## PLOT 4: Counts per year and age ##

allPlots <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
            
            categories <- getCategories(iSoort)
            
            plotData <- ecoData[ecoData$wildsoort == iSoort, ]
            
            lapply(c("count", "percent"), function(summarizeBy) {
                        
                        res <- countYearAge(data = plotData, 
                                jaartallen = min(plotData$afschotjaar):max(plotData$afschotjaar),
                                summarizeBy = summarizeBy)
                        
                        expect_equal(levels(res$data$kaak), c(categories, "Niet ingezameld"))
                        
                        res
                    })
            
        })
allPlots

# Some special cases
countYearAge(data = wildEcoData, jaartallen = 2016)
countYearAge(data = wildEcoData, jaartallen = 2016, summarizeBy = "percent")


countYearAge(data = wildEcoData, jaartallen = 2016:2017)
countYearAge(data = wildEcoData, jaartallen = 2016:2017, summarizeBy = "percent")

## PLOT 5: yearly percentage of shot animals
allPlots <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
            
            plotData <- ecoData[ecoData$wildsoort == iSoort, ]
            
            openingSeasonData <- openingstijdenData[
                    openingstijdenData$Soort == iSoort, ]
            
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
                        
                        lapply(openingstijd, function(jaar){
                                    
                                    percentageYearlyShotAnimals(
                                            data = plotData, 
                                            openingstijdenData = openingSeasonData,
                                            type = type,
                                            jaar = jaar,
                                            jaartallen = openingstijd
                                    )
                                    
                                })
                        
                    })
            
        })



## PLOT 6: Percentages per age and gender

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
            
            plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
            res <- countAgeGender(data = plotData)
            
            expect_equal(levels(res$data$leeftijd), getCategories(wildsoort))
            
            res
            
        })
allPlots

countAgeGender(data = wildEcoData, jaartallen = 2016)
countAgeGender(data = wildEcoData, jaartallen = 2016:2017)



## PLOT 7: Distribution of weight ifo age ##

# Inspect age
toInspect <- with(ecoData, which(leeftijd_comp == "Onbekend" & !is.na(leeftijd_maanden)))
xtabs(~ afschotjaar + wildsoort, ecoData[toInspect, ])
xtabs(~ afschotjaar + leeftijd_comp_bron, ecoData[toInspect, ], addNA = TRUE)

# How many Frislings removed?
xtabs(~ leeftijd_maanden + leeftijd_comp, ecoData[ecoData$wildsoort == "Wild zwijn", ], addNA = TRUE)
areRemoved <- with(ecoData, which(leeftijd_comp == "Frisling" & is.na(leeftijd_maanden))) 
length(areRemoved)

allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
            
            plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
            boxAgeWeight(data = plotData, type = unique(plotData$Leeftijdscategorie_onderkaak))
            
        })
allPlots

# Some special cases
boxAgeWeight(data = wildEcoData, jaartallen = 2016, 
        type = unique(wildEcoData$Leeftijdscategorie_onderkaak))
boxAgeWeight(data = wildEcoData, jaartallen = 2016:2017,
        type = unique(wildEcoData$Leeftijdscategorie_onderkaak))


## PLOT 9: Distribution of cheek length vs class ##

res <- boxAgeGenderLowerJaw(
        data = reeEcoData, 
        jaartallen = unique(reeEcoData$afschotjaar),
        type = unique(reeEcoData$leeftijd_comp)
)

## PLOT 10: bioindicators

## -- outdated -- ## 
#indicators <- c("onderkaaklengte", "ontweid_gewicht", "aantal_embryos")
if (FALSE) {
    
    ## PLOT 11: Number of embryos (bio-indicator
    
    pl <- countEmbryos(
            data = reeEcoData,
#    jaartallen = unique(reeEcoData$afschotjaar)
            jaartallen = 2002:2010
    )
    pl$plot
    head(pl$data)
    
}



indicators <- c("onderkaaklengte", "ontweid_gewicht")

tmp <- lapply(indicators, function(bioindicator){
            message("Plot for ", bioindicator, " in progress")
            pl <- plotBioindicator(
                    data = reeEcoData, 
#		type = "Geitkits",	
                    jaartallen = unique(reeEcoData$afschotjaar),
                    bioindicator = bioindicator,
                    sourceIndicator = c("inbo", "meldingsformulier", "both")[3]
            )
#	print(pl)
        })




## -- outdated -- ## 
if (FALSE) {
    
    ## PLOT 12: Percentage realised afschot
    percentageRealisedShotAnimals(data = reeEcoData, assignedData = toekenningsData,
            type = NULL, summarizeBy = "count")
    percentageRealisedShotAnimals(data = reeEcoData, assignedData = toekenningsData,
            type = NULL, summarizeBy = "percent")
    
}


## THE MAP

for (regionLevel in names(spatialData)[1:5]) {
    
    for (iSpecies in species) {
        
        spaceData <- createSpaceData(
                data = geoData, 
                allSpatialData = spatialData,
                year = 2016,
                species = iSpecies,
                regionLevel = regionLevel,
                unit = c("absolute", "relative")[2]
        )
        
        cat("*", regionLevel, "\n")
        print(summary(spaceData$data$freq))
        
        myPlot <- mapFlanders(
                allSpatialData = spatialData, 
                regionLevel = regionLevel, 
                colorScheme = c("white", RColorBrewer::brewer.pal(
                                n = nlevels(spaceData$data$group) - 1, name = "YlOrBr")),
                summaryData = spaceData$data,
                legend = "topright",
                species = iSpecies
        )
        
        print(myPlot)
        
    }
    
}


# TREND plots

for (iSpecies in species) {
    
#    print(iSpecies)
    
    for (regionLevel in names(spatialData)[1:5]) {
        
#        print(regionLevel)
        
        trendData <-  createTrendData(
                data = geoData[geoData$wildsoort == iSpecies, ],
                allSpatialData = spatialData,
                timeRange = c(2014, 2019),
                species = iSpecies,
                regionLevel = regionLevel,
                unit = "relative"
        )
        
        trendYearFlanders(
                data = trendData,
                timeRange = c(2014, 2019),
                unit = "relative"
        )$plot
        
    }
    
}