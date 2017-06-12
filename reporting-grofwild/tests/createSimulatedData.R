# Project: create simulated data
# 
# Author: lcougnaud
###############################################################################

if(FALSE){

	library(reportingGrofwild)
	
	dataDir <- system.file("extdata", package = "reportingGrofwild")
	
	provinceData <- readShapeData(zipFile = file.path(dataDir, "provinces.zip"))
	provincesNaam <- unique(provinceData@data$NAAM)
	nProvinces <- length(provincesNaam)
	communeData <-  readShapeData(zipFile = file.path(dataDir, "communes.zip"))
	communesNaam <- unique(communeData@data$NAAM)
	nCommunes <- length(communesNaam)
	
	#head(communeData@data)
	#provinceData@data
	
	species <- c("everzwijn", "ree", "damhert", "edelhert", "moeflon")
	nSpecies <- length(species)
	
	years <- 2008:2017
	nYears <- length(years)
	
	#nReplicates <- 10
	
	
	createCountsData <- function(type = c("province", "commune")){
		nEntities <- switch(type, 'province' = nProvinces, 'commune' = nCommunes)
		countsData <- data.frame(
			NAAM = rep(
				switch(type,
					'province' = provincesNaam,
					'commune' = communesNaam
				), times = nSpecies * nYears),
			specie = rep(species, each = nEntities * nYears),
			year = rep(years, each = nSpecies),
			counts = sample.int(
				50, 
				size = nSpecies * nEntities * nYears, 
				replace = TRUE
			)
		)
		return(countsData)
	}
	
	countsData <- sapply(c('province', 'commune'), createCountsData, simplify = FALSE)
	save(countsData, file = "./grofWild/inst/extdata/countsData.RData")

}
