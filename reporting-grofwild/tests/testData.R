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
expect_equal(unique(ecoData$doodsoorzaak), "afschot")

# check ecological data
myTab <- table(ecoData$wildsoort)
wildsoorten <- names(myTab)[myTab > 1]

pdf(file.path(tempdir(), "checkEcoData.pdf"))
lapply(wildsoorten, function(iSoort) {
			
			print(iSoort)
			
			myData <- subset(ecoData, wildsoort == iSoort)
			myData$onderkaaklengte <- rowMeans(myData[, c("onderkaaklengte_links", "onderkaaklengte_rechts")], na.rm = TRUE)
			
			expect_true(all(myData$doodsoorzaak == "afschot"))
			
			hist(myData$afschotjaar, main = paste(iSoort, "- afschotjaar"))
			barplot(table(myData$provincie), main = paste(iSoort, "- provincie"))
			barplot(table(myData$geslacht), main = paste(iSoort, "- geslacht"))
			
			hist(myData$ontweid_gewicht, main = paste(iSoort, "- ontweid_gewicht"))
#			if (iSoort == "Wild zwijn")
#				expect_true(myData$ontweid_gewicht < 200) else if (iSoort == "Ree")
#				expect_true(4 < myData$ontweid_gewicht & myData$ontweid_gewicht < 40)
			
			hist(myData$aantal_embryos, main = paste(iSoort, "- aantal_embryos"))

			if (iSoort %in% c("Ree")) {
				
				hist(myData$lengte_mm, main = paste(iSoort, "- onderkaaklengte"), border = "red",
						xlim = range(myData[ , c("lengte_mm", "onderkaaklengte", "onderkaaklengte_comp")], na.rm = TRUE))
				hist(myData$onderkaaklengte, add = TRUE, border = "blue")
				hist(myData$onderkaaklengte_comp, add = TRUE)
				
			}
			
			boxplot(leeftijd_maanden ~ leeftijd_comp, data = myData, main = paste(iSoort, "- leeftijd (maanden)"))
			
			xtabs(~ Leeftijdscategorie_onderkaak + leeftijdscategorie_MF + leeftijd_comp, data = myData)
			
		})

dev.off()


# Can we combine data sources? 
geoData <- loadRawData(type = "geo", shapeData = spatialData)
tmp <- merge(geoData, ecoData)