# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


context("Test Data Loading")


## 0. Update Data
## --------------------

# This will update 
# (1) spatialData.RData, shape data and
# (2) gemeentecodes.csv, file for matching NIS to NAAM
# Next, install the package for the latest files to be available from the extdata folder

# createShapeData()   # created shape data

# Load the shape data
load(file = file.path(dataDir, "spatialData.RData"))

# This will update waarnemingen
# createWaarnemingenData()

# This will update trafficData
#createTrafficData()



## 1. Ecological Data
## -------------------


ecoData <- loadRawData(type = "eco")

test_that("Ecological data", {
    
    expect_equal(unique(ecoData$doodsoorzaak), "afschot")
    
    # check ecological data
    myTab <- table(ecoData$wildsoort)
    wildsoorten <- names(myTab)[myTab > 1]
    
    plotFile <- file.path(tempdir(), "checkEcoData.pdf")
    pdf(plotFile)
    lapply(wildsoorten, function(iSoort) {
        
        print(iSoort)
        
        myData <- subset(ecoData, wildsoort == iSoort)
        myData$onderkaaklengte <- rowMeans(myData[, c("onderkaaklengte_links", "onderkaaklengte_rechts")], na.rm = TRUE)
        
        expect_true(all(myData$doodsoorzaak == "afschot"))
        
        hist(myData$afschotjaar, main = paste(iSoort, "- afschotjaar"))
        barplot(table(myData$provincie), main = paste(iSoort, "- provincie"))
        barplot(table(myData$geslacht_comp), main = paste(iSoort, "- geslacht"))
        
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
        
        boxplot(as.numeric(leeftijd_maanden) ~ leeftijd_comp, data = myData, main = paste(iSoort, "- leeftijd (maanden)"))
        
        xtabs(~ Leeftijdscategorie_onderkaak + leeftijdscategorie_MF + leeftijd_comp, data = myData)
        
      })
    
    dev.off()
    
    expect_true(file.exists(plotFile))
    
  })


## 2. Geographical Data
## --------------------

test_that("Spatial data", {
    
    plotFile <- file.path(tempdir(), "checkGeoData.pdf")
    pdf(plotFile)
    for (iLevel in names(spatialData)) {
      print(iLevel)
      plot(spatialData[[iLevel]], col = RColorBrewer::brewer.pal(10, "Set1"))
    }
    dev.off()
    
    expect_true(file.exists(plotFile))
    
  })


test_that("Geographical data", {
    
    # Can we combine data sources? 
    geoData <- loadRawData(type = "geo")
    tmp <- merge(geoData, ecoData)
    
    # Correct names for commune shape data?
    notMatching <- which(!geoData$gemeente_afschot_locatie %in% spatialData$communes@data$NAAM)
    expect_equal(0, length(notMatching[!is.na(geoData$gemeente_afschot_locatie[notMatching])]))
    
    
  })


## 3. Wildschade
## -------------------

test_that("Wildschade data", {
    
    # Can we combine data sources? 
    wildschadeData <- loadRawData(type = "wildschade")
    
    dim(wildschadeData)
    head(wildschadeData)
    
    # Correct names for commune shape data?
    notMatching <- which(!wildschadeData$gemeente_afschot_locatie %in% spatialData$communes@data$NAAM)
    expect_equal(0, length(notMatching[!is.na(wildschadeData$gemeente_afschot_locatie[notMatching])]))
    
  })
