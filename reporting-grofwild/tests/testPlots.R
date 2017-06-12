# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################

library(reportingGrofwild)

# Load all data
ecologyData <- loadEcologyData()



## PLOT 1: Counts per year and province ##

allPlots <- lapply(levels(ecologyData$wildsoort), function(wildsoort) {
      
      plotData <- ecologyData[ecologyData$wildsoort == wildsoort, ]
      countYearProvince(data = plotData, wildNaam = wildsoort)
      
    })
allPlots

# Some special cases
wildData <- ecologyData[ecologyData$wildsoort == "Wild zwijn", ]
countYearProvince(data = wildData, wildNaam = "wild zwijn", 
    jaartallen = 2016, doodsoorzaak = "afschot")
countYearProvince(data = wildData, wildNaam = "wild zwijn",
    jaartallen = 2016:2017, doodsoorzaak = "afschot")



## PLOT 2: Table per age and province ##