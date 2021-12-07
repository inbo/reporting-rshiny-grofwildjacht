# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



context("Test WBE")

# Load all data
load(file = file.path(dataDir, "spatialData.RData"))

ecoData <- loadRawData(type = "eco")