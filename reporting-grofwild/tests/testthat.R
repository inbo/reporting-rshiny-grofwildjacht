# Main file for running the tests
# 
# Author: mvarewyck
###############################################################################



library(testthat)
library(reportingGrofwild)



dataDir <- system.file("extdata", package = "reportingGrofwild")


test_check("reportingGrofwild")