# Main file for running the tests
# 
# Author: mvarewyck
###############################################################################



library(testthat)
library(reportingGrofwild)



dataDir <- system.file("extdata", package = "reportingGrofwild")
doPrint <- FALSE

test_check("reportingGrofwild")