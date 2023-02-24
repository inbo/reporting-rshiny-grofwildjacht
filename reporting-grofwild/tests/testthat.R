# Main file for running the tests
# 
# Author: mvarewyck
###############################################################################



library(testthat)
library(reportingGrofwild)

setupS3()

doPrint <- FALSE
dataDir <- file.path("~/git/reporting-rshiny-grofwildjacht/dataS3")


test_check("reportingGrofwild")