# Main file for running the tests
# 
# Author: mvarewyck
###############################################################################



library(testthat)
library(reportingGrofwild)

doPrint <- FALSE

setupS3()

test_check("reportingGrofwild")