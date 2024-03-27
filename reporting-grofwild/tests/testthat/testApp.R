# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



context("Test Shiny Apps")


test_that("Public app does not crash on startup", {   
    
    shiny::testServer(
      app = system.file("ui/app_public", package = "reportingGrofwild"),
      expr = testthat::expect_true(TRUE)
    )
    
  })


test_that("Private app does not crash on startup", {   
    
    shiny::testServer(
      app = system.file("ui/app_private", package = "reportingGrofwild"),
      expr = testthat::expect_true(TRUE)
    )
    
  })
