# TODO: Add comment
# 
# Author: mvarewyck
###############################################################################


context("Test Memory Usage & Processing Time")

library(profvis)
setupS3()


test_that("Loading DaTa", {
    
    skip("Explorative use only")
    
# Size of S3 files
    tmpTable <- aws.s3::get_bucket_df(
      bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")))
# unique(tmpTable$Key)
    allSizes <- data.frame(name = tmpTable$Key, size = as.numeric(tmpTable$Size))
    allSizes <- allSizes[order(allSizes$size, decreasing = TRUE), ]
    allSizes$size <- sapply(allSizes$size, function(x) utils:::format.object_size(x, "auto"))
    head(allSizes, 20)
#                                                name     size
# 41                                 spatialData.gpkg 176.2 Mb
# 42                             spatialDataWBE.RData 122.5 Mb
# 237                         spatialDataWBE_sf.RData 118.1 Mb
# 26                            WildSchade_georef.csv  37.4 Mb
# 34                rshiny_reporting_data_ecology.csv  33.9 Mb
# 36      rshiny_reporting_data_ecology_processed.csv  31.5 Mb
# 39    rshiny_reporting_data_geography_processed.csv  19.4 Mb
# 238                            spatialData_sf.RData  19.1 Mb
# 37              rshiny_reporting_data_geography.csv  18.7 Mb
# 28                  WildSchade_georef_processed.csv  18.2 Mb
# 29                                   baseData.RData  12.1 Mb
# 40                                spatialData.RData   7.6 Mb
# 240                             spreadData_sf.RData   6.3 Mb
# 239                                spreadData.RData   6.3 Mb
# 27                WildSchade_georef_processed.RData   4.2 Mb
# 38  rshiny_reporting_data_geography_processed.RData   3.9 Mb
# 35    rshiny_reporting_data_ecology_processed.RData   3.6 Mb
# 100                        spatialDataWBE/240.RData   3.3 Mb
# 85                         spatialDataWBE/222.RData   2.2 Mb
# 78                         spatialDataWBE/214.RData   1.9 Mb
    
#                                            name     size
# 41                             spatialData.gpkg 176.2 Mb
# 42                         spatialDataWBE.RData 122.5 Mb
# 237                     spatialDataWBE_sf.RData 118.1 Mb
# 26                        WildSchade_georef.csv  37.4 Mb
# 34            rshiny_reporting_data_ecology.csv  33.9 Mb
# 36  rshiny_reporting_data_ecology_processed.csv  31.5 Mb
    rm(allSizes)
    
    
# Load all data before starting the app - memory usage
    profvis({
        source("/home/mvarewyck/git/reporting-rshiny-grofwildjacht/reporting-grofwild/inst/ui/app_private/global.R")
      })
    
    ## ADMIN
    
# Object sizes
    allSizes <- sapply(mget(ls()), object.size) 
    allSizes <- sort(allSizes, decreasing = TRUE)
    sapply(allSizes, function(x) utils:::format.object_size(x, "auto"))
#     spatialData         ecoData         geoData      schadeData     habitatData 
#      "180.1 Mb"       "28.9 Mb"       "25.6 Mb"       "11.4 Mb"      "752.7 Kb" 
# toekenningsData      currentKbo         rawData          uiText     matchingKbo 
#      "479.1 Kb"         "25 Kb"       "16.3 Kb"       "14.6 Kb"        "1.2 Kb" 
#      currentWbe        allTimes         dataDir         js_code outTempFileName 
#     "800 bytes"     "464 bytes"     "232 bytes"     "232 bytes"     "152 bytes" 
#     defaultYear         doDebug     biotoopData 
#      "56 bytes"      "56 bytes"       "0 bytes" 
    
    utils:::format.object_size(sum(allSizes), "auto")
# [1] "247.2 Mb"
    
    
    
    
# SINGLE WBE
    
# Object sizes
    allSizes <- sapply(mget(ls()), object.size) 
    allSizes <- sort(allSizes, decreasing = TRUE)
    sapply(allSizes, function(x) utils:::format.object_size(x, "auto"))
#         ecoData         geoData      schadeData     spatialData     habitatData 
#       "28.9 Mb"       "25.6 Mb"       "11.4 Mb"     "1010.4 Kb"      "752.7 Kb" 
#         rawData          uiText toekenningsData        allTimes      currentKbo 
#       "16.3 Kb"       "14.6 Kb"        "7.1 Kb"     "464 bytes"     "288 bytes" 
#         dataDir         js_code outTempFileName      currentWbe     defaultYear 
#     "232 bytes"     "232 bytes"     "152 bytes"      "56 bytes"      "56 bytes" 
#         doDebug     matchingKbo     biotoopData 
#      "56 bytes"      "56 bytes"       "0 bytes" 
    
    utils:::format.object_size(sum(allSizes), "auto")
# [1] "67.7 Mb"
    
    format(object.size(spatialData), "auto")
# [1] "1010.4 Kb"
    
    
    
# PUBLIC
    
    
# Load all data before starting the app - memory usage
    rm(list = ls())
    profvis({
        source("/home/mvarewyck/git/reporting-rshiny-grofwildjacht/reporting-grofwild/inst/ui/app_public/global.R")
      })
    
# Object sizes
    allSizes <- sapply(mget(ls()), object.size) 
    allSizes <- sort(allSizes, decreasing = TRUE)
    sapply(allSizes, function(x) utils:::format.object_size(x, "auto"))
#             ecoData             geoData         spatialData             rawData 
#           "28.9 Mb"           "25.6 Mb"           "24.6 Mb"           "20.5 Mb" 
#          schadeData         biotoopData         habitatData     toekenningsData 
#           "20.5 Mb"          "752.7 Kb"          "752.7 Kb"          "460.8 Kb" 
#              uiText       availableData  openingstijdenData          metaSchade 
#           "40.3 Kb"           "11.2 Kb"           "11.1 Kb"             "11 Kb" 
#           fullNames   schadeWildsoorten         schadeCodes       sourcesSchade 
#            "4.2 Kb"            "1.8 Kb"            "1.7 Kb"         "832 bytes" 
#         schadeTypes maatschappijChoices               time1    populatieChoices 
#         "616 bytes"         "376 bytes"         "344 bytes"         "304 bytes" 
#        jachtChoices       schadeChoices             dataDir             js_code 
#         "248 bytes"         "248 bytes"         "232 bytes"         "232 bytes" 
#         defaultYear             doDebug 
#          "56 bytes"          "56 bytes" 
    
    
    utils:::format.object_size(sum(allSizes), "auto")
# [1] "122.2 Mb"
    
    format(object.size(spatialData), "auto")
# [1] "24.6 Mb"
    
  })


test_that("Starting the app", {
    
    skip("Explorative use only")
    
    
# Data checks before app starts - memory usage + time
    profvis({
        testS3()
        # Time difference of 1.252697 mins
        # Memory: 390 Mb
        ## Most time is spent on loading spatial data for all WBEs (1 minute)
      })
    
    
# Start private app - memory usage
    profvis({
        setupS3()
        setwd("/home/mvarewyck/git/reporting-rshiny-grofwildjacht/reporting-grofwild/inst/ui/app_private")
        runApp()
        ## Single WBE
        # Time difference of 6.683212 secs
        # Memory: 110 Mb
        
        ## Admin
        # Time difference of 37.17669 secs
        # Memory: 300 Mb
      })
    
    # Data checks & start app - memory usage + time
    profvis({
        runWildApp(public = FALSE, kbo = 445465768)
        # Memory: 450 Mb
      })
    
    
  })
