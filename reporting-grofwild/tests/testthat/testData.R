# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


context(paste("Test Data Loading", config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))))

# setupS3()

test_that("Connection to S3", {
    
    checkS3()
    
    # List all available files on the S3 bucket
    tmpTable <- data.table::rbindlist(aws.s3::get_bucket(
        bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))))
    # unique(tmpTable$Key)
    
    # Bucket is not empty
    expect_gte(length(unique(tmpTable$Key)), 1)
    
#    # Read single file
#    rawData <- readS3(FUN = read.csv, file = unique(tmpTable$Key)[1]) 
#    expect_is(rawData, "data.frame")
        
  })


test_that("Upload data", {
    
    skip("WARNING: This will add/remove files from S3 bucket")
    
    dataFiles <- list.files(system.file("extdata", package = "reportingGrofwild"), full.names = TRUE)
    dataFiles <- dataFiles[!grepl("readme|uiText|meta_schade", dataFiles)]
    
    response <- writeS3(dataFiles = dataFiles)
    # Warning messages: https://github.com/cloudyr/aws.s3/issues/354
    expect_true(all(response))
    
    if (FALSE) {
      iFile <- basename(grep("meta_schade", dataFiles, value = TRUE))
      aws.s3::delete_object(iFile, 
        bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")))
    }
        
  })




## 1. Shape Data
## --------------

# This will update 
# (1) spatialData.RData, spatialDataWBE.RData shape data and
# (2) gemeentecodes.csv, file for matching NIS to NAAM
# Next, install the package for the latest files to be available from the extdata folder

test_that("Create Shape Data", {
    
    skip("WARNING: This will update and upload spatial data to the S3 bucket")
    # (1) spatialData.RData, shape data and
    # (2) gemeentecodes.csv, file for matching NIS to NAAM
    
    readShapeData(jsonDir = "~/git/reporting-rshiny-grofwildjacht/data")   # created shape data
    
  })


test_that("Read Shape Data", {
    
    # All
    suppressWarnings(rm(spatialData))
    readS3(file = "spatialData.RData")
    expect_true(exists("spatialData"), info = "spatialData.RData")
    
    # WBE
    suppressWarnings(rm(spatialDataWBE))
    readS3(file = "spatialDataWBE.RData")
    expect_true(exists("spatialDataWBE"), info = "spatialDataWBE.RData")
    
  })


## 2. Raw Data
## ------------

test_that("Eco, geo, schade data", {
    
    ecoData <- loadRawData(type = "eco")
    expect_is(ecoData, "data.frame", info = "rshiny_reporting_data_ecology.csv")
    
    geoData <- loadRawData(type = "geo")
    expect_is(geoData, "data.frame", info = "rshiny_reporting_data_geography.csv")
    
    # Can we combine data sources? 
    tmp <- merge(geoData, ecoData)
    
    # Correct names for commune shape data?
    notMatching <- which(!geoData$gemeente_afschot_locatie %in% spatialData$communes@data$NAAM)
    expect_equal(0, length(notMatching[!is.na(geoData$gemeente_afschot_locatie[notMatching])]))
    
    
    expect_warning(schadeData <- loadRawData(type = "wildschade"))
    expect_is(schadeData, "SpatialPointsDataFrame", info = "WildSchade_georef.csv")
    
    # Correct names for commune shape data?
    notMatching <- which(!schadeData$gemeente_afschot_locatie %in% spatialData$communes@data$NAAM)
    expect_equal(0, length(notMatching[!is.na(schadeData$gemeente_afschot_locatie[notMatching])]))
    
  })


## 3. Habitat Data
## ----------------

test_that("Habitat data", {
    
    regionLevels <- list(
      "flanders" = "flanders_habitats", 
      "provinces" = "Provincies_habitats", 
      "communes" = "Gemeentes_habitats", 
      "faunabeheerzones" = "Faunabeheerzones_habitats", 
      # "fbz_gemeentes" = "FaunabeheerDeelzones",  # currently missing see #295 
      "utm5" = "utm5_vlgrens_habitats", 
      "wbe" = "WBE_habitats"
    ) 
    
    # Tests both public & private (wbe)
    for (i in seq_along(regionLevels)) {
      
      biotoopData <- loadHabitats(spatialData = spatialData, regionLevels = names(regionLevels)[i])
      infoFiles <- grep(pattern = regionLevels[[i]],
        x = sapply(aws.s3::get_bucket(bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))), function(x) as.list(x)$Key),
        value = TRUE)
      expect_is(biotoopData, "list", info = toString(infoFiles))
      
    }
    
  })



## 4. Other
## --------

test_that("Extra data", {
    
    gemeenteData <- loadGemeentes()
    expect_is(gemeenteData, "data.frame", info = "gemeentecodes.csv")
    
    openingstijdenData <- loadOpeningstijdenData()
    expect_is(openingstijdenData, "data.frame", info = "Openingstijden_grofwild.csv")
    
    toekenningsData <- loadToekenningen()
    expect_is(toekenningsData, "data.frame", info = "Verwezenlijkt_categorie_per_afschotplan.csv")
        
  })



## 5. Debugging
## -------------


test_that("Speed up reading data", {
    
    skip("Long runtime")
    
    dataFile <- c("Gemeentes_habitats.csv", "rshiny_reporting_data_ecology.csv")[2]
    bucket <- config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))
    
    # High randomnesss in download (connection?)
    nRuns <- 20
    allTimes <- matrix(NA, nrow = 4, ncol = nRuns)
    for (i in 1:nRuns) {
      
      time1 <- Sys.time()
      tmp0 <- aws.s3::s3read_using(FUN = data.table::fread, bucket = bucket, object = dataFile)
      allTimes[1, i] <- Sys.time() - time1
      # Time difference of 9.25581 secs
      
      time1 <- Sys.time()
      tmp1 <- aws.s3::s3read_using(FUN = read.csv, bucket = bucket, object = dataFile)
      allTimes[2, i] <- Sys.time() - time1 
      # Time difference of 1.427831 secs
      
      time1 <- Sys.time()
      tmp2 <- data.table::fread(aws.s3::save_object(dataFile, bucket = bucket))
      allTimes[3, i] <- Sys.time() - time1
      # Time difference of 7.885844 secs
      
      allFiles <- aws.s3::get_bucket(bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")))
      fileNames <- sapply(allFiles, function(x) as.list(x)$Key)
      time1 <- Sys.time()
      tmp3 <- data.table::fread(aws.s3::save_object(allFiles[[grep(dataFile, fileNames)]]))
      allTimes[4, i] <- Sys.time() - time1
      # Time difference of 1.083263 secs
      
    }
    
    dataFile
    apply(allTimes, 1, median)
    apply(allTimes, 1, min)
    apply(allTimes, 1, max)
    apply(allTimes, 1, mean)
    
    
    
    # [1] "Gemeentes_habitats.csv"
    # [1] 0.9210250 1.0067641 0.9253262 0.9862000
    # [1] 0.5577002 0.3242614 0.2447915 0.3510141
    # [1] 9.593569 1.373696 2.449536 1.934339
    # [1] 1.4599329 0.9326187 0.9286930 1.0003309
    
#    con <- s3connection(dataFile, bucket = config::get("bucket"))
#    close(con)
    
  })