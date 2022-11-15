# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


context("Test Data Loading")


## INFO: https://www.gormanalysis.com/blog/connecting-to-aws-s3-with-r/

# credentials are in ~/.aws/credentials OR manually copy/paste OR using aws.signature::
x <- rawToChar(readBin("~/.aws/credentials", "raw", n = 1e5L))
profile <- Sys.getenv("AWS_PROFILE", "inbo")
credentials <- strsplit(x, profile)[[1]][2]

Sys.setenv(
  AWS_DEFAULT_REGION = eval(parse(text = config::get("credentials")$region)),
  AWS_ACCESS_KEY_ID = strsplit(strsplit(credentials, "aws_access_key_id = ")[[1]][2], "\n")[[1]][1], 
  AWS_SECRET_ACCESS_KEY = strsplit(strsplit(credentials, "aws_secret_access_key = ")[[1]][2], "\n")[[1]][1]
)

#Sys.setenv(
#  AWS_DEFAULT_REGION = config::get("credentials")$region,
#  AWS_ACCESS_KEY_ID = config::get("credentials")$id, 
#  AWS_SECRET_ACCESS_KEY = config::get("credentials")$secret
#)

test_that("Connection S3", {
    
    checkS3()
    
    # List all available files on the S3 bucket
    tmpTable <- data.table::rbindlist(aws.s3::get_bucket(bucket = config::get("bucket")))
    unique(tmpTable$Key)
    
  })


test_that("Upload data", {
    
    skip("WARNING: This will add/remove files")
    
    dataFiles <- list.files(system.file("extdata", package = "reportingGrofwild"), full.names = TRUE)
    dataFiles <- dataFiles[!grepl("readme|uiText|meta_schade", dataFiles)]
    
    response <- sapply(dataFiles, function(iFile) {
        put_object(file = iFile, object = basename(iFile), bucket = config::get("bucket"), multipart = TRUE)
      })
    # Warning messages: https://github.com/cloudyr/aws.s3/issues/354
    expect_true(all(response))
    
    if (FALSE) {
      iFile <- basename(grep("meta_schade", dataFiles, value = TRUE))
      aws.s3::delete_object(iFile, bucket = config::get("bucket"))
    }
    
  })

# This will update 
# (1) spatialData.RData, spatialDataWBE.RData shape data and
# (2) gemeentecodes.csv, file for matching NIS to NAAM
# Next, install the package for the latest files to be available from the extdata folder



## 0. Update Shape Data
## --------------------

test_that("Create Shape Data", {
    
    skip("WARNING: This will update and upload them to the S3 bucket in config::get('bucket')")
    # (1) spatialData.RData, shape data and
    # (2) gemeentecodes.csv, file for matching NIS to NAAM
    
    readShapeData(jsonDir = "~/git/reporting-rshiny-grofwildjacht/data")   # created shape data
    
  })


test_that("Read Shape Data", {
    
    suppressWarnings(rm(spatialData))
    readS3(file = "spatialData.RData")
    expect_true("spatialData" %in% ls()) 
    
  })


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
                
        hist(myData$afschotjaar, main = paste(iSoort, "- afschotjaar"))
        barplot(table(myData$provincie), main = paste(iSoort, "- provincie"))
        barplot(table(myData$geslacht_comp), main = paste(iSoort, "- geslacht"))
        
        hist(myData$ontweid_gewicht, main = paste(iSoort, "- ontweid_gewicht"))
#			if (iSoort == "Wild zwijn")
#				expect_true(myData$ontweid_gewicht < 200) else if (iSoort == "Ree")
#				expect_true(4 < myData$ontweid_gewicht & myData$ontweid_gewicht < 40)
        
        hist(myData$aantal_embryos, main = paste(iSoort, "- aantal_embryos"))
        
        if (iSoort %in% c("Ree")) {
          
          hist(myData$onderkaaklengte_mm, main = paste(iSoort, "- onderkaaklengte"), border = "red",
            xlim = range(myData[ , c("onderkaaklengte_mm", "onderkaaklengte", "onderkaaklengte_comp")], na.rm = TRUE))
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
      plot(spatialData[[iLevel]], col = RColorBrewer::brewer.pal(9, "Set1"))
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
    expect_warning(wildschadeData <- loadRawData(type = "wildschade"))
    
    dim(wildschadeData)
    head(wildschadeData)
    
    # Correct names for commune shape data?
    notMatching <- which(!wildschadeData$gemeente_afschot_locatie %in% spatialData$communes@data$NAAM)
    expect_equal(0, length(notMatching[!is.na(wildschadeData$gemeente_afschot_locatie[notMatching])]))
    
  })


## 4. Other
## --------

test_that("Read csv data from S3", {
    
    rawData <- readS3(FUN = read.csv, file = "FaunabeheerDeelzones_0000_2018_habitats.csv") 
    expect_is(rawData, "data.frame")
    
  })


test_that("Speed up reading data", {
    
    skip("Long runtime")
    
    dataFile <- c("Gemeentes_habitats.csv", "rshiny_reporting_data_ecology.csv")[2]
    bucket <- config::get("bucket")
    
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
      
      allFiles <- aws.s3::get_bucket(bucket = config::get("bucket"))
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