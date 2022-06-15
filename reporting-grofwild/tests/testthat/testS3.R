# Project: inbo-grofwildjacht_git
# 
# Test connection with S3 bucket
###############################################################################


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
    data.table::rbindlist(aws.s3::get_bucket(bucket = config::get("bucket")))
    
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


test_that("Read data", {
    
    # Rdata
    suppressWarnings(rm(spatialData))
    readS3(file = "spatialData.RData")
    expect_true("spatialData" %in% ls()) 
    
    # csv
    rawData <- readS3(FUN = read.csv, file = "FaunabeheerDeelzones_0000_2018_habitats.csv") 
    expect_is(rawData, "data.frame")
    
  })

test_that("Speed up reading data", {
    
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


## How to configure the application.yaml
#- id: euraa
#display-name: EU-RAA
#description: EU Risk Assessment Agenda Projects and Partnering
#container-env:
#EMAIL_USERNAME: no.reply@efsa.europa.eu
#EMAIL_SMTP_HOST: smtp.office365.com
#EMAIL_SMTP_PORT: 587
#EMAIL_FROM: no.reply@efsa.europa.eu
#R_CONFIG_ACTIVE: shiny-efsa
#container-cmd: ["R", "-e euraa::runEURAA()"]
#container-image: r4eucr.azurecr.io/efsa/euraa:latest
#access-groups: [R4EU_ra, R4EU_efsastaff]

