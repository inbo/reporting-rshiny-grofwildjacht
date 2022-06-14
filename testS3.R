# Project: inbo-grofwildjacht_git
# 
# Test connection with S3 bucket
###############################################################################


## INFO: https://www.gormanalysis.com/blog/connecting-to-aws-s3-with-r/

# credentials are in ~/.aws/credentials
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


## How to configure the application.yaml
#- id: euraa
#display-name: EU-RAA
#description: EU Risk Assessment Agenda Projects and Partnering
#container-env:
#  EMAIL_PASSWORD: secretKeyRef:efsa-smtp-password:password
#EMAIL_USERNAME: no.reply@efsa.europa.eu
#EMAIL_SMTP_HOST: smtp.office365.com
#EMAIL_SMTP_PORT: 587
#EMAIL_FROM: no.reply@efsa.europa.eu
#RETHINKDB_HOST: "10.250.7.98"
#RETHINKDB_PORT: 28015
#R4EU_INTEGRATION_ENABLED: TRUE
#R4EU_INTEGRATION_ENDPOINT: "https://r4eu.efsa.europa.eu/app/database"
#R_CONFIG_ACTIVE: shiny-efsa
#container-cmd: ["R", "-e euraa::runEURAA()"]
#container-image: r4eucr.azurecr.io/efsa/euraa:latest
#access-groups: [R4EU_ra, R4EU_efsastaff]

