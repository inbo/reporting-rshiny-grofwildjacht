
#' Setup connection to S3 bucket based on AWS file
#' 
#' INFO: https://www.gormanalysis.com/blog/connecting-to-aws-s3-with-r/
#' @param awsFile path to AWS file
#' @return no return value, ENV variables are set correctly
#' 
#' @author mvarewyck
#' @export
setupS3 <- function(awsFile = "~/.aws/credentials") {
  
  # credentials are in ~/.aws/credentials OR manually copy/paste OR using aws.signature::
  x <- rawToChar(readBin(awsFile, "raw", n = 1e5L))
  profile <- Sys.getenv("AWS_PROFILE")
  credentials <- strsplit(x, profile)[[1]][2]
  
  Sys.setenv(
    AWS_DEFAULT_REGION = eval(parse(text = config::get("credentials", file = system.file("config.yml", package = "reportingGrofwild"))$region)),
    AWS_ACCESS_KEY_ID = strsplit(strsplit(credentials, "aws_access_key_id = ")[[1]][2], "\n")[[1]][1], 
    AWS_SECRET_ACCESS_KEY = strsplit(strsplit(credentials, "aws_secret_access_key = ")[[1]][2], "\n")[[1]][1]
  )
  
}
  
  
#' Quick check for valid user credentials to make connection with S3 bucket
#' @return no return value
#' 
#' @author mvarewyck
#' @export
checkS3 <- function() {
  
  credentials <- Sys.getenv(c("AWS_DEFAULT_REGION", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"))
  if (any(credentials == ""))
    stop("Please specify 'Sys.setenv()' for ", 
      paste(names(credentials)[which(credentials == "")], collapse = ", "))
  
  
  
}


#' Download files from the S3 bucket
#' @param FUN function, which function should be called to download the data;
#' if .RData file this argument can be empty 
#' @param ... additional arguments to \code{FUN}
#' @param file character, name of the file to be downloaded
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-wbe-uat-data"
#' @return depending on the input file
#' \itemize{
#' \item{rdata}{no return value,
#' the R object (data.frame or list) loaded from the \code{file} is assigned to
#' the \code{.GlobalEnv}}
#' \item{other files}{R object (data.frame or list) is returned}
#' }
#' @author mvarewyck
#' @importFrom aws.s3 s3load s3read_using
#' @export
readS3 <- function(FUN = read.csv, ..., file, 
  bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild"))) {
  
  if (tolower(tools::file_ext(file)) == "rdata") {
    
    s3load(bucket = bucket, object = basename(file), envir = .GlobalEnv)
    
  } else {
    
    s3read_using(FUN = FUN, ..., bucket = bucket, object = basename(file))
  
  }

}



#' Write files to the S3 bucket
#' @param dataFiles character vector, path to files to be uploaded
#' @return boolean vector, TRUE for each dataFile that was uploaded correctly
#' 
#' @author mvarewyck
#' @importFrom aws.s3 put_object
#' @importFrom config get
#' @export
writeS3 <- function(dataFiles) {
      
      response <- sapply(dataFiles, function(iFile) {
          put_object(file = iFile, object = basename(iFile), 
            bucket = config::get("bucket", file = system.file("config.yml", package = "reportingGrofwild")), 
            multipart = TRUE)
        })
      
      response
      
}

