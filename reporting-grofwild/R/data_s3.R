

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
readS3 <- function(FUN, ..., file, bucket = config::get("bucket")) {
  
  checkS3()
  
  if (tolower(tools::file_ext(file)) == "rdata") {
    
    s3load(bucket = bucket, object = basename(file), envir = .GlobalEnv)
    
  } else {
    
    s3read_using(FUN = FUN, ..., bucket = bucket, object = basename(file))
  
  }

}

