# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Run the reporting-grofwild application
#' @param installDependencies boolean, whether to first install packages listed
#' in the Suggests field of DESCRIPTION; default value is FALSE
#' @param copyFiles boolean, whether to copy files for the app from the inst/ui
#' to a temporary directory which is writeable
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @import shiny
#' @importFrom devtools install_github dev_package_deps
#' @importFrom stats update
#' @export
runWildApp <- function(installDependencies = FALSE, copyFiles = FALSE, ...) {
  
  # (1) Install all suggested R packages (see DESCRIPTION)
  if (installDependencies) {
    
    ## (a) CRAN packages
    update(dev_package_deps(pkg = system.file("ui", package = "reportingGrofwild"), 
            dependencies = "Suggests"))
    
    
    ## (b) non-CRAN packages - by hand
    if (!requireNamespace("INBOtheme")) {
      
      install_github('inbo/INBOtheme')
      
    }
    
  }
  
  # (2) Copy the UI files & folders from "inst/ui" for local use
  
  if (copyFiles) {
    
    appDir <- tempdir()
    oldDir <- setwd(appDir)
    on.exit(setwd(oldDir))
    
    uiDir <- system.file("ui", package = "reportingGrofwild")
    uiFiles <- list.files(path = uiDir, full.names = FALSE, recursive = TRUE)
    
    sapply(uiFiles, function(from) {
          
          to <- file.path(appDir, from)
          toDir <- dirname(to)
          
          if (!dir.exists(toDir)) {
            
            dir.create(path = toDir, recursive = TRUE)
            
          }
          
          file.copy(from = file.path(uiDir, from), to = to, overwrite = TRUE)
          
        })  
    
    
  } else {
    
    appDir <- system.file("ui", package = "reportingGrofwild")  
    
  }
  
  
  # (3) Run the application
  runApp(appDir = appDir, ...)
  
}


