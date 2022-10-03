# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Run the reporting-grofwild application
#' @param installDependencies boolean, whether to first install packages listed
#' in the Suggests field of DESCRIPTION; default value is FALSE
#' @param public boolean, whether to start the public or private version of the app
#' @param kbo numeric, specific KBO number for which to show the WBE (private) app;
#' only relevant if \code{public} is FALSE
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @import shiny
#' @importFrom remotes install_github dev_package_deps
#' @importFrom stats update
#' @export
runWildApp <- function(installDependencies = FALSE, 
  public = TRUE, kbo = "", ...) {
  
  # (1) Install all suggested R packages (see DESCRIPTION)
  if (installDependencies) {
    
    ## (a) CRAN packages
    update(remotes::dev_package_deps(pkgdir = system.file("", package = "reportingGrofwild"), 
            dependencies = "Suggests"))
    
    
    ## (b) non-CRAN packages - by hand
    if (!requireNamespace("INBOtheme")) {
      
      remotes::install_github('inbo/INBOtheme')
      
    }
    
  }
  
  # (2) Copy the UI files & folders from "inst/ui" for local use
  
  if (public)
    appDir <- system.file("ui/app_public", package = "reportingGrofwild") else
    appDir <- system.file("ui/app_private", package = "reportingGrofwild")
  
  
  # (3) Specific WBE
  # WARNING: This overrides the kbo read from shinyproxy
  Sys.setenv("SHINYPROXY_USERNAME" = kbo)
  
  
  # (4) Run the application
  runApp(appDir = appDir, ...)
  
}
