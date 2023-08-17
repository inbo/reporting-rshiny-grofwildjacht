# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Run the reporting-grofwild application
#' @param installDependencies boolean, whether to first install packages listed
#' in the Suggests field of DESCRIPTION; default value is FALSE
#' @param public boolean, whether to start the public or private version of the app
#' @param kbo character, specific KBO number(s) or 'admin' login to start the WBE (private) app;
#' only relevant if \code{public} is FALSE;
#' examples are "admin", '["454472813"]' or '["445465768","450506996","454472813"]'
#' Note: brackets can be left out, but are there if sent from shinyproxy
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @import shiny
#' @importFrom remotes install_github dev_package_deps
#' @importFrom stats update
#' @importFrom config get
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
  
  # (2) Point to the correct app directory  
  if (public)
    appDir <- system.file("ui/app_public", package = "reportingGrofwild") else
    appDir <- system.file("ui/app_private", package = "reportingGrofwild")
  
  
  # (3) Specific WBE
  # WARNING: This overrides the kbo read from shinyproxy
  if (kbo != "") {
    if (kbo == "admin")
      Sys.setenv("SHINYPROXY_USERGROUPS" = "WBE_ADMIN") else
      Sys.setenv("SHINYPROXY_KBO_NUMMERS" = kbo)
  }
  
  
  # (4) Check S3 data - On UAT only, not PRD
  if (config::get("datacheck", file = system.file("config.yml", package = "reportingGrofwild")))
    errorApp <- tryCatch(
      testS3(),
      error = function(err)
        shinyApp(ui = fluidPage(
            tags$h3("Error during Data Check"),
            HTML(err$message)
        ), server = function(input, output, session){})
      )
  
    
  # (5) Retrieve GIT Hash
  if (Sys.getenv("GIT_SHA") == "")
    # For local use only
    Sys.setenv("GIT_SHA" = tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(err) ""))
    
  
  # (6) Run the application
  if (exists("errorApp") && is(errorApp, "shiny.appobj"))
    errorApp else 
    runApp(appDir = appDir, ...)
  
}
