#' UI header element
#' @param offset integer with column offset for the first 
#' header element
#' @param color character, text color, 'black' by default
#' @param path character vector with elements to include in the
#' path, among: 'home', 'specie', 'category', 'plot' 
#' (NULL by default).
#' @param id character, module id/specie
#' @param specie (optional) hard-coded specie in the path (NULL by default)
#' @param ... extra parameters for \code{\link[shiny]{fluidRow}}
#' @author lcougnaud
#' @import shiny
#' @inherit shiny::fluidRow return
#' @export
headerUI <- function( 
  offset = ifelse(is.null(path), 6, 0), color = "black",
  path = NULL, category, specie = NULL, id, ...){

  if(!is.null(path)){
    
    path <- match.arg(
      arg = path, 
      choices = c("home", "specie", "category", "plot"),
      several.ok = TRUE
    )
    
    pathElements <- c(
      if("home" %in% path)
        list(column(
          width = 1, 
          actionLink(inputId = NS(id, "pathHome"), label = "Home")
        )),
      if("specie" %in% path){
        pathSpecie <- if(!is.null(specie)){
          specie
        }else{
          textOutput(outputId = NS(id, "pathSpecie"))
        }
        list(
          column(width = 1, "/"), 
          column(width = 2, pathSpecie)
        )
      },
      if("category" %in% path)
        list(
          column(width = 1, "/"), 
          column(width = 2, category)
        ),
      if("plot" %in% path)
        list(
          column(width = 1, "/"), 
          column(width = 2, textOutput(outputId = NS(id, "pathPlot")))
         )
    )
    extra <- column(width = 9, do.call(fluidRow, pathElements))
    
  }else extra <- NULL

    cssColor <- paste0("color:", color)
    fluidRow(
      ..., 
      extra,
      column(
        width = 1, offset = offset,
          tags$p(
            tags$a(
              id = "contact", 
              href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", 
              target="_blank", "Contact",
              style = cssColor
            )
          )
       ),
       column(width = 1, 
         shiny::actionLink(
          inputId = "WBE", 
          label = "WBE", 
          onclick = "window.open('https://wbe.inbo.be', '_self')",
          style = cssColor
         )
      ),
       column(width = 1, versionUI(id = "public"), style = cssColor)
    )

}