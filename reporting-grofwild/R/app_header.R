#' UI header element
#' @param offset integer with column offset for the first 
#' header element
#' @param color character, text color, 'black' by default
#' @param path character vector with elements to include in the
#' path, among: 'home', 'specie', 'category', 'subcategory', 'plot' 
#' (NULL by default).
#' @param id character, module id/specie
#' @param specie (optional) hard-coded specie in the path (NULL by default)
#' @param ... extra parameters for \code{\link[shiny]{fluidRow}}
#' @author lcougnaud
#' @import shiny
#' @inherit shiny::fluidRow return
#' @export
headerUI <- function( 
  offset = ifelse(is.null(path), 8, 0), color = "black",
  path = NULL, category, specie = NULL, id, ...){

  if(!is.null(path)){
    
    ns <- NS(namespace = id)
    
    path <- match.arg(
      arg = path, 
      choices = c("home", "specie", "category", "subcategory", "plot"),
      several.ok = TRUE
    )
    
    path <- c(
      list(column(width = 0.05, "")),
      if("home" %in% path)
        list(
          column(
            width = 0.5, 
            actionLink(inputId = ns("pathHome"), label = "Home")
          )
        ),
      if("specie" %in% path){
        pathSpecie <- if(!is.null(specie)){
          specie
        }else{
          textOutput(outputId = ns("pathSpecie"))
        }
        list(
          column(width = 0.25, "/"), 
          column(width = 2, pathSpecie)
        )
      },
      if("category" %in% path)
        list(
          column(width = 0.25, "/"), 
          column(width = 1, category)
        ),
      if("subcategory" %in% path)
        list(
          column(width = 0.25, "/"), 
          column(width = 2.5, textOutput(outputId = ns("pathSubcategory")))
        ),
      if("plot" %in% path)
        list(
          column(width = 0.25, "/"), 
          column(width = 2.5, textOutput(outputId = ns("pathPlot")))
         )
    )
    headerLeft <- column(width = 9, do.call(fluidRow, path))
    
  }else headerLeft <- NULL
  
  cssColor <- paste0("color:", color)
  headerRight <- list(
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
    column(width = 0.5, 
      shiny::actionLink(
        inputId = "WBE", 
        label = "WBE", 
        onclick = "window.open('https://wbe.inbo.be', '_self')",
        style = cssColor
      )
    ),
    column(width = 0.5, versionUI(id = "public"), style = cssColor)
  )
  
  header <- fluidRow(..., headerLeft, headerRight)
  
  return(header)

}

#' Extend \code{\link[shiny]{column}} to a decimal width
#' @inheritParams shiny::column
#' @return \code{\link[shiny]{div}}
#' @importFrom shiny column
#' @author lcougnaud
column <- function(width, ...){
  
  if(width %% 1 > 0)
    div(
      class = "col-sm-1", 
      style = paste0("width: ", round(width*8.33333333, 8), "%"), 
      ...
    )
  else
    shiny::column(width = width, ...)
  
}