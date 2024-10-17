#' UI header element
#' @param offset Integer with column offset for the first 
#' header element
#' @param color Character, text color, 'black' by default
#' @param ... 
#' @author lcougnaud
#' @import shiny
#' @inherit shiny::fluidRow return
#' @export
headerUI <- function(..., offset = 6, color = "black"){
    
    cssColor <- paste0("color:", color)
    fluidRow(...,
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