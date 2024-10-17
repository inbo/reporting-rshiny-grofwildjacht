#' UI header element
#' @param offset column offset for the first header element
#' @param ... 
#' @author lcougnaud
#' @import shiny
#' @inherit shiny::fluidRow return
#' @export
headerUI <- function(offset = 6, ...){
    
    fluidRow(...,
      column(
        width = 1, offset = offset,
          tags$p(
            tags$a(
              id = "contact", 
              href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", 
              target="_blank", "Contact",
              style = "color: white"
            )
          )
       ),
       column(width = 1, 
         shiny::actionLink(
          inputId = "WBE", 
          label = "WBE", 
          onclick = "window.open('https://wbe.inbo.be', '_self')"
         )
      ),
       column(width = 1, versionUI(id = "public"), style = "color: white")
    )

}