#' Create query string for a specific page and selected items
#' @param selection reactive values with selected items
#' @param page string with current page
#' @inheritParams getInfo
#' @return string with hash of current selection, e.g.
#' '#ree/beheer'
#' @author mvarewyck, lcougnaud
#' @export
createQueryString <- function(selection, page, defaults) {
  
  if (page == "Home")
    return("")
  
  iLevel <- match(page, selection)
  
  if (is.na(iLevel))
    iLevel <- which.max(sapply(names(selection), function(iName) selection[[iName]] != defaults[[iName]]))
  
  stringElements <- selection[seq_len(iLevel)]
  
  # Add gbifkey of species
  if ("specie" %in% names(stringElements) && stringElements$specie != defaults[["specie"]]) {
    speciesInfo <- read.csv(file.path(system.file("extdata", package = "reportingGrofwild"), "species-info.csv"))
    stringElements$gbifkey <- speciesInfo[match(stringElements[["specie"]], speciesInfo$species.name), "gbifkey"]
  }
  
  string <- if (length(stringElements) > 0){
      paste0("?", paste0(names(stringElements), "=", stringElements, collapse = "&"))
  } else ""

  return(string)
  
}
