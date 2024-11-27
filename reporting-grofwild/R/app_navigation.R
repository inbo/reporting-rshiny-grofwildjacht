#' Create query string for a specific page and selected items
#' @param selection reactive values with selected items
#' @param page string with current page
#' @return string with hash of current selection, e.g.
#' '#ree/beheer'
#' @author mvarewyck
#' @export
createQueryString <- function(selection, page) {
  
  if (page == "Home")
    return("")
  
  selectionList <- reactiveValuesToList(selection)
  toKeep <- which(!is.na(match(selectionList, page)))
  string <- paste0("#", paste(selectionList[seq_along(toKeep)], collapse = "/"))
  return(string)
  
}