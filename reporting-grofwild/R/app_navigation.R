#' Create query string for a specific page and selected items
#' @param selection reactive values with selected items
#' @param page string with current page
#' @return string with hash of current selection, e.g.
#' '#ree/beheer'
#' @author mvarewyck, lcougnaud
#' @export
createQueryString <- function(selection, page) {
  
  if (page == "Home")
    return("")
  
  iLevel <- match(page, selection)
  
  if(is.na(iLevel))
    return("")
  
  stringElements <- selection[seq_len(iLevel)]
  
  string <- if(length(stringElements) > 0){
    paste0("#", paste(stringElements, collapse = "/"))
  }else ""

  return(string)
  
}



#' Modify the query string with selected species
#' @param query character, current query string
#' @param specie character, selected species by the user
#' @return character, updated \code{query} with selected species replaced
#' 
#' @author mvarewyck
#' @export
modifyQueryString <- function(query, specie) {
  
  if (specie == "")
    specie <- "Diersoort"
  
  currentSelection <- strsplit(query, split = "/")[[1]]
  currentSelection[1] <- paste0("#", specie)
  
  paste(currentSelection, collapse = "/")
  
}