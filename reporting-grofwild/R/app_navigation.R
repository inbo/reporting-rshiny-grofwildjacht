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