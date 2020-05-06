# Functions to format the data
# 
# Author: mvarewyck
###############################################################################


#' List user-friendly full names to replace R coding names
#' 
#' Will not return NA, but rather the R coding name in case
#' no match could be found.
#' 
#' @param x character, what to transform
#' @param type character, defines how to transform short to full names
#' @param rev boolean, whether to switch names and values
#' @return character vector
#' 
#' @author mvarewyck
#' @export
fullNames <- function(x, type = c("wildschade", "maanden"), rev = FALSE) {
    
    type <- match.arg(type)
    
    new <- switch(type,
            wildschade = c(
                    # SchadeBasisCode
                    "Gewas" 	= "GEWAS",
                    "Voertuig" 	= "VRTG",
                    "Andere" 	= "ANDERE",
                    # SchadeCode
                    "Woelschade"				= "WLSCHD",
                    "Vraatschade" 				= "VRTSCHD",
                    "Gewas - andere"    = "GEWASANDR", 
                    "Verkeersongeluk zonder letsel" 	= "GNPERSLTSL",
                    "Verkeersongeluk met letsel" 		= "PERSLTSL",
                    "Verkeersongeluk onbekend" 					= "ONBEKEND",
                    "Valwild"					= "VALWILD"
            ),
            maanden = c(
                    "januari" = "January",
                    "februari" = "February",
                    "maart" = "March",
                    "april" = "April",
                    "mei" = "May",
                    "juni" = "June",
                    "juli" = "July",
                    "augustus" = "August",
                    "september" = "September",
                    "oktober" = "October",
                    "november" = "November",
                    "december" = "December" 
                )
    )
    
    
    
    if (rev) {
        toReturn <- names(new)
        names(toReturn) <- new
        
    } else {
        toReturn <- new
    }
    
    
    result <- toReturn[match(x, toReturn)]
    
    ## in case there was no match (i.e. because new schade code(s))
    ## keep the raw name anyway instead of NA
    if(any(is.na(result))){
      
      naPosition <- which(is.na(result))
      result[naPosition] <- x[naPosition]
      names(result)[naPosition] <- x[naPosition]
    }
    
    return(result)
    
}


#' Replace english months detected in vector elements with Dutch month names
#' 
#' Will not return NA, but rather the original content if
#' no match could be found. In case a factor is supplied, 
#' a factor will be returned keeping the original order of the levels, but 
#' with adjusted months. 
#' 
#' @param x character or factor, what to transform
#' @return character or factor vector, depending on what was supplied
#' 
#' @author eadriaensen
#' @export
setMonthsInDutch <- function(x) {
	
  class <- class(x)
  
  if (class == "factor") {
    levels <- levels(x)
  	x <- as.character(x)
  } else if (class != "character") {
  	stop("This function can only work on vectors of class 'factor' or 'character'.")
  }
  
  dictionary <- data.frame("english" = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                           "nederlands" = c("januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus", "september", "oktoboer", "november", "december"),
                           stringsAsFactors = FALSE)
  
  result <- sapply(x, function(element) {
        
    matched <- sapply(dictionary[["english"]], function(englishElement) {
  		    grepl(englishElement, element)
  	    })
    if (!any(matched)) {
      
      return(element)
      message("Warning: the following element could not be translated and is kept as is: ", element ," Please make sure there are no typos in the supplied vector.")
    } else {
      ind <- which(matched)
      gsub(dictionary[ind, "english"], dictionary[ind, "nederlands"], element)
    }
        
  })

  if (class == "factor") {
    
    levelsAdjusted <- sapply(levels, function(element) {
          
          matched <- sapply(dictionary[["english"]], function(englishElement) {
                grepl(englishElement, element)
              })
          if (!any(matched)) {
            
            return(element)
            message("Warning: the following element could not be translated and is kept as is: ", element ," Please make sure there are no typos in the supplied vector.")
          } else {
            ind <- which(matched)
            gsub(dictionary[ind, "english"], dictionary[ind, "nederlands"], element)
          }
          
        })
    
    result <- factor(result, levels = levelsAdjusted)
    
  }
  
  return(result)
  
}