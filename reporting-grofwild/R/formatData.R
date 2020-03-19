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
fullNames <- function(x, type = c("wildschade"), rev = FALSE) {
    
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
                    "Geen personen met letsel" 	= "GNPERSLTSL",
                    "Personen met letsel" 		= "PERSLTSL",
                    "Onbekend" 					= "ONBEKEND",
                    "Valwild"					= "VALWILD"
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
