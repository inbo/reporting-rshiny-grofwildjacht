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
                    "Verkeersongeluk zonder letsel" 	= "GNPERSLTSL",
                    "Verkeersongeluk met letsel" 		= "PERSLTSL",
                    "Verkeersongeluk onbekend" 					= "ONBEKEND",
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


#' Transform vector ready to be in title
#' 
#' Separates elements of vectors with a comma; the final element of the vector is seperated with 'en'
#' 
#' @param vector character vector with elements to be collapsed together into one string
#' @return string Elements of the vector are separated by comma in the string, 
#' ultimate and penultimate element are separated by 'en'. 
#' 
#' @author Eva Adriaensen
#' @export
vectorToTitleString <- function(vector) {
  
  sub(",\\s+([^,]+)$", " en \\1", toString(vector))

}

#' List Dutch names to replace English names for exoten
#' 
#' Will not return NA, but rather the original English name in case
#' no match could be found.
#' 
#' @param x character, what to transform
#' @param type character, defines how to transform short to full names
#' @param rev boolean, whether to switch names and values
#' @return named character vector, names are the original values
#' 
#' @author eadriaensen
#' @export
exoten_dutchNames <- function(x, type = c("regio")) {
  
  type <- match.arg(type)
  
  new <- switch(type,
      regio = c(
          "België" 	    = "Belgium",
          "Brussels Hoofdstedelijk Gewest" 	= "Brussels-Capital Region",
          "Vlaanderen" 	= "Flemish Region",
          "Wallonië"	= "Walloon Region"
      )
  )

  toReturn <- names(new)
  names(toReturn) <- new
    

  
  
  result <- toReturn[match(x, names(toReturn))]
  
  ## in case there was no match (i.e. because new schade code(s))
  ## keep the raw name anyway instead of NA
  if(any(is.na(result))){
    
    naPosition <- which(is.na(result))
    result[naPosition] <- x[naPosition]
    names(result)[naPosition] <- x[naPosition]
  }
  
  return(result)
  
}
