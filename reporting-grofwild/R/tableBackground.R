# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate table for wegdensiteit (F03_1)
#' 
#' @param biotoopData 
#' @param character vector containing names of provinces to take into account
#' 
#' 
#' @return 
#' 
#' @author wverlinden
#' @importFrom DT rbind 
#' @export 

tableBackground <- function(biotoopData, locations){
  
  # Subset data
  toReport <- subset(biotoopData$provinces, regio %in% locaties, select = c("regio", "weg_dens_km"))
  
  # Add Vlaams Gewest
  toReport <- rbind(toReport, biotoopData$flanders[, c("regio", "weg_dens_km")])
  
  # Convert from percentages
  toReport[,2] <- round(toReport[,2]*100, 1)
  
  # Set column names
  colnames(toReport) <- c("Studiegebied", "Wegdensiteit")
  
  return(toReport)
  
  
}