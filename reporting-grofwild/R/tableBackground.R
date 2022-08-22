# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate table for wegdensiteit (F03_1)
#' 
#' @param biotoopData data.frame
#' @param locations character vector, selected regions
#' @param character vector containing names of provinces to take into account
#' 
#' 
#' @return data.frame
#' 
#' @author wverlinden
#' @export 
tableBackground <- function(biotoopData, locaties){
  
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