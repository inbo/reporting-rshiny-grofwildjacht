# Project: grofWild_git
# 
# Author: wverlinden
###############################################################################


#' Function to generate table for wegdensiteit (F03_1)
#' 
#' @param data data.frame with row for each location of interest;
#' columns are measured background variables
#' @return data.frame
#' 
#' @author wverlinden
#' @export 
tableBackground <- function(data, regionLevel, locaties){
  
  # Subset
  data <- data[, c("regio", "weg_dens_km")]
  
  # Convert from percentages
  data[,2] <- round(data[,2]*100, 1)
  
  # Set column names
  colnames(data) <- c("Studiegebied", "Wegdensiteit")
  
  return(data)
  
  
}