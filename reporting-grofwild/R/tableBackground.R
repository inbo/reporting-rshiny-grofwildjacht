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
  data <- data[, c("regio", "weg_dens_km", "perimeter_bos_m", "densiteit_bos", "gem_opp_bos_km2")]
  
  # Convert from percentages
  data$weg_dens_km <- round(data$weg_dens_km, 3)
  data$perimeter_bos_m <- round(data$perimeter_bos_m) 
  data$gem_opp_bos_km2 <- round(data$gem_opp_bos_km2)
  
  # Set column names
  colnames(data) <- c("Studiegebied", "Wegdensiteit (km/km\U00B2)", 
    "Bos perimeter (km/km\U00B2)", "Bos densiteit (#/km\U00B2)", "Bos oppervlakte (km\U00B2)")
  
  return(data)
  
  
}