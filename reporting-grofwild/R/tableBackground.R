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
  data <- data[, c("regio", "perimeter_bos_m", "gem_opp_bos_km2", "densiteit_bos", 
      "weg_dens_km", "Area_km2")]
  
  # Convert from percentages
  data$perimeter_bos_m <- round(data$perimeter_bos_m/1000/data$Area_km2, 2) 
  data$gem_opp_bos_km2 <- round(data$gem_opp_bos_km2)
  data$densiteit_bos <- round(data$densiteit_bos/data$Area_km2, 3)
  data$weg_dens_km <- round(data$weg_dens_km, 3)
  data$Area_km2 <- NULL
  
  # Set column names
  colnames(data) <- c("Studiegebied", "Wegdensiteit (km/km\U00B2)", 
    "Bos perimeter (km/km\U00B2)", "Bos densiteit (#/km\U00B2)", "Gemiddelde bos oppervlakte (km\U00B2)")
  
  return(data)
  
  
}