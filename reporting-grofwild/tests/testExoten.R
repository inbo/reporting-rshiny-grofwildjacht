# Test plots and summaries for Exoten
# 
# Author: mvarewyck, eadriaensen
###############################################################################

# Load exoten data
exotenData <- loadExotenData(type = "indicators")

# Explore data
colnames(exotenData)
sapply(exotenData, class)

## extract necessary columns
exotenData <- exotenData[, c(
                # Period - slider should use first_observed
                "first_observed", "last_observed", 
                # Taxonomy
                "kingdom", "phylum", "class", "order", "family",
                # Locality
                "locality", "locationId", "native_range",
                # Degree establishment
                "degree_of_establishment",
                # Pathway
                "pathway_level1", "pathway_level2",
                # Habitat
                # "habitat",  ## I think it's easier to use the 3 below / habitat is not NA, but "" when missing
                "marine", "freshwater", "terrestrial",
                # Source
                "source"
                )]
        
sapply(exotenData[, -17], function(x) if (is.character(x)) table(x) else summary(x))
sapply(exotenData, function(x) sum(is.na(x)))
#is.na(first_observed) = 6403
#is.na(marine) = 111

## PLOT 1
## Grafiek: aantal geïntroduceerde uitheemse soorten per jaar

countIntroductionYearExoten(data = exotenData)



