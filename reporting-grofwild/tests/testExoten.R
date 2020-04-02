# Test plots and summaries for Exoten
# 
# Author: mvarewyck, eadriaensen
###############################################################################

##
## Load exoten data
##
exotenData <- loadExotenData(type = "indicators")

##
## Explore data
##
colnames(exotenData)
sapply(exotenData, class)        
sapply(exotenData[, -17], function(x) if (is.character(x)) table(x) else summary(x))
sapply(exotenData, function(x) sum(is.na(x)))
#          first_observed           last_observed                 kingdom 
#                    6403                    6403                       0 
#                  phylum                   class                   order 
#                       2                      11                       4 
#                  family                locality              locationId 
#                       4                       0                       0 
#            native_range degree_of_establishment          pathway_level1 
#                     270                   10060                     751 
#          pathway_level2                  marine              freshwater 
#                    2144                     111                     111 
#             terrestrial                  source 
#                     111                     111 


##
## Define user choices
##
exoten_time <- min(exotenData$first_observed, na.rm = TRUE):max(exotenData$first_observed, na.rm = TRUE)

exoten_regionChoices <- unique(exotenData$locality)
exoten_region <- exoten_regionChoices[2:3]

exoten_doeChoices <- unique(exotenData$degree_of_establishment[!is.na(exotenData$degree_of_establishment)])
exoten_doe <- exoten_doeChoices[1:length(exoten_doeChoices)]

## taxonomy
## kingdom -> phylum -> class -> order -> family
## TODO: deal with NAs

#kingdom
exoten_kingdomChoices <- unique(exotenData$kingdom)
exoten_kingdom <- exoten_kingdomChoices[1] #[1:length(exoten_kingdomChoices)]

#phylum
exoten_phylumChoices <- unique(exotenData$phylum[exotenData$kingdom %in% exoten_kingdom])
exoten_phylum <- exoten_phylumChoices[1:length(exoten_phylumChoices)] 

#class
exoten_classChoices <-  unique(exotenData$class[exotenData$kingdom %in% exoten_kingdom & 
                                                exotenData$phylum %in% exoten_phylum])
exoten_class <- exoten_classChoices[1:length(exoten_classChoices)]

#order
exoten_orderChoices <- unique(exotenData$order[exotenData$kingdom %in% exoten_kingdom & 
                                               exotenData$phylum %in% exoten_phylum &
                                               exotenData$class %in% exoten_class])
exoten_order <- exoten_orderChoices[1:length(exoten_orderChoices)]

#family
exoten_familyChoices <- unique(exotenData$family[exotenData$kingdom %in% exoten_kingdom & 
                                                exotenData$phylum %in% exoten_phylum &
                                                exotenData$class %in% exoten_class &
                                                exotenData$order %in% exoten_order])
exoten_family <- exoten_familyChoices[1:length(exoten_familyChoices)]

## pathways
## pathway_level1 > pathway_level2
## TODO: deal with NAs

# pathway_level1
exoten_pwLevelOneChoices <- unique(exotenData$pathway_level1)
exoten_pwLevelOne <- exoten_pwLevelOneChoices[3:7] #[1:length(exoten_pwLevelOneChoices)]

# pathway_level2
exoten_pwLevelTwoChoices <- unique(exotenData$pathway_level2[exotenData$pathway_level1 %in% exoten_pwLevelOne])
exoten_pwLevelTwo <- exoten_pwLevelTwoChoices[3:7] #[1:length(exoten_pwLevelTwoChoices)]

##
## Create data upon user choices
##
toRetain <- exotenData$first_observed %in% exoten_time &
            exotenData$locality%in% exoten_region &
            exotenData$kingdom %in% exoten_kingdom &
            exotenData$phylum %in% exoten_phylum &
            exotenData$class %in% exoten_class &
            exotenData$order %in% exoten_order &
            exotenData$family %in% exoten_family &
            exotenData$pathway_level1 %in% exoten_pwLevelOne &
            exotenData$pathway_level2 %in% exoten_pwLevelTwo
            

exoten_data <- exotenData[toRetain,]


## PLOT 1
## Grafiek: aantal geïntroduceerde uitheemse soorten per jaar
##

countIntroductionYearExoten(data = exoten_data)

## PLOT 2
## Grafiek: aantal geïntroduceerde uitheemse soorten per jaar
##


