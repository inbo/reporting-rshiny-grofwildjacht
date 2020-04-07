# Test plots and summaries for Exoten
# 
# Author: mvarewyck, eadriaensen
###############################################################################

library(trias)

##
## Load exoten data
##
exotenData <- loadExotenData(type = "indicators")

##
## Explore data
##
colnames(exotenData)
sapply(exotenData, class)        
sapply(exotenData, function(x) if (is.character(x)) table(x) else summary(x))
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
exoten_region <- exoten_regionChoices[1]

exoten_doeChoices <- unique(exotenData$degree_of_establishment[!is.na(exotenData$degree_of_establishment)])
exoten_doe <- exoten_doeChoices[1:length(exoten_doeChoices)]

## TODO: write test/warning in case of new habitat columns.
## these are currently manually defined
exoten_habitatChoices <- colnames(exotenData)[colnames(exotenData) %in% c("marine", "freshwater", "terrestrial")]
exoten_habitat <- exoten_habitatChoices[1:length(exoten_habitatChoices)] 

## taxonomy
## kingdom -> phylum -> class -> order -> family

#kingdom
exoten_kingdomChoices <- unique(exotenData$kingdom[!is.na(exotenData$kingdom)])
exoten_kingdom <- exoten_kingdomChoices[1] #[1:length(exoten_kingdomChoices)]

#phylum
exoten_phylumChoices <- na.omit(unique(exotenData$phylum[exotenData$kingdom %in% exoten_kingdom]))
exoten_phylum <- exoten_phylumChoices[1:length(exoten_phylumChoices)] 

#class
exoten_classChoices <-  na.omit(unique(exotenData$class[exotenData$kingdom %in% exoten_kingdom & 
                                                exotenData$phylum %in% exoten_phylum]))
exoten_class <- exoten_classChoices[1:length(exoten_classChoices)]

#order
exoten_orderChoices <- na.omit(unique(exotenData$order[exotenData$kingdom %in% exoten_kingdom & 
                                               exotenData$phylum %in% exoten_phylum &
                                               exotenData$class %in% exoten_class]))
exoten_order <- exoten_orderChoices[1:length(exoten_orderChoices)]

#family
exoten_familyChoices <- na.omit(unique(exotenData$family[exotenData$kingdom %in% exoten_kingdom & 
                                                exotenData$phylum %in% exoten_phylum &
                                                exotenData$class %in% exoten_class &
                                                exotenData$order %in% exoten_order]))
exoten_family <- exoten_familyChoices[1:length(exoten_familyChoices)]

## pathways
## pathway_level1 > pathway_level2

# pathway_level1
exoten_pwLevelOneChoices <- unique(exotenData$pathway_level1[!is.na(exotenData$pathway_level1)])
exoten_pwLevelOne <- exoten_pwLevelOneChoices[3:7] #[1:length(exoten_pwLevelOneChoices)]

# pathway_level2
exoten_pwLevelTwoChoices <- na.omit(unique(exotenData$pathway_level2[exotenData$pathway_level1 %in% exoten_pwLevelOne]))
exoten_pwLevelTwo <- exoten_pwLevelTwoChoices[3:7] #[1:length(exoten_pwLevelTwoChoices)]

##
## Create data upon user choices
##
toRetain <- exotenData$first_observed %in% exoten_time &
            exotenData$locality%in% exoten_region &
            exotenData$degree_of_establishment %in% exoten_doe &
            ##habitat
            apply(exotenData[, .SD, .SDcols = which(colnames(exotenData) %in% exoten_habitat)], 1, function(x) any(x, na.rm = TRUE))          
            ## kingdom
            exotenData$kingdom %in% exoten_kingdom &
            # kingdom - dependent
            exotenData$phylum %in% exoten_phylum &
            exotenData$class %in% exoten_class &
            exotenData$order %in% exoten_order &
            exotenData$family %in% exoten_family &
            ## pathway
            exotenData$pathway_level1 %in% exoten_pwLevelOne &
            # pathway - dependent
            exotenData$pathway_level2 %in% exoten_pwLevelTwo
            

exoten_data <- exotenData[toRetain,]


## PLOT 1
## Grafiek: Aantal geïntroduceerde uitheemse soorten per jaar
##

countIntroductionYear(data = exoten_data)

## PLOT 2
## Grafiek: Cumulatief aantal geïntroduceerde uitheemse soorten per jaar
##

cumulativeIntroductionYear(data = exoten_data)

## PLOT 3
## Grafiek: Aantal geïntroduceerde uitheemse soorten per pathway
##

