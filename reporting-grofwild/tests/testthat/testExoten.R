# Test plots and summaries for Exoten
# 
# Author: mvarewyck
###############################################################################

context("Test Exoten")


test_that("Exoten", {
    
    skip(message = "not up to date")
    
    # Read data
    exotenData <- data.table::fread("~/Documents/grofwild/exoten/data_input_checklist_indicators.tsv")
#exotenData <- readr::read_tsv("~/Documents/grofwild/exoten/data_input_checklist_indicators.tsv", na = "", guess_max = 5000)
    
    # Explore data
    colnames(exotenData)
    sapply(exotenData, class)
    exotenData <- exotenData[, c(
        # Period
        "first_observed", "last_observed", 
        # Taxonomy
        "kingdom", "phylum", "class", "order", "family",
        # Locality
        "locality", "native range",
        # Degree establishment
        "degree of establishment",
        # Pathway
        "pathway_level1", "pathway_level2",
        # Habitat
        # "habitat",  ## I think it's easier to use the 3 below
        "marine", "freshwater", "terrestrial",
        # Source
        "source"
      )]
    
    sapply(exotenData[, -13], function(x) if (is.character(x)) table(x) else summary(x))
    sapply(exotenData, function(x) sum(is.na(x)))
    
  })