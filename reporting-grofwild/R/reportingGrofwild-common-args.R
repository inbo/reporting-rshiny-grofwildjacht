#' Common arguments for the functions of the \code{reportingGrofWild}
#' package
#' @param ecoData data.frame, ecological data as loaded by
#' \code{loadRawData(type = "eco")}
#' @param geoData data.frame, geographical data as loaded by
#' \code{loadRawData(type = "geo")}
#' @param openingstijdenData data.frame, openingstijden data as 
#' loaded by \code{loadOpeningstijdenData}
#' @param waarnemingenData data.frame waarnemingen data as loaded
#' by \code{loadRawData(type = "waarnemingen")}
#' @param allSpatialData data.frame, spatial data
#' @param spatialData data.frame, spatial data
#' @param biotoopData data.frame, biotoop data as loaded by
#' \code{loadHabitats}
#' @param schadeData data.frame, schade data as loaded by
#' \code{loadRawData(type = "wildschade")}
#' @param schadeTypes character with type of damage, as 'types' 
#' output from \code{loadMetaSchade}
#' @param schadeCodes character with type of damage, as unlisted 'codes' 
#' output from \code{loadMetaSchade}
#' @param gewasChoices character with type of damage, as 'GEWAS'
#' field in 'codes' output from \code{loadMetaSchade}
#' @param voertuigChoices character with type of damage, as 'VRTG'
#' field in 'codes' output from \code{loadMetaSchade}
#' @param defaultYear integer, current (default) end year 
#' of the selected period
#' @param uiText data.frame, HTML formatted text to be displayed 
#' in the UI - as loaded by 
#' \code{read.csv(file = file.path(dataDir, "uiText.csv"))}
#' @param speciesList list of species, grouped by category,
#' as 'wildsoorten' output from \code{loadMetaSchade}
#' @param species species, character vector with species,
#' as unlisted 'wildsoorten' output from \code{loadMetaSchade}
#' @param regio character vector, names of the selected regions in \code{data}
#' to be shown in the plot title
#' @param id character, unique identified for the Shiny module
#' @param specie reactive with specie as character
#' @param category character vector with category/ies,
#' (e.g.'afschot' or 'schade')
#' @param subcategory reactive with subcategory of interest
#' @param subcategories character vector with subcategories,
#' as returned by \code{\link{getSubcategoryOutput}}
#' @param output character vector with output (i.e. table or plot),
#' as returned by \code{\link{getOutputSpecie}} (e.g. 'trendYearRegionUI')
#' @param plot reactive with selected plot (or table)
#' @param plotDetails character vector, detail plots to be shown below the map;
#' should be subset of \code{c("flanders", "region", "biotoop")}
#' @param regionChoices named character vector, choices 
#' for the region levels
#' @param doHide boolean, whether to initially hide the plot; default TRUE
#' @param filterVariable boolean, whether to show filter option for variable
#' @name reportingGrofwild-common-args
NULL
