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
#' @param draagvlakData list with draagvlak survey results, each list element
#' is a data.frame; object as returned by \code{\link{loadDraagvlakData}}
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
#' @param regionLevels numeric vector, index of region levels of which to 
#' show the regions
#' @param regionLevelSelected character vector, name of the regionLevel to be 
#' automatically selected
#' @param allRegionsSelected boolean, whether to automatically select all 
#' options from the region selectizeInput
#' @param id character, unique identified for the Shiny module
#' @param specie reactive with specie as character
#' @param category character vector with category/ies,
#' (e.g.'afschot' or 'schade')
#' @param subcategory reactive with subcategory of interest
#' @param subcategories character vector with subcategories,
#' as returned by \code{\link{getSubcategoryOutput}}
#' @param output character vector of length 1 with single output (i.e. table or plot),
#' as returned by \code{\link{getOutputSpecie}} (e.g. 'trendYearRegionUI')
#' @param outputs character vector with outputs (i.e. table or plot),
#' as returned by \code{\link{getOutputSpecie}} (e.g. 'trendYearRegionUI')
#' @param plot reactive with selected plot (or table)
#' @param plotDetails character vector, detail plots to be shown below the map;
#' should be subset of \code{c("flanders", "region", "biotoop")}
#' @param regionChoices named character vector, choices 
#' for the region levels
#' @param doHide boolean, whether to initially hide the plot; default TRUE
#' @param filterVariable boolean, whether to show filter option for variable
#' @param schade_code reactive with selected schade code choices
#' @param schade_gewas reactive with selected schade gewas choices
#' @param schade_voertuig reactive with selected schade voertuig choices
#' @param summarizeBy character, how to summarize counts over groups
#' @param interval character, data shown in intervals
#' @param showDataSource character vector, for which variables to show choices 
#' of data source levels. 
#' @param showType, boolean, whether to select a select input field with type
#' @name reportingGrofwild-common-args
NULL
