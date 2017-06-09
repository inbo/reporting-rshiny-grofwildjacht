#' get path of report available in the package
#' @return string with path of report
#' @author Laure Cougnaud and Kirsten Van Hoorde
#' @export
getPathReport <- function(){
	
	basePathReport <- system.file("report", package = "grofWild")
	pathReport <- dir(basePathReport , pattern = "grofWild_results.Rmd", full.names = TRUE)
	
	return(pathReport)
	
}

#' get path of css file
#' @return string with path of css file
#' @author Laure Cougnaud
#' @export
getPathCss <- function(){
	
	basePath <- system.file("report", package = "grofWild")
	pathFile <- dir(basePath, pattern = "custom.css", full.names = TRUE)
	
	return(pathFile)
	
}

#' get path of file contained in the 'figure' folder of the report
#' @param figureName string name of the figure file
#' ('graph[1-3].png')
#' @return string with path of figure
#' @author Laure Cougnaud
#' @export
getPathFigure <- function(figureName){
	
	basePath <- system.file("report/figure", package = "grofWild")
	pathFile <- dir(basePath, pattern = figureName, full.names = TRUE)
	
	return(pathFile)
	
}
