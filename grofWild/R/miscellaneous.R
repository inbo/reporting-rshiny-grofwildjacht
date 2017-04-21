#' format the sessionInfo output for markdown
#' (sort packages for each slot)
#' @param order, string, either 'alphabetically' or 'original', 
#' depending if the strings in each slot (e.g. the packages in 'attached base packages') should be
#' sorted alphabetically or if no sorting should be done
#' @return no returned value, the reformatted output of sessionInfo is printed in the current console
#' @importFrom utils capture.output packageDescription packageVersion
#' @author Laure Cougnaud
#' @export
printSessionInfoMarkdown <- function(order = c("alphabetically", "original")){
	
	order <- match.arg(order)
	
	# get ourput of sessionInfo
	sessionInfo <- capture.output(print(sessionInfo()))
	idxEmpty <- which(sessionInfo == "")
	sessionInfo <- sessionInfo[!(1:length(sessionInfo)) %in% idxEmpty]
	
	# idx of elements to paste intop one string
	idxToPaste <- which(grepl("\\[[[:digit:]]{1,}\\]", sessionInfo))
	idxSep <- c(0, which(diff(idxToPaste) != 1), length(idxToPaste))
	# idx of elements to paste
	idxToPasteSplit <- sapply(1:(length(idxSep)-1), function(i) idxToPaste[(idxSep[i]+1):(idxSep[i+1])])
	# paste the elements with ', '
	elPaste <- sapply(idxToPasteSplit, function(i){
				res <- gsub("^ *|\\[[[:digit:]]{1,}\\]| *$", "", sessionInfo[i])
				res2 <- c(sapply(res, function(x){
									res1 <- strsplit(x, split = " ")[[1]]; res1[res1!=""]#paste(res1[res1!=""], collapse = ", ")
								}))
				res2Vector <- unlist(res2)
				paste(
						switch(order, 'alphabetically' = sort(res2Vector), 'original' = res2Vector), 
						collapse = ", ")
			})
	
	# idx of elements to keep from sessionInfo
	idxKept <- which(!(1:length(sessionInfo)) %in% idxToPaste) 
	
	# create the final output
	idxAddedInit <- c(which(diff(idxKept) > 1), length(idxKept)) + 1
	# idx of pasted elements
	idxAdded <- idxAddedInit + 0:(length(idxAddedInit)-1)
	resFinal <- rep("", length(idxKept) + length(idxAdded))
	resFinal[idxAdded] <- elPaste
	# idx of elements kept from the sessionInfo
	resFinal[resFinal == ""] <- sessionInfo[idxKept]
	
	# add list in markdown
	idxList <- idxAdded-1
	resFinal[idxList] <- paste0("* ", resFinal[idxAdded-1], "\n")
	idxNotList <- !(1:length(resFinal)) %in% idxList
	resFinal[idxNotList] <- paste0(resFinal[idxNotList], "\n\n")
	
	# print the result into the console
	cat(resFinal)
	
}
