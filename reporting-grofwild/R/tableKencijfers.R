
# data <- everGeoAll[provincie == "Vlaams Brabant"]
#' kencijfers summary table. The summary table of number of municipalities for two consective years. Filter is applied to the obsevation
#' data. Municipality is retained in the summary if there is any animal shot.
#' @param data The geo data contains aschot and waarnemingendata
#' @param bron data source, it should be one of the ["waarnemingen.be", "afschot", "both"]
#' @param jaar the year for which summary of municipalities' observed and shot animal stats
#' @param treshold threshold for number of animals observed
#' @author Yingjie Zhang
#' @export
#' 

tabelKencijfers <- function(data, 
                            jaar = 2023, 
                            bron = "both",
                            threshold = 0){
  
  releventColumns <- c("afschotjaar", "provincie", "gemeente_afschot_locatie", "dataSource", "aantal")
  stopifnot(releventColumns %in% colnames(data))
  
  
  if(bron == "afschot")   threshold <- 0
  bron <- if(bron == "both") c( "waarnemingen.be", "afschot" ) else bron
  
  yearRange <- range(dataSubset$afschotjaar)
  
  if(jaar == yearRange[1]) dataSubset <- data[afschotjaar = jaar]  else dataSubset <- data[afschotjaar %in% c(jaar, jaar-1)]
  
  dataSubset <- unique(dataSubset[, releventColumns, with = FALSE])
  
  dataSubset <- dataSubset[(dataSource == "afschot" & aantal > 0) | (dataSource == "waarnemingen.be" & aantal > threshold)]
  dataCurrentYear <-   dataSubset[afschotjaar == jaar]
  
  resultTable <- cbind("Totaal aantal gemeenten", nrow(  dataCurrentYear),nrow(  dataCurrentYear) )
  # create current year summary table
  
  if(jaar > yearRange[1]){
    
    dataPreYear <-   dataSubset[afschotjaar == jaar-1]
    
    common <- intersect(dataCurrentYear[[ "gemeente_afschot_locatie"]],dataPreYear[["gemeente_afschot_locatie"]])
    new <- setdiff(dataCurrentYear[[ "gemeente_afschot_locatie"]],dataPreYear[["gemeente_afschot_locatie"]]) |> na.exclude()
    old <-  setdiff(dataPreYear[["gemeente_afschot_locatie"]], dataCurrentYear[[ "gemeente_afschot_locatie"]]) |> na.exclude()
    
    resultTable <- rbind(  resultTable,  if(length(common) != 0) cbind("Dezelfde gemeentes",length(common), common),
                           if(length(new) != 0) cbind("Nieuwe gemeentes", length(   new ), new),
                           if(length(old) != 0) cbind("Niet meer in deze gemeentes", length(old), old)
    )
    
  }else{
    resultTable <- rbind( resultTable, cbind("Dezelfde gemeentes",
                                             length(dataCurrentYear[["gemeente_afschot_locatie"]]), 
                                             dataCurrentYear[["gemeente_afschot_locatie"]]) )
    
    
  }
  
 colnames(resultTable) <- NULL
  
  return( resultTable)
}


#### server

tabelKencijfersServer <- function(id, data, label = "tabel kencijfers"){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    }
}
#### UI