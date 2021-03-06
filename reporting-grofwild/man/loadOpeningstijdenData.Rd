% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadData.R
\name{loadOpeningstijdenData}
\alias{loadOpeningstijdenData}
\title{read openingstijden data}
\usage{
loadOpeningstijdenData(
  dataDir = system.file("extdata", package = "reportingGrofwild")
)
}
\arguments{
\item{dataDir}{character vector, defines the path to the data files}
}
\value{
data.frame with columns:
\itemize{
\item{'Soort': }{specie}
\item{'Type': }{specie type}
\item{'Jaar': }{year}
\item{'Startdatum': }{start datum, in the format '\%d/\%m/\%Y'}
\item{'Stopdatum': }{end datum, in the format '\%d/\%m/\%Y'}
}
and attribute 'Date', the date that this data file was created
}
\description{
read openingstijden data
}
