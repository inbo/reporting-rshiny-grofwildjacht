% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percentageRealisedShotAnimals.R
\name{percentageRealisedShotAnimals}
\alias{percentageRealisedShotAnimals}
\title{Create interactive plot to represent yearly percentage killed and non-killed animals}
\usage{
percentageRealisedShotAnimals(
  data,
  assignedData,
  jaartallen = NULL,
  type = NULL,
  regio = "",
  summarizeBy = c("count", "percent"),
  width = NULL,
  height = NULL
)
}
\arguments{
\item{data}{data.frame with raw data for summary table}

\item{assignedData}{data.frame with summary data on the number of assigned
animals that can be shot}

\item{jaartallen}{integer vector, defines the year(s) considered for the reference period
in the plot; if NULL no selection on year(s) is made}

\item{type}{animal type, used to filter \code{data} and \code{openingstijdenData} ('type' column)
If NULL (by default) or 'all', the data is not filtered.}

\item{regio}{character vector, names of the selected regions in \code{data}
to be shown in the plot title}

\item{summarizeBy}{character, whether to summarize data in terms of counts or percentages}

\item{width}{plot width (optional)}

\item{height}{plot height (optional)}
}
\value{
list with:
\itemize{
\item{'plot': }{plotly object, for a given species the realised percentage
killed animals out of assigned animals for the year \code{jaar}}
\item{'data': }{data displayed in the plot, as data.frame with:
\itemize{
\item{'jaar': }{the year}
\item{'aantal': }{the number of animals that were shot or not shot}
\item{'totaal': }{the number of assigned animals for the given year}
\item{'percent': }{percentage calculated as aantal/totaal*100}
\item{'class': }{character, whether the numbers refer to shot (gerealiseerd)
or non-shot (niet-gerealiseerd) animals}
}
}
}
}
\description{
Visualization of results obtained by tableProvince with categorie = typePercent
}
\author{
mvarewyck
}
