% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trendYearRegion.R
\name{trendYearRegion}
\alias{trendYearRegion}
\title{Create interactive plot for counts per selected communes and years}
\usage{
trendYearRegion(
  data,
  locaties = NULL,
  combinatie = NULL,
  timeRange = NULL,
  unit = c("absolute", "relative"),
  schadeTitles = FALSE,
  width = NULL,
  height = NULL
)
}
\arguments{
\item{data}{data.frame with raw data for plotting}

\item{locaties}{character vector, regions that were selected to plot}

\item{combinatie}{logical, summarised view of selected regions}

\item{timeRange}{numeric vector, time range selected for plot}

\item{unit}{character, whether absolute or relative frequencies (aantal/100ha) 
should be reported}

\item{schadeTitles}{boolean, indicates whether the function should generate titles for schadeData; default is FALSE}

\item{width}{plot width (optional)}

\item{height}{plot height (optional)}
}
\value{
list with:
\itemize{
\item{'plot': }{plotly object, for a given species the observed number 
per year and per selected commune is plotted in a line plot}
\item{'data': }{data displayed in the plot, as data.frame with:
\itemize{
\item{'afschotjaar': }{year at which the animals was shot}
\item{'locatie': }{comune name}
\item{'aantal' or 'aantal/100ha': }{absolute or relative counts of animals}
}
}
}
}
\description{
Create interactive plot for counts per selected communes and years
}
