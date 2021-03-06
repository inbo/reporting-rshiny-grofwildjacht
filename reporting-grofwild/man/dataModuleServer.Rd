% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyModules.R
\name{dataModuleServer}
\alias{dataModuleServer}
\title{Display formatted frequency table of data (ui-side)}
\usage{
dataModuleServer(input, output, session, data, variable)
}
\arguments{
\item{input}{shiny input variable for specific namespace}

\item{output}{shiny output variable for specific namespace}

\item{session}{shiny session variable for specific namespace}

\item{data, }{character vector, values for which frequency table should be generated}

\item{variable}{character, name of the variable that is summarized}
}
\value{
ui object (tagList)
}
\description{
Display formatted frequency table of data (ui-side)
}
