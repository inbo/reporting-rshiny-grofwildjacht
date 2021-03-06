% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyModules.R
\name{plotModuleServer}
\alias{plotModuleServer}
\title{Interactive plot or table (server-side)}
\usage{
plotModuleServer(
  input,
  output,
  session,
  plotFunction,
  data,
  openingstijdenData,
  toekenningsData = NULL,
  categorie = NULL,
  bioindicator = NULL,
  locaties = NULL,
  timeRange = NULL,
  unit = NULL,
  schade = FALSE,
  datatable = FALSE,
  schadeChoices = NULL,
  schadeChoicesVrtg = NULL,
  schadeChoicesGewas = NULL,
  variable = NULL,
  combinatie = NULL,
  schadeTitles = FALSE
)
}
\arguments{
\item{input}{shiny input variable for specific namespace}

\item{output}{shiny output variable for specific namespace}

\item{session}{shiny session variable for specific namespace}

\item{plotFunction}{character, defines the plot function to be called}

\item{data}{reactive data.frame, data for chosen species}

\item{openingstijdenData}{data with openingstijden, optional}

\item{toekenningsData}{data with toekenningen, optional}

\item{categorie}{character, defines which type of table should be made}

\item{bioindicator}{string with column of \code{data}
with bioindicator, either 'onderkaaklengte',
'ontweid_gewicht', or 'aantal_embryos'}

\item{locaties}{character, defines on which locations to filter on;
defined externally for large map}

\item{timeRange}{numeric vector, defines on which year range to filter on;
defined externally for large map}

\item{unit}{character, defines whether absolute or relative frequencies are reported;
defined externally for large map}

\item{schade}{boolean, indicates whether module is used for schadeData; default is FALSE}

\item{datatable}{boolean, indicates whether module should be used to output a datatable object for table; default is FALSE}

\item{schadeChoices}{character, chosen schade types (basisCode) to filter on, optional}

\item{schadeChoicesVrtg}{character, chosen schade types related to "VRTG" to filter on, optional}

\item{schadeChoicesGewas}{character, chosen schade types related to "GEWAS" to filter on, optional}

\item{variable}{character, defines which variable is of interest for the table}

\item{combinatie}{logical, summarised view of selected regions}

\item{schadeTitles}{boolean, indicates whether the function should generate titles for schadeData; default is FALSE}
}
\value{
no return value; plot output object is created
}
\description{
Interactive plot or table (server-side)
}
\author{
mvarewyck
}
