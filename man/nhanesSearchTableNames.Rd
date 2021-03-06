% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nhanes.R
\name{nhanesSearchTableNames}
\alias{nhanesSearchTableNames}
\title{Search for matching table names}
\usage{
nhanesSearchTableNames(
  pattern = NULL,
  ystart = NULL,
  ystop = NULL,
  includerdc = FALSE,
  nchar = 100,
  details = FALSE
)
}
\arguments{
\item{pattern}{Pattern of table names to match}

\item{ystart}{Four digit year of first survey included in search, where ystart >= 1999.}

\item{ystop}{Four digit year of final survey included in search, where ystop >= ystart.}

\item{includerdc}{If TRUE then RDC only tables are included (default=FALSE).}

\item{nchar}{Truncates the variable description to a max length of nchar.}

\item{details}{If TRUE then complete table information from the comprehensive
data list is returned (default=FALSE).}
}
\value{
A list of table names that match the pattern.
}
\description{
Returns a list of table names that match a specified pattern.
}
\details{
Searches the Doc File field in the NHANES Comprehensive Data List 
(see https://wwwn.cdc.gov/nchs/nhanes/search/DataPage.aspx) for tables
that match a given name pattern. Only a single pattern may be entered.
}
\examples{
\donttest{nhanesSearchTableNames('BMX')}
nhanesSearchTableNames('HPVS', includerdc=TRUE, details=TRUE)
}
