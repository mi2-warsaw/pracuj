% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_offers.R
\name{get_offers}
\alias{get_offers}
\title{Importing offers table from a database}
\usage{
get_offers(dbname = 'pracuj', user = 'reader',
  password = 'qux94874', host = 'services.mini.pw.edu.pl',
  sorted_by_id = TRUE, windows = .Platform$OS.type == 'windows')
}
\arguments{
\item{dbname}{name of database; default: 'pracuj'}

\item{user}{name of user; default: 'reader'}

\item{password}{password of database; default: 'qux94874'}

\item{host}{name of host; default: 'services.mini.pw.edu.pl'}

\item{sorted_by_id}{information if table should be sorted by id; default: TRUE}

\item{windows}{information of used operation system;
default: .Platform$OS.type == 'windows'}

\item{description}{should the column description be returned as well?}
}
\value{
data frame
}
\description{
Function \code{get_offers} imports offers table from a database.
}
\examples{
\dontrun{
offers <- get_offers()
dim(votes)
}
}
\author{
Przemyslaw Biecek
}

