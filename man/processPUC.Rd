\name{processPUC}
\alias{processPUC}

\title{Pair CTD data and PUC samples
}
\description{
Pairs PUC water samples with moored CTD temperature, salinity, and pressure data averaged over a 45 minute window.
}
\usage{
processPUC(puc.file, ctd.file, tz.in = "UTC", tz.out = "UTC", write.csv = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{puc.file}{CSV file of PUC sample metadata exported from CTD database. Must have a "DateTimeLocal" column that specifies start date and time of sample collection.
}
  \item{ctd.file}{CSV file of paired CTD time series}
  \item{tz.in}{Time zone of PUC deployment data, default is \code{UTC}}
  \item{tz.out}{Time zone of output data, default is \code{UTC}}
  \item{write.csv}{Specify \code{FALSE} to suppress CSV output}
}
\details{Obtain list of time zones using \code{\link[base]{OlsonNames}}}

\value{
\item{puc}{Dataframe of PUC sample data with temperature, salinity, and pressure values}
}
