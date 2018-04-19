\name{processCTD_moored}
\alias{processCTD_moored}

\title{Process moored CTD time series}
\description{
Takes SBE19, SBE19plus, or SBE19plusV2 CNV file for a moored diurnal suite time series and converts to a CSV file.
}
\usage{
processCTD_moored(file, tz.in = "UTC", tz.out = "UTC", write.csv = TRUE, SN3029 = FALSE)
}

\arguments{
  \item{file}{CNV file to process}
  \item{tz.in}{Time zone of raw data, default is \code{UTC}}
  \item{tz.out}{Time zone to output data, default is \code{UTC}}
  \item{write.csv}{Specify \code{FALSE} to suppress CSV output}
  \item{SN3029}{Specify \code{TRUE} if CTD time series is from the SBE19 (serial number 3029)}
}
\details{Obtain list of time zones using \code{\link[base]{OlsonNames}}}
\value{
\item{ctd.ts}{Time, pressure, depth, temperature, conductivity, and salinity time series data}
\item{seafet.ts}{Temperature and salinity formatted for SeaFET pH correction}
}
