\name{plotDS}
\alias{plotDS}
\title{
Plot diurnal suite instrument data
}
\description{
Plots ADCP, CTD, and SeaFET time series.
}
\usage{
plotDS(speed, direction, pressure, ctd, seafet, puc = NULL,  read.csv = TRUE,
plot.tz = "UTC", time.step = "4 hour", temp.range = NULL, sal.range = NULL, pH.range = NULL)
}

\arguments{
  \item{speed}{ADCP current speed, output of \code{processADCP}.}
  \item{direction}{ADCP current direction, output of \code{processADCP}.}
  \item{pressure}{ADCP pressure, output of \code{processADCP}.}
  \item{ctd}{Moored CTD time series, output of \code{processCTD_moored}.}
  \item{seafet}{eaFET pH time series, output of \code{processSEAFET}.}
  \item{puc}{PUC water samples, output of \code{processPUC}. Default is \code{NULL}.}
  \item{read.csv}{If \code{TRUE}, read data from CSV files. If \code{FALSE}, read data frames from R workspace. Default is \code{TRUE}.}
  \item{plot.tz}{Time zone to plot. Default is \code{UTC}.}
  \item{time.step}{Time step to plot. Default is "4 hour".}
  \item{temp.range}{Range of plotted temperature data, specify as \code{c(min,max)}. Default is \code{NULL}.}
  \item{sal.range}{Range of plotted salinity data, specify as \code{c(min,max)}. Default is \code{NULL}.}
  \item{pH.range}{Range of plotted pH data, specify as \code{c(min,max)}. Default is \code{NULL}.}
}
