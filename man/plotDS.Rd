\name{plotDS}
\alias{plotDS}
\title{
Plot diurnal suite instrument data
}
\description{
Plots ADCP, CTD, and SeaFET time series.
}
\usage{
plotDS(speed.csv, direction.csv, pressure.csv, ctd.csv, seafet.csv, puc.csv = NULL,
plot.tz = "UTC", time.step = "4 hour", temp.range = NULL, sal.range = NULL, pH.range = NULL)
}

\arguments{
  \item{speed.csv}{CSV file of ADCP current speed, output of \code{processADCP}.}
  \item{direction.csv}{CSV file of ADCP current direction, output of \code{processADCP}.}
  \item{pressure.csv}{CSV file of ADCP pressure, output of \code{processADCP}.}
  \item{ctd.csv}{CSV file of moored CTD time series, output of \code{processCTD_moored}.}
  \item{seafet.csv}{CSV file of SeaFET pH time series, output of \code{processSEAFET}.}
  \item{puc.csv}{CSV file of PUC water samples, output of \code{processPUC}. Default is \code{NULL}.}
  \item{plot.tz}{Time zone to plot. Default is \code{UTC}.}
  \item{time.step}{Time step to plot. Default is "4 hour".}
  \item{temp.range}{Range of plotted temperature data, specify as \code{c(min,max)}. Default is \code{NULL}.}
  \item{sal.range}{Range of plotted salinity data, specify as \code{c(min,max)}. Default is \code{NULL}.}
  \item{pH.range}{Range of plotted pH data, specify as \code{c(min,max)}. Default is \code{NULL}.}
}
