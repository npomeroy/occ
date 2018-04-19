\name{processADCP}
\alias{processADCP}

\title{Process ADCP data}

\description{Converts a PRF file collected by a Nortek Aquadopp to speed, direction, and pressure data frames.}

\usage{
processADCP(file, write.csv = TRUE)
}

\arguments{
\item{file}{PRF file to process}
\item{write.csv}{Specify \code{FALSE} to suppress CSV output
}
}

\details{Column names in speed and direction data frames refer to depth bin.
}

\value{
\item{speed}{Depth-binned water column speed though time (in m/s)}
\item{direction}{Depth-binned water column direction though time (0-360 degrees)}
\item{pressure}{Pressure time series}
}
