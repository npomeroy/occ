\name{trimDS}
\alias{trimDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trim CTD and SeaFET time series
}
\description{This function uses ADCP data to establish the true start and end points of the diel suite and uses these time points to trim remove above-water data from CTD and SeaFET time series.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
trimDS(speed,direction,pressure, ctd, seafet, read.csv = TRUE, write.csv = TRUE, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speed}{ADCP current speed, output of \code{processADCP}.}
  \item{direction}{ADCP current direction, output of \code{processADCP}.}
  \item{pressure}{ADCP pressure, output of \code{processADCP}.}
  \item{ctd}{Moored CTD time series, output of \code{processCTD_moored}.}
  \item{seafet}{SeaFET pH time series, output of \code{processSEAFET}.}
   \item{read.csv}{If \code{TRUE}, read data from CSV files. If \code{FALSE}, read data frames from R workspace. Default is \code{TRUE}.}
    \item{write.csv}{If \code{TRUE}, write trimmed CTD and SeaFET data to CSV files. Default is \code{FALSE}.}
    \item{filename}{Leading filename for output files (e.g. "HA1801_Line_JAR11_DS")}
}
}
\value{
\item{speed}{Depth-binned water column speed though time (in m/s)}
\item{direction}{Depth-binned water column direction though time (0-360 degrees)}
\item{pressure}{Pressure time series}
\item{ctd}{Trimmed CTD time series}
\item{seafet}{Trimmed SeaFET time series}
}

}
