\name{mergedataSTR}
\alias{mergedataSTR}
\title{Add 
Add meta data to STR time series
}
\description{
Add meta data from a lookup table to STR time series
}
\usage{
mergedataSTR(str.file, lookup.file, output.dir)
}
\arguments{
  \item{str.file}{CSV output of \code{processSTR}.
}
\item{lookup.file}{CSV file containing STR meta data. Must include STR serial numbers in "Serial.Number" column.}
\item{output.dir}{Path to desired output directiory.

}
