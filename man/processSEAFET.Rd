\name{processSEAFET}
\alias{processSEAFET}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Process SeaFET data}
\description{Converts raw SeaFET output to CSV with date, pH, and temperature}
\usage{
processSEAFET(path, tz.in = "UTC", tz.out = "UTC", csv = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{SeaFET CSV file}
  \item{tz.in}{Time zone of raw data, default is \code{UTC}}
  \item{tz.out}{Time zone to output data, default is \code{UTC}}
  \item{csv}{Specify \code{FALSE} to suppress CSV output}
}
\details{External pH is used as processed pH value.}
\value{}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
