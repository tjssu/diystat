% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch2-fn.R
\name{freq.table}
\alias{freq.table}
\title{Create a Frequency Table}
\usage{
freq.table(x, breaks, dig = 4, ws = "n", ...)
}
\arguments{
\item{x}{Data vector (or matrix).}

\item{breaks}{Breaks (or number) of intervals.}

\item{dig}{The number of decimal places. Default: 4.}

\item{ws}{Graphic window size, Default: "n".}

\item{...}{Other graphic parameters.}
}
\value{
xtab Frequency Table
}
\description{
To create a frequency table for a numeric vector (or matrix) x.
}
\examples{
set.seed(1234)
freq.table(rnorm(100))
freq.table(rnorm(100), ws=c(7,5), col=7)
}
\keyword{Ch2}
\keyword{Descriptive}
\keyword{Statistics}
