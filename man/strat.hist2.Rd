% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch2-fn.R
\name{strat.hist2}
\alias{strat.hist2}
\title{Stratified Histograms with a Data Frame}
\usage{
strat.hist2(df, cdep, cfac, spec, br, col, ws, mfd, mar, prob = FALSE)
}
\arguments{
\item{df}{Data frame of input data.}

\item{cdep}{The column of the dependent variable.}

\item{cfac}{The column of the stratifying factor.}

\item{spec}{Specification limits.}

\item{br}{Break method Default: Sturges.}

\item{col}{Vector of colors for the histograms.}

\item{ws}{Graphic window size vector.}

\item{mfd}{Multi-frame dimension vector.}

\item{mar}{Margin vector of the plot.}

\item{prob}{Logical value for density Default: FALSE.}
}
\value{
None.
}
\description{
To draw stratified histograms using a data frame.
}
\examples{
strat.hist2(iris, 1, 5)
strat.hist2(iris, 1, 5, prob=T)
strat.hist2(iris, 1, 5, spec=c(4,7), prob=T)
for (k in 1:4) strat.hist2(iris, k, 5)
}
\keyword{Ch2.}
\keyword{Descriptive}
\keyword{Statistics}
