% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch8-fn.R
\name{cont.spdf}
\alias{cont.spdf}
\title{Plot PDF of Continuous Random Variables}
\usage{
cont.spdf(dist, para, para2, ws = c(7, 4), xp, sep = FALSE, ...)
}
\arguments{
\item{dist}{Distribution name ("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f").}

\item{para}{First parameter vector of the distribution.}

\item{para2}{Second parameter vector (except "exp", "t", "chisq").}

\item{ws}{Grapic window size, Default: c(7,4).}

\item{xp}{Location vector for vertical lines.}

\item{sep}{Logical: plot each PDF separately? Default: FALSE.}

\item{...}{Other graphic parameters.}
}
\value{
None.
}
\description{
Plot the PDF of Multiple Continuous Random Variables
}
\examples{
# Four normal PDFs
mu <- c(0, 0, 2, 2)
sig <- c(1, 2, 1, 2)
cont.spdf("norm", mu, sig, xp=mu, xlim=c(-7,7))
# 12 normal PDFs
mu <- rep(1:4, 3); sig <- rep(1:3, each=4)
cont.spdf("norm", mu, sig, xp=mu, sep=TRUE, xlim=c(-5,10))
# Four F PDFs
cont.spdf("f", 5:8, 8:11, sep=TRUE)
}
