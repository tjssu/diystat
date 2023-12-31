% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch8-fn.R
\name{fpdf.sim}
\alias{fpdf.sim}
\title{Simulation of the F-distribution}
\usage{
fpdf.sim(nu1 = 5, nu2 = 5, N = 10000, ng = 250, seed = 9857, xp = 1:9, dig = 4)
}
\arguments{
\item{nu1}{Numerator degree of freedom, Default: 5}

\item{nu2}{Denominator degree of freedom, Default: 5}

\item{N}{Number of random values, Default: 10000}

\item{ng}{Number of classes in histogram, Default: 250}

\item{seed}{Seed value for random number generator, Default: 9857}

\item{xp}{Vector of x-axis values, Default: 1:9}

\item{dig}{Number of digits below the decimal point, Default: 4}
}
\value{
None.
}
\description{
Simulation of the F-distribution
}
\examples{
fpdf.sim(nu1=8, nu2=5)

fpdf.sim(50, 50, ng=100, xp=1:5/2)
}
