% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch12-fn.R
\name{pbgof.test}
\alias{pbgof.test}
\title{Goodness of Fit Test for Poisson or Binomial Distribution}
\usage{
pbgof.test(
  tab,
  par,
  dist = "p",
  mc1,
  mc2,
  alp = 0.05,
  dtab = 2,
  dig = 4,
  ws = c(7, 4)
)
}
\arguments{
\item{tab}{Frequency table.}

\item{par}{Parameters (Poisson: mean, Binomial: c(n,p).)}

\item{dist}{Distribution name ('p' or 'b'), Default: 'p'}

\item{mc1}{Lower category numbers to merge.}

\item{mc2}{Upper category numbers to merge.}

\item{alp}{Level of significance, Default: 0.05.}

\item{dtab}{Number of decimal places for the table, Default: 2.}

\item{dig}{Number of decimal places for other output, Default: 4.}

\item{ws}{Graphic window size, Default: c(7,4).}
}
\value{
list(stat, df, cv, pv, tab)
}
\description{
Goodness of fit test for Poisson or binomial distribution from a frequency table.
}
\examples{
# Goodness-of-fit Test (Poisson/Binomial)
set.seed(1234)
x <- rpois(100,5)
mytab <- table(x)
pbgof.test(mytab, 5)
pbgof.test(mytab)
pbgof.test(mytab, mc1=8:9)

pbgof.test(mytab, c(10, 0.5), dist="b")
pbgof.test(mytab, 10, dist="b")

}
