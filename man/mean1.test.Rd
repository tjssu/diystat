% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch10-fn.R
\name{mean1.test}
\alias{mean1.test}
\title{Hypothesis Test and CI for a Mean}
\usage{
mean1.test(
  x,
  mu0,
  alt = "two",
  alp = 0.05,
  sig,
  ss,
  n,
  dig = 4,
  ws = c(7, 4),
  ...
)
}
\arguments{
\item{x}{Sample mean, or sample data.}

\item{mu0}{Population mean under H0 (if missing, just find the CI).}

\item{alt}{Type of the alternative hypothesis ("gr", "le", "two"), Default: "two".}

\item{alp}{Level of significance, Default: 0.05.}

\item{sig}{Population standard deviation (only when it is known).}

\item{ss}{Sample standard deviation (necessary only if no data are given).}

\item{n}{Sample size (necessary only if no data are given).}

\item{dig}{Number of decimal places, Default: 4.}

\item{ws}{Graphic window size, Default: c(7,4).}

\item{...}{Other graphic parameters.}
}
\value{
Summary Statistics.
}
\description{
Hypothesis Test for a Single Population Mean.
}
\examples{
# 95\%-CI, known sigma
mean1.test(199.5, sig=5, n=50)
# 95\%-CI, unknown sigma
mean1.test(199.5, ss=5, n=50)

# H0: mu>12.5, known sigma
mean1.test(x=12.64, mu0=12.5, alt="gr", sig=0.5, n=40)
# H0: mu>12.5, unknown sigma
mean1.test(x=12.64, mu0=12.5, alt="gr", ss=0.5, n=40)

set.seed(1234)
x <- rnorm(40, 12.64, 0.5)
# H0: mu != 12.5, unknown sigma
mean1.test(x, mu0=12.5)
# H0: mu < 12.5, unknown sigma
mean1.test(x, mu0=12.5, alt="le")

}
