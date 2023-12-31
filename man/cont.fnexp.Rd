% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch5-fn.R
\name{cont.fnexp}
\alias{cont.fnexp}
\title{Expected Values of a Function of Continuous Random Variable}
\usage{
cont.fnexp(gx, fx, lo = -Inf, up = Inf, dig = 4, prt = "exp", err = 1e-06)
}
\arguments{
\item{gx}{Function of X for which the expected value will be obtained.}

\item{fx}{Continuous PDF of X.}

\item{lo}{Lower limit of X, Default: -Inf.}

\item{up}{Upper limit of X, Default: Inf.}

\item{dig}{Number of decimal places, Default: 4.}

\item{prt}{Print option: one of c("exp", "var"), Default: "exp".}

\item{err}{Error limit for integration, Default: 1E-06.}
}
\value{
list(Ex=E(X), Vx=Var(X), Dx=D(X))
}
\description{
Calculate E(g(X)) of a Continuous Random Variable X
}
\examples{
fx <- function(x) exp(-2*x)*(x>0)
y <- function(x) 3*x^2 - 3
cont.fnexp(y, fx, 0, Inf, prt="var")

}
