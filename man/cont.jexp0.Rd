% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch5-fn.R
\name{cont.jexp0}
\alias{cont.jexp0}
\title{Expected Values of Two Continuous Random Variables}
\usage{
cont.jexp0(
  FUN,
  lo1 = -Inf,
  up1 = Inf,
  lo2 = -Inf,
  up2 = Inf,
  dig = 4,
  prt = "exp"
)
}
\arguments{
\item{FUN}{Continuous joint PDF.}

\item{lo1}{Lower limit of X, Default: -Inf.}

\item{up1}{Upper limit of X, Default: Inf.}

\item{lo2}{Lower limit of Y, Default: -Inf.}

\item{up2}{Upper limit of Y, Default: Inf.}

\item{dig}{Number of decimal places, Default: 4.}

\item{prt}{Print option: one of c("", "exp", "cov", "cor"), Default: ''.}
}
\value{
list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))
}
\description{
Joint pdf and Expected Values of Two Continuous Random Variables
}
\examples{
pdf <- function(x, y) 0.5*(x+3*y)*(x>0 & x<1)*(y>0 & y<1)
cont.jexp(pdf, prt="cor")

fxy <- function(x,y) 24*x*y*(x>0)*(y>0)*(x+y<1)
cont.jexp(fxy,0,1,0,1, prt="cor")
}
