% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch14-fn.R
\name{sreg.pre}
\alias{sreg.pre}
\title{Preliminary Analysis for a Simple Linear Regression}
\usage{
sreg.pre(x, y, data, r0, alt = "two", alp = 0.05, dig = 4, ws = "n", ...)
}
\arguments{
\item{x}{Independent (explanatory) variable (or formula).}

\item{y}{Dependent (response) variable.}

\item{data}{Data frame applied to x and y (or formula).}

\item{r0}{Correlation coefficient value under H0 (for test).}

\item{alt}{Type of alternative, one of ("two", "gr", "le"), Default: 'two'.}

\item{alp}{Level of significance, Default: 0.05.}

\item{dig}{Number of decimal places, Default: 4.}

\item{ws}{Graphic window size, Default: "n".}

\item{...}{Other graphic parameters.}
}
\value{
List of statistics.
}
\description{
Preliminary Analysis via a Scatter Plot for a Simple Linear Regression.
}
\examples{
# First two columns of "iris" data set
sreg.pre(iris[[1]], iris[[2]])
sreg.pre(1, 2, iris)
# Test for correlation
sreg.pre(1, 2, iris, r0=0, ws=c(7,4))
# The 3rd and 4th columns of "iris" data set
sreg.pre(iris[[3]], iris[[4]], ws=c(7,4))
sreg.pre(3, 4, iris, ws=c(7,4))

x <- c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
y <- c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
sreg.pre(x, y)
# Test for correlation
sreg.pre(x, y, r0=0.9, ws=c(7,4))
sreg.pre(y ~ x, r0=0.9, ws=c(7,4))
# Data transformation
(y2 <- y*x/1000)
sreg.pre(x, y2)
# Test for correlation coefficient
sreg.pre(x, y2, r0=0.6, alt="gr", ws=c(7,4))
}
