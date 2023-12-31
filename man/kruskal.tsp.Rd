% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch15-fn.R
\name{kruskal.tsp}
\alias{kruskal.tsp}
\title{Kruskal-Wallis Test}
\usage{
kruskal.tsp(x, y, dig = 4, ws = "n")
}
\arguments{
\item{x}{Response data.}

\item{y}{Data of factor levels.}

\item{dig}{Number of decimal places, Default: 4.}

\item{ws}{Graphic window size, Default: "n".}
}
\value{
None.
}
\description{
Kruskal-Wallis Test with a Plot
}
\examples{
x <- c(4.6,10.6,8.0,25.0,7.1, 2.9,10.1,3.2,3.8,6.6, 
       6.8,9.4,26.5,12.8,8.3, 3.4,3.9,6.0,8.6,5.0)
y <- rep(1:4, each=5)
kruskal.tsp(x, y, ws=c(7,4))
}
