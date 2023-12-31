% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch3-fn.R
\name{bayes.plot}
\alias{bayes.plot}
\title{Prior and Posterior Probabilities}
\usage{
bayes.plot(prior, cond, post, gname, cname = "X", dig = 4, ws = c(7, 4), ...)
}
\arguments{
\item{prior}{Prior probability distribution vector.}

\item{cond}{Conditional distribution vector.}

\item{post}{Posterior probability distribution vector.}

\item{gname}{Group names, Default: A, B, C, ...}

\item{cname}{Conditional event name, Default: X.}

\item{dig}{Number of decimal places, Default: 4.}

\item{ws}{The size of the graphic window, Default: c(7,4).}

\item{...}{Other graphic parameters (col, cex, main).}
}
\value{
None.
}
\description{
Displaying the Prior and the Posterior Probabilities
}
\examples{
prior <- c(0.2, 0.4, 0.3, 0.1)
cond <- c(0.04, 0.02, 0.01, 0.05)
# Direct way
bayes.plot(prior, cond)
tot <- prior*cond
# Using posterior probabilities
post <- tot / sum(tot)
bayes.plot(prior, post=post)
}
