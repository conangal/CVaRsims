% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CVaR.Spark fun.R
\name{cvar.spk}
\alias{cvar.spk}
\title{CVaR simulations with sparklyr}
\usage{
cvar.spk(sc, part = 2, port_list, alpha = c(0.99, 0.995, 0.999), N = 1000)
}
\arguments{
\item{sc}{spark connection}

\item{part}{number of partitions to split the calculations over}

\item{port_list}{List containing: dataframe of the portfolio, covariance matrix, RSq vector}

\item{alpha}{confidence levels needed for Value-at-Risk and Expected Shortfall}

\item{N}{number of iterations required}
}
\value{
prints CVaR and ES values, generates a histogram
}
\description{
Function to estimate the CVaR and Expected Shortfall for a specified portfolio to given confidence levels using a specified number of iterations using sparklyr to achieve parallelised calculations
}
\examples{
cvar.spk(sc, 2, cvar.port.gen(100), c(0.975,0.99,0.995), N=1000)
}
\keyword{cvar}
\keyword{portfolio}
\keyword{standalone}
