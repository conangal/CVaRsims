% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CVaR.SA fun.R
\name{cvar.sim}
\alias{cvar.sim}
\title{CVaR simulations on a standalone machine}
\usage{
cvar.sim(port_list, alpha = c(0.99, 0.995, 0.999), N = 1000)
}
\arguments{
\item{port_list}{List containing: dataframe of the portfolio, covariance matrix, RSq vector}

\item{alpha}{confidence levels needed for Value-at-Risk and Expected Shortfall}

\item{N}{number of iterations required}
}
\value{
prints CVaR and ES values, generates a histogram
}
\description{
Function to estimate the CVaR and Expected Shortfall for a specified portfolio to given confidence levels using a specified number of iterations
}
\examples{
cvar.sim(cvar.port.gen(100), c(0.975,0.99,0.995), N=1000)
}
\keyword{cvar}
\keyword{portfolio}
\keyword{standalone}
