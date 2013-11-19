\name{price.option.sigma}
\alias{price.option.sigma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Asian Call Option Pricing }
\description{
  Price asian call options using monte carlo in an interval for sigma
}
\usage{
price.option.sigma(S0, sigma.min, sigma.max, sigma.step, K, r, T, tavg,
reps, seed = 3166125, pseudo = TRUE, cluster = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sigma.min}{ Minimum sigma }
  \item{sigma.max}{ Maximum sigma }
  \item{sigma.step}{ inter-sigma step size }
  \item{S0}{ share price at $t=0$ }
  \item{K}{ strike price }
  \item{r}{ the risk-free interest rate }
  \item{T}{ time to maturity }
  \item{tavg}{ number of time steps to be used for share price averaging }
  \item{reps}{ number of MC steps }
  \item{seed}{ the random number seed }
  \item{pseudo}{ logical value indicating whether the pseudo random
  number should be chosen or a quasi random number generator (Sobol) }
  \item{cluster}{ parameter which may contain the cluster configuration
  as specified by snow and obtained through the makeCluster routine from
  snow. Default is NULL, which implies that the simulation is run in
  serial execution mode.}
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Asian Option }
\keyword{ Monte Carlo }% __ONLY ONE__ keyword per line
