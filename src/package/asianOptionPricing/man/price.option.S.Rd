\name{price.option.S}
\alias{price.option.S}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Asian Call Option Pricing }
\description{
  Price an asian call option given an interval of S0
}
\usage{
price.option.S(S.min, S.max, S.step, K, r, sigma, T, tavg, reps, seed =
3166125, pseudo = TRUE, cluster=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S.min}{ Minimum S0 }
  \item{S.max}{ Maximum S0 }
  \item{S.step}{ inter-S0 step size }
  \item{K}{ strike price }
  \item{r}{ the risk-free interest rate }
  \item{sigma}{ the underlying volatility }
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
\keyword{ Monte Carlo }
\keyword{ Asian Option }% __ONLY ONE__ keyword per line
