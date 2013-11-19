\name{share.price.mc}
\alias{share.price.mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Asian Monte Carlo Call Option Pricing }
\description{
  Price asian call options using monte carlo simulation.
}
\usage{
share.price.mc(S0, K, r, sigma, T, tavg, reps, seed = 3166125, pseudo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S0}{ the share price at $t=0$ }
  \item{K}{ the strike price of the option }
  \item{r}{ the risk-free interest rate }
  \item{sigma}{ the volatility }
  \item{T}{ time to maturity }
  \item{tavg}{ the number of time steps to be averaged }
  \item{reps}{ the number of monte carlo steps }
  \item{seed}{ the random number seed }
  \item{pseudo}{ logical value indicating whether the pseudo random
  number should be chosen or a quasi random number generator (Sobol) }
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Monte Carlo }
\keyword{ Asian Option }% __ONLY ONE__ keyword per line
