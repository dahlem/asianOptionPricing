\name{volatility.bracket}
\alias{volatility.bracket}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Option Pricing in a pre-specified volatility brackets }
\description{
  Price asian call option using monte carlo in specified volatility brackets
}
\usage{
volatility.bracket(S0, K, r, T, l, reps, seed = 3166125, pseudo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S0}{ S0 }
  \item{K}{ strike price }
  \item{r}{ the risk-free interest rate }
  \item{sigma}{ the underlying volatility }
  \item{T}{ time to maturity }
  \item{l}{ number of time steps to be used for share price averaging }
  \item{reps}{ number of MC steps }
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
\keyword{ Volatility }
\keyword{ Asian Option }% __ONLY ONE__ keyword per line
