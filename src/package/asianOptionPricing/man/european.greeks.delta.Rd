\name{european.greeks.delta}
\alias{european.greeks.delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Delta of a European Option }
\description{
  Calculate the delta of a european option analytically
}
\usage{
european.greeks.delta(S0, K, r, sigma, T, t0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S0}{ the share price at $t=0$ }
  \item{K}{ the strike price }
  \item{r}{ the risk-less interest rate }
  \item{sigma}{ the volatility }
  \item{T}{ the maturity time }
  \item{t0}{ the current time }
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Delta }
\keyword{ European Option }% __ONLY ONE__ keyword per line
