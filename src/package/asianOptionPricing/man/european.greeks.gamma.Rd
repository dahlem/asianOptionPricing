\name{european.greeks.gamma}
\alias{european.greeks.gamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The Gamma for a European Option }
\description{
  Calculates the gamma of a european option.
}
\usage{
european.greeks.gamma(S0, K, r, sigma, T, t0)
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
\keyword{ Gamma }
\keyword{ European Option }% __ONLY ONE__ keyword per line
