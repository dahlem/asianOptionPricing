\name{calc.option.prices}
\alias{calc.option.prices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Call Option Pricing }
\description{
  Calculate the asian call prices given the average payoff generated
  from the monte carlo simulation. This function calculates the
  confidence interval given the monte carlo standard error.
}
\usage{
calc.option.prices(prices, T, t, r, reps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prices}{ Dataframe with payoffStdDev, payffAve C, Clow, and Cup fields }
  \item{T}{ time to maturity }
  \item{t}{ current time }
  \item{r}{ risk-free interest rate }
  \item{reps}{ number of monte carlo steps }
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Option Pricing }% __ONLY ONE__ keyword per line
