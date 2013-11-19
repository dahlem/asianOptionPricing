\name{equal.space}
\alias{equal.space}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Equal-spaced sequences }
\description{
  This function returns an equal-spaced sequence of numbers given the
  minimum, maximum, and the number of points including the maximum/minimum.
}
\usage{
equal.space(min, max, number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min}{ Minimum number of the sequence }
  \item{max}{ Maximum number of the sequence }
  \item{number}{ the number of points including min/max }
}
\details{
  This function can be used to layout a plotting area in case of
  multiple data fields being plotted.
}
\value{
  Returns a sequence including min/max.
}
\author{ Dominik Dahlem }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sequence }% __ONLY ONE__ keyword per line
