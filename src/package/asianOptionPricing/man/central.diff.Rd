\name{central.diff}
\alias{central.diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Central Difference }
\description{
  Calculates the derivative using the central difference scheme.
}
\usage{
central.diff(f1, f2, h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f1}{ first value for $x1$ where $x1 < x2$ }
  \item{f2}{ second value for $x2$ where $x1 < x2$ }
  \item{h}{ the step size }
}
\value{
  Returns the derivative of the function f at the given point $f'(x)$.
}
\author{ Dominik Dahlem }
\seealso{ See Also as \code{\link{forward.diff}}, \code{\link{first.derivative}} }
\examples{
##---- Should be DIRECTLY executable !! ----
fd <- central.diff(1,3,1)
fd # 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ calculus }
\keyword{ differentiation }% __ONLY ONE__ keyword per line
