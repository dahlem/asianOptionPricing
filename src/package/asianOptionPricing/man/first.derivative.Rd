\name{first.derivative}
\alias{first.derivative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Differentiation }
\description{
  Differentiate at a given point using either the central difference or
  the forward difference scheme.
}
\usage{
first.derivative(f1, f2, h, diff = "central")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f1}{ first value for $x1$ where $x1 < x2$ }
  \item{f2}{ second value for $x2$ where $x1 < x2$ }
  \item{h}{ the step size }
  \item{diff}{ character string identifying the difference scheme. }
}
\details{
  Currently supported difference schemes are \dQuote{central} and \dQuote{forward}.
}
\value{
  Returns the derivative at a given point.
}
\author{ Dominik Dahlem }
\seealso{ See Also as \code{\link{diff.forward}}, \code{\link{diff.central}} }
\examples{
##---- Should be DIRECTLY executable !! ----
fd <- first.derivative(1, 3, 1)
fd # 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Calculus }
\keyword{ Differentiation }% __ONLY ONE__ keyword per line
