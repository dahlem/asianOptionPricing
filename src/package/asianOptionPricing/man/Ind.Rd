\name{Ind}
\alias{Ind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Indicator function }
\description{
  This function implements the indicator function:
  \deqn{X^+ = \left{\begin{array}
      X, if X \geq 0 \\
      0, if X \le 0
      \end{array}
    }
  }
}
\usage{
Ind(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The argument for the indicator function. }
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
## returns 3.14...
Ind(pi)

## returns 0
Ind(-pi)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Indicator }% __ONLY ONE__ keyword per line
