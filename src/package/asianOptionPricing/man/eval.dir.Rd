\name{eval.dir}
\alias{eval.dir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create the evaluation directory }
\description{
  This function creates the evaluation directory where the given file
  should be put into.
}
\usage{
eval.dir(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ the filename to be placed into the evaluation directory }
}
\details{
  In the evaluation directory a directory with the timestamp of the
  current session will be created. This avoids to some degree data being overwritten.
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ io }% __ONLY ONE__ keyword per line
