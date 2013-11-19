\name{csv.write.data}
\alias{csv.write.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Write data into a comma-separated file }
\description{
  Write the given dataframe into a comma separated file.
}
\usage{
csv.write.data(data, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the dataframe to be written }
  \item{file}{ the file to be written into }
}
\details{
  Internally, the function will write the data into an evaluation
  directory which encodes the current time of the session in order to
  avoid data being overwritten.
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ csv }% __ONLY ONE__ keyword per line
