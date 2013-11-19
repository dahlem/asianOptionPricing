\name{merge.result}
\alias{merge.result}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Merge data frames }
\description{
  This function merges data frames with the same data structure. Within
  the context of this package, \code{lapply} returns a list of results
  with each having a particular data frame. \code{merge.result} will
  take this result and merge it into a single dataframe containing all
  partial results.
}
\usage{
merge.result(result)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{result}{ A list of data frames returned from a function such as \code{lapply}. }
}
\value{
  Returns a dataframe with all partial results merged into one.
}
\author{ Dominik Dhlem }

\examples{
## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data processing }
\keyword{ merge data }% __ONLY ONE__ keyword per line
