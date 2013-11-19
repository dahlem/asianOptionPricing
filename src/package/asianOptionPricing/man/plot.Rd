\name{plot}
\alias{plot.option.sigma}
\alias{plot.option.share}
\alias{plot.delta.share}
\alias{plot.gamma.share}
\alias{plot.share.evo}
\alias{plot.share.average}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot share/option characteristics }
\description{
  These functions plot different characteristics of shares/options.
}
\usage{
plot.option.sigma(data, ps=TRUE, mx=FALSE, file="option_sigma.eps", lposx="center", lposy="bottom")
plot.option.share(data, ps=TRUE, file="option_share.eps")
plot.delta.share(data, K, ps=TRUE, file="delta_share.eps", europe=FALSE)
plot.gamma.share(data, K, ps=TRUE, file="gamma_share.eps", europe=FALSE)
plot.share.evo(data, ps=TRUE, file="share_evo.eps")
plot.share.average(data, ps=FALSE, file="hist_Savg.eps")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the dataframe to be plotted }
  \item{ps}{ logical value indicating whether the ouput should be
  printed into a postscript file }
  \item{K}{ the strike price }
  \item{file}{ the filename to be printed to }
  \item{europe}{ logical value indicating whether the analytical
  european characteristic should be superimposed }
  \item{mx}{ logical value indicating whether maximum value in the graph
  should be emphasised}
  \item{lposx}{ indicates the x position of the legend, default to center}
  \item{lposy}{ indicates the y position of the legend, default to bottom}
}
\value{
  \code{plot.option.sigma} plots the option price against $\sigma$
  \code{plot.option.share} plots the option price the share price
  \code{plot.delta.share} plots the $\Delta$ sensitivity against the
  share price
  \code{plot.gamma.share} plots the $\Gamma$ sensitivity against the
  share price
  \code{plot.share.evo} plots the share price evolution
  \code{plot.share.average} plots a histogram of the average share price
  given a number of simulation runs
}
\author{ Dominik Dahlem }
\examples{
##---- Should be DIRECTLY executable !! ----
S.min <- 0
S.max <- 28
h <- 0.1
K <- 14
r <- 0.0002
sigma <- 0.013
T <- 100
l <- 22
reps <- 1000

prices <- price.option.S(S.min, S.max, h, K, r, sigma, T, l, reps)

# calculate the greeks
greeks.vals <- greeks.delta(prices, h)
greeks.vals <- greeks.gamma(greeks.vals, h)

# plot the graphs
plot.delta.share(greeks.vals, K)
plot.gamma.share(greeks.vals, K)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plotting }
\keyword{ option sensitivities }
\keyword{ share }% __ONLY ONE__ keyword per line
