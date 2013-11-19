\name{asianOptionPricing-package}
\alias{asianOptionPricing-package}
\alias{asianOptionPricing}
\docType{package}
\title{
Asian Option Pricing and the Greeks
}
\description{
  This package provides routines to simulate the Asian option price and
  its greeks (delta and gamma) using Monte Carlo
  simulation. Additionally, volatility brackets can be simulated to see
  the effect of an increasing annual volatility on the option price
  behaviour. The package supports parallel execution (using snow) and
  serial execution modes. If a valid cluster parameter (obtained through
  makeCluster from snow) is passed into price.option.S and
  price.option.sigma routines, the execution mode switches to
  parallel. This, however, implies that the parallel execution is
  properly setup with snow. Since snow is an abstraction layer on top of
  MPI, PVM, and sockets any of those apart from sockets are supported by
  this package.
}
\details{
\tabular{ll}{
Package: \tab asianOptionPricing\cr
Version: \tab 0.3\cr
Date: \tab 2008-05-02\cr
Depends: \tab R (>= 2.6), fOptions, gplots\cr
Suggests: \tab MASS\cr
License: \tab GPL-2\cr
URL: \tab http://www.r-project.org\cr
Built: \tab R 2.7.0; ; 2008-05-08 12:41:15; unix\cr
}

Index:
\preformatted{
Ind                     Indicator function
calc.option.prices      Call Option Pricing
call.option.discount    Discounted Option Price
call.payoff             Payoff of a call option
central.diff            Central Difference
csv.write.data          Write data into a comma-separated file
equal.space             Equal-spaced sequences
european.d1             d1 function for european option pricing
european.greeks.delta   Delta of a European Option
european.greeks.gamma   The Gamma for a European Option
eval.dir                Create the evaluation directory
first.derivative        Differentiation
forward.diff            Forward Difference
merge.result            Merge data frames
plot.option.sigma       Plot share/option characteristics
price.dev               Price Evolution
price.option.S          Asian Call Option Pricing
price.option.sigma      Asian Call Option Pricing
share.price.mc          Asian Monte Carlo Call Option Pricing
}
}
\author{
Dominik Dahlem <Dominik.Dahlem@gmail.com>

Maintainer: Dominik Dahlem <Dominik.Dahlem@gmail.com>
}
\keyword{ Asian Options }
\keyword{ Derivative Pricing }
\keyword{ Monte Carlo }
\keyword{ Quasi Monte Carlo }
\examples{

## serial execution
S0 <- 12
K <- 14
S.min <- 0
S.max <- 2 * K
S.step <- 0.1
r <- 0.0002
sigma <- 0.013
sigma.min <- 0
sigma.step <- 0.05
T <- 100
l <- 22
reps <- 2000
seed <- 123574903
pseudoRnd <- TRUE
euro <- TRUE

# simulate the greeks and plot the option price vs. share price
g <- greeks(S.min=S.min, S.max=S.max, h=S.step,
            K=K, r=r, sigma=sigma, T=T, l=l, reps=reps, european=euro, pseudo=pseudoRnd,
            cluster=cl)
plot.option.share(g)


## parallel execution
cl <- NULL
nodes <- read.table("NODEFILE")
  
# make the cluster
cl <- makeCluster(length(nodes$V1)), type="MPI")

# load the asianOptionPricing library on each node
clusterEvalQ(cl, library(asianOptionPricing)) 

# volatility bracket 0 < sigma_annual < 6
option.payoff <- price.option.sigma(S0=S0, sigma.min=sigma.min, sigma.max=6,
 sigma.step=sigma.step, K=K, r=r, T=T, l=l, reps=reps,
 seed=seed, pseudo=pseudoRnd, cluster=cl)
 
# write the data into a file and plot the volatility bracket
csv.write.data(option.payoff, "priceOptionSigma600.dat")
plot.option.sigma(data=option.payoff, mx=TRUE, file="priceOptionSigma600.eps")

# stop the cluster again
stopCluster(cl)
}
