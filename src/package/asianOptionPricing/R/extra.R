## Copyright (C) 2008 Dominik Dahlem <Dominik.Dahlem@gmail.com>
##
## This file is free software; as a special exception the author gives
## unlimited permission to copy and/or distribute it, with or without
## modifications, as long as this notice is preserved.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
## implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

## Author: Dominik Dahlem <Dominik.Dahlem@gmail.com>
## Maintainer: Dominik Dahlem <Dominik.Dahlem@gmail.com>
## Keywords: monte carlo pricing
## Created: 18.04.2008


# run MC pricing a number of times
share.average.mc <- function(S0, r, sigma, T, l, reps, seed=4711, pseudo=TRUE, init=TRUE)
{
  priceDev <- rep(0, T+1)
  Savgs <- rep(0, reps)

  # reset the random number generator to ensure consistent results
  if (init) {
    random.init(seed=seed)
  }

  # support for scrambled sobol sequences as well as pseudo normal random numbers
  if (pseudo) {
    rnds <- matrix(rnorm(T * reps), nrow=T, ncol=reps)
  } else {
    rnds <- rnorm.sobol(T, reps, TRUE, 3, seed)
  }

  # the zero-th element is S0
  priceDev[1] <- S0

  # repeat the mc reps times then take the average
  for (i in 1:reps) {

    # generate the path from 0 to T
    for (t in 2:(T+1)) {
      priceDev[t] <- price.dev(priceDev[t-1], r, sigma, rnds[(t-1),i])
    }

    # calculate the average asian option share price
    Savgs[i] <- mean(priceDev[(T-l+2):(T+1)])
  }

  return(Savgs)
}


# run MC pricing a number of times
share.price.evolve.mc <- function(S0, r, sigma, T, reps, seed=4711, pseudo=TRUE, init=TRUE)
{
  priceDev <- matrix(nrow=T+1, ncol=reps)

  # reset the random number generator to ensure consistent results
  if (init) {
    random.init(seed=seed)
  }

  # support for scrambled sobol sequences as well as pseudo normal random numbers
  if (pseudo) {
    rnds <- matrix(rnorm(T * reps), nrow=T, ncol=reps)
  } else {
    rnds <- rnorm.sobol(T, reps, TRUE, 3, seed)
  }

  # the zero-th element is S0
  priceDev[1,] <- S0

  # repeat the mc reps times then take the average
  for (i in 1:reps) {

    # generate the path from 0 to T
    for (t in 2:(T+1)) {
      priceDev[t,i] <- price.dev(priceDev[t-1,i], r, sigma, rnds[(t-1),i])
    }
  }
  stdDev <- base::apply(priceDev, 1, sd)

  # calc the mean price evolution + standard deviation
  return(data.frame(S=base::apply(priceDev, 1, mean),
                    se=stdDev/sqrt(reps)))
}
