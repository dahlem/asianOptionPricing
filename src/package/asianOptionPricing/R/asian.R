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
## Keywords: asian option, option pricing, Black-Scholes, sensitivities
## Created: 18.04.2008


price.option.sigma <- function(S0, sigma.min, sigma.max, sigma.step, K, r, T, l,
                               reps, seed=3166125, pseudo=TRUE, cluster=NULL)
{
  # run mc in the range of [sigma.min, sigma.max]
  annual2dailyVol <- sqrt(1/252)

  if (length(cluster) == 0) {
    result <- lapply(seq(sigma.min, sigma.max, sigma.step) * annual2dailyVol,
                     share.price.mc,
                     S0=S0, K=K, r=r, T=T, l=l, reps=reps,
                     seed=seed, pseudo=pseudo)
  } else {
    # execute in a cluster
    result <- clusterApplyLB(cluster, seq(sigma.min, sigma.max, sigma.step) * annual2dailyVol,
                             share.price.mc,
                             S0=S0, K=K, r=r, T=T, l=l, reps=reps,
                             seed=seed, pseudo=pseudo)
  }

  # merge the different results into a single data frame
  prices <- merge.result(result)
  options <- calc.option.prices(prices, T, 0, r, reps)

  return(options)
}


price.option.S <- function(S.min, S.max, S.step, K, r, sigma, T, l,
                           reps, seed=3166125, pseudo=TRUE, cluster=NULL)
{
  # run mc in the range of [S.min, S.max]
  if (length(cluster) == 0) {
    result <- lapply(seq(S.min, S.max, S.step),
                     share.price.mc,
                     K=K, r=r, sigma=sigma, T=T, l=l, reps=reps,
                     seed=seed, pseudo=pseudo)
  } else {
    # execute in a cluster
    result <- clusterApplyLB(cluster, seq(S.min, S.max, S.step),
                             share.price.mc,
                             K=K, r=r, sigma=sigma, T=T, l=l, reps=reps,
                             seed=seed, pseudo=pseudo)
  }

  # merge the different results into a single data frame
  prices <- merge.result(result)
  options <- calc.option.prices(prices, T, 0, r, reps)

  return(options)
}


# run MC pricing a number of times
share.price.mc <- function(S0, K, r, sigma, T, l, reps, seed=3166125, pseudo=TRUE, init=TRUE)
{
  priceDev <- rep(0, T+1)
  payoff <- rep(0, reps)

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
    Savgs <- mean(priceDev[(T-l+2):(T+1)])

    # calculate the payoff at time T
    payoff[i] <- call.payoff(Savgs, K)
  }

  # calculate the average payoff and the standard deviation
  return(data.frame(T = T,
                    payoffAve = mean(payoff),
                    payoffStdDev = sd(payoff),
                    sigma=sigma,
                    K=K,
                    S0=S0,
                    r=r))
}


calc.option.prices <- function(prices, T, t, r, reps)
{
  prices <- transform(prices,
                      C = rep(-1, length(prices$T)),
                      Cup = rep(-1, length(prices$T)),
                      Clow = rep(-1, length(prices$T)))

  # calculate the option value at time 0
  # also calculate the upper/lower bound based on the standard error
  stdErr <- prices$payoffStdDev / sqrt(reps)
  prices$C <- call.option.discount(prices$payoffAve, T, t, r)
  prices$Cup <- call.option.discount(prices$payoffAve + qnorm(0.975) * stdErr, T, t, r)
  prices$Clow <- call.option.discount(prices$payoffAve + qnorm(0.025) * stdErr, T, t, r)

  # make sure the lower option price is not below 0
  prices$Clow[prices$Clow < 0] <- 0

  return(prices)
}


price.dev <- function(Sold, r, sigma, rnd)
{
  return(Sold * exp(price.rate(r, sigma, rnd)))
}


price.rate <- function(r, sigma, rnd)
{
  return(r - 0.5 * sigma^2 + sigma * rnd)
}


call.option.discount <- function(payoff, T, t, r)
{
  return(exp(-r * (T - t)) * payoff)
}


call.payoff <- function(Savg, K)
{
  return((Savg - K) * Ind(Savg - K))
}
