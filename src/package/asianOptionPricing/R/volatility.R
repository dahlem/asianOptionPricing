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
## Keywords: volatility
## Created: 29.04.2008


volatility.bracket <- function(S0, K, r, T, l, reps, seed=3166125, pseudo=TRUE)
{
  # volatility bracket 0 < sigma_annual < 1
  option.payoff <- price.option.sigma(S0=S0, sigma.min=0, sigma.max=1, sigma.step=0.01,
                                      K=K, r=r, T=T, l=l, reps=reps,
                                      seed=seed, pseudo=pseudo)

  # plot the results
  csv.write.data(option.payoff, "priceOptionSigma100.dat")
  plot.option.sigma(data=option.payoff, file="priceOptionSigma100.eps")

  # volatility bracket 0 < sigma_annual < 4
  option.payoff <- price.option.sigma(S0=S0, sigma.min=0, sigma.max=4, sigma.step=0.01,
                                      K=K, r=r, T=T, l=l, reps=reps,
                                      seed=seed, pseudo=pseudo)

  # plot the results
  csv.write.data(option.payoff, "priceOptionSigma400.dat")
  plot.option.sigma(data=option.payoff, mx=TRUE, file="priceOptionSigma400.eps")
}
