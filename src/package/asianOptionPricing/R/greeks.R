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
## Keywords: greeks, delta, gamma
## Created: 18.04.2008


greeks <- function(S.min, S.max, h, K, r, sigma, T, l, reps, seed=3166125,
                   pseudo=TRUE, european=FALSE, cluster=NULL)
{
  prices <- price.option.S(S.min, S.max, h, K, r, sigma, T, l, reps, seed, pseudo,
                           cluster=cluster)

  # calculate the greeks
  greeks.vals <- greeks.delta(prices, h)
  greeks.vals <- greeks.gamma(greeks.vals, h)

  csv.write.data(greeks.vals, "greeks.dat")

  # plot the graphs
  plot.delta.share(data=greeks.vals, K=K, european=european)
  plot.gamma.share(data=greeks.vals, K=K, european=european)

  return(greeks.vals)
}


greeks.gamma <- function(prices, h)
{
  gamma <- transform(prices,
                     G = rep(-1,length(prices$T)),
                     Gup = rep(-1,length(prices$T)),
                     Glow = rep(-1,length(prices$T)))

  for (i in 3:(length(gamma$T)-2)) {
    gamma$G[i] <- first.derivative(gamma$D[i-1], gamma$D[i+1], h)
    gamma$Gup[i] <- first.derivative(gamma$Dup[i-1], gamma$Dup[i+1], h)
    gamma$Glow[i] <- first.derivative(gamma$Dlow[i-1], gamma$Dlow[i+1], h)
  }

  return(gamma)
}


greeks.delta <- function(prices, h)
{
  delta <- transform(prices,
                     D = rep(-1,length(prices$T)),
                     Dup = rep(-1,length(prices$T)),
                     Dlow = rep(-1,length(prices$T)))

  for (i in 2:(length(delta$T)-1)) {
    delta$D[i] <- first.derivative(delta$C[i-1], delta$C[i+1], h)
    delta$Dup[i] <- first.derivative(delta$Cup[i-1], delta$Cup[i+1], h)
    delta$Dlow[i] <- first.derivative(delta$Clow[i-1], delta$Clow[i+1], h)
  }

  return(delta)
}
