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
## Keywords: european options, delta
## Created: 01.05.2008


european.d1 <- function(S0, K, r, sigma, T, t0)
{
  a <- (base::log(S0/K) + r * (T - t0)) / (sigma * sqrt(T - t0))
  b <- (0.5 * sigma^2 * (T - t0)) / (sigma * sqrt(T - t0))

  return(a + b)
}


european.greeks.delta <- function(S0, K, r, sigma, T, t0)
{
  return(pnorm(european.d1(S0, K, r, sigma, T, t0)))
}


european.greeks.gamma <- function(S0, K, r, sigma, T, t0)
{
  gamma <- 1 / sqrt(2 * pi) * exp(-0.5 * european.d1(S0, K, r, sigma, T, t0)^2) * 1 / (S0 * sigma * sqrt(T - t0))

  return(gamma)
}
