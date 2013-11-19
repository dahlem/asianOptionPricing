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
## Keywords: utilities, merge data frames
## Created: 18.04.2008


merge.result <- function(result)
{
  prices <- result[[1]]
  for (i in 2:length(result)) {
    prices <- base::merge(prices, result[[i]], all=TRUE, sort=FALSE)
  }

  return(prices)
}

equal.space <- function(min, max, number)
{
  return(seq(min, max, (max-min)/(number-1)))
}
