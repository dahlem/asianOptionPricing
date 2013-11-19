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
## Keywords: indicator function, difference schemes, forward, central
## Created: 18.04.2008


Ind <- function(x)
{
  as.numeric(x >= 0)
}


first.derivative <- function(f1, f2, h, diff="central")
{
  if (diff == "central") return(central.diff(f1, f2, h))
  else if (diff == "forward") return(forward.diff(f1, f2, h))
  else stop(paste(diff, "is not a supported finite difference scheme!"))
}


central.diff <- function(f1, f2, h)
{
  f <- (f2 - f1) / (2 * h)
}


forward.diff <- function(f1, f2, h)
{
  f <- (f2 - f1) / h
}
