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
## Keywords: plotting greeks, delta, gamma, share price, option payoff
## Created: 18.04.2008


plot.option.sigma <- function(data, ps=TRUE, mx=FALSE, file="option_sigma.eps", lposx="center", lposy="bottom")
{
  eq <- equal.space(min(data$Clow), max(data$Cup), length(data$C))
  
  if (length(eq) <= 1) {
    warning("This plot can not be generated because the simulated option prices are 0.")
  } else {
    if (ps) {
      postscript(eval.dir(file), onefile=FALSE, width=5.417, height=4.062, paper="special")
    }

    # transform back to annual volatility
    data$sigma <- data$sigma / sqrt(1/252)

    plot(data$sigma, eq,
         type="n", xlab="", ylab="")

    axis(1); axis(2)
    box()
    grid()

    title(xlab=expression(sigma), ylab="Option Price (C)")

    # plot the confidence interval
    polygon(c(data$sigma, rev(data$sigma)), c(data$Clow, rev(data$Cup)),
            col="lightblue", border=0)
    lines(data$sigma, data$C)

    # print the maximum lines
    if (mx) {
      # find the sigma where the option price is maximum
      mxOption <- data[data$C == max(data$C),]

      # horizontal line
      lines(c(0,mxOption$sigma), c(mxOption$C, mxOption$C), col="red", lty=2)
      text(mxOption$sigma/2, mxOption$C, bquote(C == .(base::round(mxOption$C, digits=2))), col = "red", adj = c(0, -.1))
      
      # vertical line
      lines(c(mxOption$sigma, mxOption$sigma), c(0,mxOption$C), col="red", lty=2)
      text(mxOption$sigma, mxOption$C/2, bquote(sigma == .(base::round(mxOption$sigma, digits=2))),
           col="red", adj=c(.1, .1), pos=2)
    }
    
    gplots::smartlegend(x=lposx, y=lposy, inset=0,
                        legend = c("Option Price","95% CI"),
                        fill=c("black","lightblue"),
                        bg = "white")
    
    if (ps) {
      dev.off()
    }
  }
}


plot.option.share <- function(data, ps=TRUE, file="option_share.eps")
{
  if (ps) {
    postscript(eval.dir(file), onefile=FALSE, width=5.417, height=4.062, paper="special")
  }

  plot(data$S0, data$C, type="n", xlab="", ylab="")
  axis(1); axis(2)
  box()
  grid()

  polygon(c(data$S0, rev(data$S0)), c(data$Clow, rev(data$Cup)),
          col="lightblue", border=0)
  lines(data$S0, data$C)
  title(xlab="Share Price (S)", ylab="Option Price (C)")

  gplots::smartlegend(x="left", y= "top", inset=0,
                      legend = c("Option Price","95% CI"),
                      fill=c("black","lightblue"),
                      bg = "white")
  
  if (ps) {
    dev.off()
  }
}


plot.delta.share <- function(data, K, ps=TRUE, file="delta_share.eps", european=FALSE)
{
  # clean the data, i.e., use only inner points
  data <- data[data$D != -1,]

  if (ps) {
    postscript(eval.dir(file), onefile=FALSE, width=5.417, height=4.062, paper="special")
  }

  plot(data$S0, data$D, type="n", xlab="", ylab="")
  axis(1); axis(2)
  box()
  grid()

  polygon(c(data$S0, rev(data$S0)), c(data$Dlow, rev(data$Dup)),
          col="lightblue", border=0)

  # overlay the analytical delta for the european option
  if (european) {
    gplots::smartlegend(x="left", y= "top", inset=0,
                        legend = c(expression(Delta),"95% CI", "Strike Price", expression(Delta[European])),
                        fill=c("black", "lightblue", "blue", "red"),
                        bg = "white")
    
    lines(data$S0, european.greeks.delta(data$S0, K, data$r[1], data$sigma[1], data$T[1], 0),
          col="red")
  } else {
    gplots::smartlegend(x="left", y= "top", inset=0,
                        legend = c(expression(Delta),"95% CI", "Strike Price"),
                        fill=c("black", "lightblue", "blue"),
                        bg = "white")
  }

  lines(data$S0, data$D)

  # mark the strike price
  abline(v=K, col="blue", lty=11)
  mtext("K", at = K, side = 1, line = 1, col = "blue", padj = 2)

  title(xlab="Share Price (S)", ylab=expression(Delta))

  if (ps) {
    dev.off()
  }
}


plot.gamma.share <- function(data, K, ps=TRUE, file="gamma_share.eps", european=FALSE)
{
  # clean the data, i.e., use only inner points
  data <- data[data$G != -1,]

  if (ps) {
    postscript(eval.dir(file), onefile=FALSE, width=5.417, height=4.062, paper="special")
  }

  plot(data$S0, data$G, type="n", xlab="", ylab="")
  axis(1); axis(2)
  box()
  grid()

  # mark the strike price
  abline(v=K, col="blue", lty=11)
  mtext("K", at = K, side = 1, line = 1, col = "blue", padj = 2)

  polygon(c(data$S0, rev(data$S0)), c(data$Glow, rev(data$Gup)),
          col="lightblue", border=0)

  # overlay the analytical gamma for the european option
  if (european) {
    gplots::smartlegend(x="left", y= "top", inset=0,
                        legend = c(expression(Gamma),"95% CI", "Strike Price", expression(Gamma[European])),
                        fill=c("black", "lightblue", "blue", "red"),
                        bg = "white")
    
    lines(data$S0, european.greeks.gamma(data$S0, K, data$r[1], data$sigma[1], data$T[1], 0),
          col="red")
  } else {
    gplots::smartlegend(x="left", y= "top", inset=0,
                        legend = c(expression(Gamma),"95% CI", "Strike Price"),
                        fill=c("black", "lightblue", "blue"),
                        bg = "white")
  }

  
  lines(data$S0, data$G)
  title(xlab="Share Price (S)", ylab=expression(Gamma))

  if (ps) {
    dev.off()
  }
}


plot.share.evo <- function(data, ps=TRUE, file="share_evo.eps")
{
  high <- data$S + qnorm(0.975) * data$se
  low <- data$S + qnorm(0.025) * data$se

  if (ps) {
    postscript(eval.dir(file), onefile=FALSE, width=5.417, height=4.062, paper="special")
  }

  plot(equal.space(min(low), max(high), length(high)), type="n", xlab="", ylab="")
  axis(1); axis(2)
  box()
  grid()

  polygon(c(1:length(data$S), rev(1:length(data$S))), c(low, rev(high)),
          col="lightblue", border=0)
  lines(data$S)

  gplots::smartlegend(x="left", y= "top", inset=0,
                      legend = c("Share Price Evolution","95% CI"),
                      fill=c("black", "lightblue"),
                      bg = "white")
  
  title(xlab="Time (t)", ylab="Share Price (S)")

  if (ps) {
    dev.off()
  }
}


plot.share.average <- function(data, ps=TRUE, file="hist_Savg.eps")
{
  if (ps) {
    postscript(eval.dir(file), onefile=FALSE, width=5.417, height=4.062, paper="special")
  }

  hist(data, xlab="", ylab="")
  abline(v=mean(data), col="blue")
  mtext("Save", at = mean(data), side = 1, line = 1, col = "blue", padj = 2)
  title(xlab="Average Price")

  if (ps) {
    dev.off()
  }
}
