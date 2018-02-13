#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-13-10, 2-28-12                                          #
# PURPOSE: Graph pi_i vs x_i                                        #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Because the curve() function plays an important role in the 
# example, below is a simple use of it

  x11(width = 7, height = 6, pointsize = 12)
  curve(expr = x^2, xlim = c(-1, 2), col = "red",
      main = "Plot of f(x) = x^2", xlab =  "x",
      ylab = "f(x)", panel.first = grid(col = "gray", lty = "dotted"))


#####################################################################
# Logistic regression model plot
      
  x11(width = 7, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure2.1BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mfrow = c(1,2))
  # par(pty="s") #Create square plots

  beta0<-1
  beta1<-0.5
  curve(expr = exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)), xlim = c(-15, 15), col = "black",
      main = expression(pi == frac(e^{1+0.5*x[1]}, 1+e^{1+0.5*x[1]})), xlab =  expression(x[1]),
      ylab = expression(pi))

  beta0<-1
  beta1<--0.5
  curve(expr = exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)), xlim = c(-15, 15), col = "black",
      main = expression(pi == frac(e^{1-0.5*x[1]}, 1+e^{1-0.5*x[1]})), xlab =  expression(x[1]),
      ylab = expression(pi))
  # dev.off()  # Create plot for book

  # See help(plotmath) for more on the expression function and run demo(plotmath) for examples
  # Also, plogis(beta0+beta1*x) could be used instead of exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))


#####################################################################
# Other ways to do the plot

  # Simplest
  beta0<-1
  beta1<-0.5
  curve(expr = exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)), xlim = c(-15, 15))


  # Without curve()
  beta0<-1
  beta1<-0.5
  x<-seq(from = -15, to = 15, by = 0.5)
  pi<-exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))

  x11(width = 7, height = 6, pointsize = 12)
  par(pty="s")
  plot(x = x, y = pi, xlab = "x", ylab = "pi", ylim = c(0,1), 
     type = "l", col = 2, main = "pi = exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))", panel.first = 
     grid(col = "gray", lty = "dotted"))


#####################################################################
# All distributions - holding mean and variance constant

  x11(width = 7, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure2.13color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  par(xaxs = "i")  # Plot only in specified x-axis range (default is "r" which goes 4% more on each side)
  rm(pi)  # I used pi to mean something else than pi = 3.14 above
  # par(pty="m")

  M<-2  # Mean
  V<-3  # ariance
  xlim.val<-c(qnorm(0.001, mean = M, sd = sqrt(V)),  qnorm(0.999, mean = M, sd = sqrt(V)))  # x-axis limits
  rd<-2  # Number of digits for rounding.

  # Logistic
  # Note: If X ~ Logistic(mu, sigma) (mu is location parameter and sigma is scale parameter), then
  #       the mean (M) is M = mu and the variance (V) is V = sigma^2 * pi^2 / 3. This leads to
  #       sigma = sqrt(V) * sqrt(3) / pi. (X-mu)/sigma = beta0 + beta1*X which leads to the beta0 and
  #       beta1 as given below
  beta0<--M*pi/(sqrt(3) * sqrt(V))  # -mu/sigma
  beta1<-pi/(sqrt(3) * sqrt(V))     # 1/sigma
  # curve(expr = plogis(x, location = M, scale = sqrt(V) * sqrt(3) / pi), xlim = xlim.val, col = "red", lwd = 2,
  #  lty = 1, ylim = c(0,1), xlab =  "x", ylab = expression(pi), panel.first = grid(col = "gray", lty = "dotted")) #w/o beta0 and beta1
  curve(expr = plogis(beta0 + beta1*x), xlim = xlim.val, col = "red", lwd = 2,
    lty = 1, ylim = c(0,1), xlab =  "x", ylab = expression(pi), panel.first = grid(col = "gray", lty = "dotted"))
  logistic.beta<-data.frame(name = "logistic", beta0 = round(beta0,rd), beta1 = round(beta1,rd))

  # Normal
  #  (X-mu)/sigma = beta0 + beta1*X which leads to the beta0 and beta1 as given below
  beta0<--M/sqrt(V)  # -mu/sigma
  beta1<-1/sqrt(V)   # 1/sigma
  # curve(expr = pnorm(x, mean = M, sd = sqrt(V)), add = TRUE, col = "blue", lwd = 2, lty = "dashed")  #w/o beta0 and beta1
  # curve(expr = pnorm((x-M)/sqrt(V)), add = TRUE, col = "blue", lwd = 2, lty = "dashed")  #w/o beta0 and beta1
  curve(expr = pnorm(beta0 + beta1*x), add = TRUE, col = "blue", lwd = 2, lty = "dashed")
  normal.beta<-data.frame(name = "probit", beta0 = round(beta0,rd), beta1 = round(beta1,rd))

  # Complementary log-log
  # Note: If X ~ Gumbel(mu, sigma) (mu is location parameter and sigma is scale parameter), then
  #       Y = 1-X has a CDF of F(y) = 1 - exp(-(exp(y+mu-1)/sigma)). We can set (y+mu-1)/sigma equal to
  #       beta0 + beta1*x to find the beta0 and beta1 as shown below. Also, the mean (M) of Y is
  #       M = 1 - mu - euler*sigma, and the variance (V) of Y is V = pi^2 * sigma^2 / 6. Solving for
  #       mu and sigma, we obtain the parameter values as shown below that are written in terms of M and V.
  euler<--digamma(1)  #Euler's constant
  sigma<-sqrt(V) * sqrt(6) / pi
  mu<-1 - M - euler*sigma
  # curve(expr = 1 - exp(-exp((x + mu - 1)/sigma)), add = TRUE, col = "green", lwd = 2, lty = "dotdash") #w/o beta0 and beta1
  beta0<-(mu-1)/sigma
  beta1<-1/sigma
  curve(expr = 1 - exp(-exp(beta0 + beta1*x)), add = TRUE, col = "green", lwd = 2, lty = "dotdash")
  cloglog.beta<-data.frame(name = "cloglog", beta0 = round(beta0,rd), beta1 = round(beta1,rd))

  # Compare beta0 and beta1 among CDFs
  rbind.data.frame(logistic.beta, normal.beta, cloglog.beta)

  # Use in legend
  logistic<-substitute(paste("Logistic: ", beta[0] == beta0, ", ", beta[1] == beta1),
    list(beta0 = as.numeric(logistic.beta[2]), beta1 = as.numeric(logistic.beta[3])))
  probit<-substitute(paste("Probit: ", beta[0] == beta0, ", ", beta[1] == beta1),
    list(beta0 = as.numeric(normal.beta[2]), beta1 = as.numeric(normal.beta[3])))
  cloglog<-substitute(paste("Comp. log-log: ", beta[0] == beta0, ", ", beta[1] == beta1),
    list(beta0 = as.numeric(cloglog.beta[2]), beta1 = as.numeric(cloglog.beta[3])))

  legend(x = -3.25, y = 1, legend = c(as.expression(logistic), as.expression(probit), as.expression(cloglog)), lty = c(1,2,4), lwd = c(2,2,2),
       col = c("red", "blue", "green"), bty = "n", cex = 1, seg.len = 4)
  # dev.off()  # Create plot for book
  # Used http://r.789695.n4.nabble.com/R-legend-plotmath-substitute-problem-td814632.html to help
  #  solve problem with creating a legend - as.expression() is needed.


#####################################################################
# Black-and-white version of plot

  x11(width = 7, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure2.13BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  par(xaxs = "i")  # Plot only in specified x-axis range (default is "r" which goes 4% more on each side)

  beta0<--M*pi/(sqrt(3) * sqrt(V))  # -mu/sigma
  beta1<-pi/(sqrt(3) * sqrt(V))     # 1/sigma
  curve(expr = plogis(beta0 + beta1*x), xlim = xlim.val, col = "black", lwd = 2,
    lty = 1, ylim = c(0,1), xlab =  "x", ylab = expression(pi))

  # Normal
  #  (X-mu)/sigma = beta0 + beta1*X which leads to the beta0 and beta1 as given below
  beta0<--M/sqrt(V)  # -mu/sigma
  beta1<-1/sqrt(V)   # 1/sigma
  curve(expr = pnorm(beta0 + beta1*x), add = TRUE, col = "black", lwd = 2, lty = "dashed")

  # Complementary log-log
  euler<--digamma(1)  # Euler's constant
  sigma<-sqrt(V) * sqrt(6) / pi
  mu<-1 - M - euler*sigma
  beta0<-(mu-1)/sigma
  beta1<-1/sigma
  curve(expr = 1 - exp(-exp(beta0 + beta1*x)), add = TRUE, col = "black", lwd = 2, lty = "dotdash")

  legend(x = -3.25, y = 1, legend = c(as.expression(logistic), as.expression(probit), as.expression(cloglog)), lty = c(1,2,4), lwd = c(2,2,2),
       col = c("black", "black", "black"), bty = "n", cex = 1, seg.len = 4)
  # dev.off()  # Create plot for book


######################################################################
# All distributions  - mean and variance not held constant (unfair comparison)

  x11(width = 10, height = 6, pointsize = 12)
  par(mfrow = c(1,2))
  par(pty="s")
 
  beta0<-1
  beta1<-0.5
 
  curve(expr = plogis(beta0+beta1*x), xlim = c(-15, 15), col = "red", lwd = 2, lty = "solid",
      main = expression(paste(beta[0], " = 1 and ", beta[1]," = 0.5")), xlab =  "x",
      ylab = expression(pi), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = pnorm(beta0+beta1*x, mean=0, sd=1), xlim = c(-15, 15), col = "blue", add = TRUE, lty = 2, lwd = 2)
  curve(expr = 1-exp(-exp(beta0+beta1*x)), xlim = c(-15, 15), col = "green", add = TRUE, lty = 4, lwd = 2)
  # There is a pgumbel(q, loc=0, scale=1, lower.tail = TRUE) function in the evd and VGAM packages
  legend(x = 0, y = 0.4, legend = c("logistic", "probit", "comp. log-log"), lty = c(1,2,4), lwd = c(2,2,2), 
       col = c("red", "blue", "green"), bty = "n", cex = 0.75)


  beta0<-1
  beta1<--0.5

  curve(expr = plogis(beta0+beta1*x), xlim = c(-15, 15), col = "red", lwd = 2, lty = "solid",
      main = expression(paste(beta[0], " = 1 and ", beta[1]," = -0.5")), xlab =  "x",
      ylab = expression(pi), panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = pnorm(beta0+beta1*x, mean=0, sd=1), xlim = c(-15, 15), col = "blue", add = TRUE, lty = 2, lwd = 2)
  curve(expr = 1-exp(-exp(beta0+beta1*x)), from = -15, to = 15, col = "green", add = TRUE, lty = 4, lwd = 2)
  legend(x = -15, y = 0.4, legend = c("logistic", "probit", "comp. log-log"), lty = c(1,2,4), lwd = c(2,2,2), 
       col = c("red", "blue", "green"), bty = "n", cex = 0.75)

#
