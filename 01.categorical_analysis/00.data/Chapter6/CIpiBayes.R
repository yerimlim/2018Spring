##########################################################################
# NAME: Chris Bilder                                                     #
# DATE: 5-31-13                                                          #
# PURPOSE: Estimates and C.I.s for pi using Bayes methods                #
## NOTES:                                                                #
##########################################################################

# Basic computations
w <- 4  # Sum(y_i)
n <- 10
alpha <- 0.05

# Bayes estimate
a <- 1
b <- 1
# a <- 0.5 From Jeffreys' prior
# b <- 0.5
pi.hatb <- (w+a)/(n+a+b)
pi.hatb

# MLE
pi.hat <- w/n
pi.hat


# Equal-tail interval
qbeta(p = c(alpha/2, 1-alpha/2), shape1 = w + a, shape2 = n + b - w)

# Jeffreys' prior
qbeta(p = c(alpha/2, 1-alpha/2), shape1 = w + 0.5, shape2 = n + 0.5 - w)

# HPD
library(TeachingDemos)
# The posterior.icdf argument means "posterior inverse CDF"
save.hpd <- hpd(posterior.icdf = qbeta, shape1 = w + a, shape2 = n + b - w, conf = 1-alpha)
save.hpd

# Verify lower and upper limits are at the same p(pi|w) values
dbeta(x = save.hpd, shape1 = w + a, shape2 = n + b - w)

# Verify area between limits is 1-alpha
pbeta(q = save.hpd[2], shape1 = w + a, shape2 = n + b - w) -
 pbeta(q = save.hpd[1], shape1 = w + a, shape2 = n + b - w)

# binom package - gives incorrect value for HPD (version 1.0-5)
library(package = binom)
binom.confint(x = w, n = n, conf.level = 1-alpha, methods = "bayes",
  prior.shape1 = a, prior.shape2 = b)
binom.bayes(x = w, n = n, conf.level = 0.95, type = "highest",
      prior.shape1 = a, prior.shape2 = b)  # Supposed to be HPD
binom.bayes(x = w, n = n, conf.level = 0.95, type = "central",
      prior.shape1 = a, prior.shape2 = b)  # Equal tail



################################################################################
# Calculation details for HPD

 # Find the pi that maximizes p(pi|w) - one HPD limit will be to the left and one will be to the right
 mode <- optimize(f = dbeta, interval = c(0,1), maximum = TRUE, shape1 = w + a, shape2 = n + b - w)
 mode$maximum  # $objective gives height too
 height <- dbeta(x = mode$maximum, shape1 = w + a, shape2 = n + b - w)  # max f(pi|w) value
 height

 # Finds a pi corresponding to a given p(pi|w)
 find.pi <- function(x, shape1, shape2, height) {
  dbeta(x = x, shape1 = shape1, shape2 = shape2) - height
 }

 # Finds the p(pi|w) where the HPD limits occur
 find.p <- function(height, shape1, shape2, mode, conf.level) {

  # Find pi on the x-axis
  lower <- uniroot(f = find.pi, interval = c(0, mode), shape1 = w + a, shape2 = n + b - w,
   height = height)
  upper <- uniroot(f = find.pi, interval = c(mode, 1), shape1 = w + a, shape2 = n + b - w,
   height = height)

  # Check if area is equal to the confidence level (the value here will be 0 if it is)
  pbeta(q = upper$root, shape1 = w + a, shape2 = n + b - w) -
  pbeta(q = lower$root, shape1 = w + a, shape2 = n + b - w) - conf.level
 }
 
 # Save the p(pi|w) where HPD limits occur
 save.res <- uniroot(f = find.p, interval = c(0, height), shape1 = w + a, shape2 = n + b - w,
  mode = mode$maximum, conf.level = 0.95)
 # Find the corresponding value of pi given p(pi|w)
 lower <- uniroot(f = find.pi, interval = c(0, mode$maximum), shape1 = w + a, shape2 = n + b - w,
  height = save.res$root)
 upper <- uniroot(f = find.pi, interval = c(mode$maximum, 1), shape1 = w + a, shape2 = n + b - w,
  height = save.res$root)
 data.frame(lower$root, upper$root)
 
 # Plot the p(pi|w) and the HPD
 x11(width = 7, height = 6, pointsize = 12)
 # pdf(file = "c:\\figures\\Figure6.9color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 curve(expr = dbeta(x = x, shape1 = w + a, shape2 = n + b - w), xlim = c(0,1),
  xlab = expression(pi), ylab = expression(paste("p(", pi, "|w)")), col = "red", lwd = 2)
 abline(h = 0)

 abline(h = save.res$root, lty = "dashed")
 segments(x0 = lower$root, y0 = 0, x1 = lower$root, y1 = save.res$root, lty = "dashed")
 segments(x0 = upper$root, y0 = 0, x1 = upper$root, y1 = save.res$root, lty = "dashed")
 # dev.off()  # Create plot for book


 # Black-and-white version of plot
 # pdf(file = "c:\\figures\\Figure6.9BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 curve(expr = dbeta(x = x, shape1 = w + a, shape2 = n + b - w), xlim = c(0,1),
  xlab = expression(pi), ylab = expression(paste("p(", pi, "|w)")), col = "black", lwd = 2)
 abline(h = 0)

 abline(h = save.res$root, lty = "dashed")
 segments(x0 = lower$root, y0 = 0, x1 = lower$root, y1 = save.res$root, lty = "dashed")
 segments(x0 = upper$root, y0 = 0, x1 = upper$root, y1 = save.res$root, lty = "dashed")
 # dev.off()  # Create plot for book


################################################################################
# Some packages that find HPDs using simulated data
# 1) HPD() from LaplacesDemon package
# 2) HPDinterval() from coda package



