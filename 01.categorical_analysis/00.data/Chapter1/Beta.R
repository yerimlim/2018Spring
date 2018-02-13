###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  10-1-10                                                          #
# PURPOSE: Plots of the beta distribution                                 #
# NOTES:                                                                  #
###########################################################################

# Basic computations
x11(width = 6, height = 6, pointsize = 12)
par(mfrow = c(2,2))
curve(expr = dbeta(x = x, shape1 = 1, shape2 = 1), ylab = "f(v)", 
  xlab = "v", from = 0, to = 1, ylim = c(0,5), main = "beta(1,1)")
curve(expr = dbeta(x = x, shape1 = 1, shape2 = 5), ylab = "f(w)", 
  xlab = "v", from = 0, to = 1, ylim = c(0,5), main = "beta(1,5)")
curve(expr = dbeta(x = x, shape1 = 5, shape2 = 1), ylab = "f(w)", 
  xlab = "v", from = 0, to = 1, ylim = c(0,5), main = "beta(5,1)")
curve(expr = dbeta(x = x, shape1 = 5, shape2 = 5), ylab = "f(w)", 
  xlab = "v", from = 0, to = 1, ylim = c(0,5), main = "beta(5,5)")


#
