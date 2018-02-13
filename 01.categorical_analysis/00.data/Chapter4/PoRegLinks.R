######################################################################
# NAME:  Tom Loughin                                                 #
# DATE:  2011-06-11                                                  #
# Purpose: Demonstrate log link for Poisson regression               #
# NOTES:                                                             #
######################################################################

#########################
# set parameters for mean functions 
beta0a <- .1
beta1a <- 1
beta0b <- 5 
beta1b <- -1


  x11(width = 7, height = 6, pointsize = 15)
  # pdf(file = "c:\\figures\\Figure4.4BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  # Parameter set "a"
  curve(expr = exp(beta0a + beta1a*x), from = 0, to = 5, xlab = expression(x), ylab = expression(mu = exp(beta[0] + beta[1]*x)), lty = "solid", ylim = c(0,200), lwd = 2) 
  curve(expr = exp(beta0a + beta1a*x) + 2*sqrt(exp(beta0a + beta1a*x)), add = TRUE, lty = "dotted", lwd = 2)
  curve(expr = exp(beta0a + beta1a*x) - 2*sqrt(exp(beta0a + beta1a*x)), add = TRUE, lty = "dotted", lwd = 2)
  # Parameter set "b"
  curve(expr = exp(beta0b + beta1b*x), add = TRUE, lty = "dashed", lwd = 2)
  curve(expr = exp(beta0b + beta1b*x) + 2*sqrt(exp(beta0b + beta1b*x)), add = TRUE, lty = "dotted", lwd = 2)
  curve(expr = exp(beta0b + beta1b*x) - 2*sqrt(exp(beta0b + beta1b*x)), add = TRUE, lty = "dotted", lwd = 2)
  legend(x = 1.5, y = 200, legend = c(expression(beta[1] > 0), expression(beta[1] < 0), "95% probability range"), cex = 0.9, lty = c("solid", "dashed", "dotted"), lwd = 2, bty = "n")
  # dev.off()  # Create plot for book

