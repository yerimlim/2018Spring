######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  1-27-11                                                     #
# UPDATE:                                                            #
# Purpose: Simple example for Newton-Raphson algorithm               #
#                                                                    #
# NOTES:                                                             #
#                                                                    #
######################################################################

  # Data
  sum.y<-4
  n<-10
  w<-sum.y

  # Initialize some values
  epsilon<-0.0001      # Convergence criteria
  pi.hat<-0.3          # Start value
  save.pi.hat<-pi.hat  # Save the results for each iteration here and put pi.hat as first value
  change<-1            # Initialize change for first time through loop

  # Loop to find the MLE (uses Newton-Raphson algorithm)
  while (abs(change) > epsilon) {
    num<-(w-n*pi.hat) / (pi.hat*(1-pi.hat))
    den<- -w/pi.hat^2 - (n-w)/(1-pi.hat)^2
    pi.hat.new<-pi.hat - num/den  # pi^(i+1) = pi^(i) - (1st der log(L))/(2nd der log(L))
    change<-pi.hat.new-pi.hat  # -num/den
    pi.hat<-pi.hat.new  # Same for next time through loop
    save.pi.hat<-c(save.pi.hat, pi.hat)  # Keeps iteration history
  }

  # Print iteration history
  data.frame(iteration = 1:length(save.pi.hat), save.pi.hat)


  # Log-likelihood function plot
  curve(expr = sum.y*log(x) + (n-sum.y)*log(1-x), from = 0, to = 1,
      xlab = expression(pi), ylab = "Log-likelihood function")
  points(x = save.pi.hat, y = sum.y*log(save.pi.hat) + (n-sum.y)*log(1-save.pi.hat), pch = 1)  # Each pi^(i)

  # Zoomed in Log-likelihood function plot - use when first pi.hat = 0.3
  curve(expr = sum.y*log(x) + (n-sum.y)*log(1-x), from = 0.25, to = 0.5,
        xlab = expression(pi), ylab = "Log-likelihood function")
  points(x = save.pi.hat, y = sum.y*log(save.pi.hat) + (n-sum.y)*log(1-save.pi.hat), pch = 1)  # Each pi^(i)
  segments(x0 = save.pi.hat, y0 = -10, x1 = save.pi.hat, y1 = sum.y*log(save.pi.hat) + (n-sum.y)*log(1-save.pi.hat),
    lty = "dotted")  # Could choose a different y0 to make this more general


  # first derivation of log-likelihood function
  curve(expr = (w-n*x) / (x*(1-x)), from = 0.2, to = 0.6,
      xlab = expression(pi), ylab = "First derivative of log-likelihood function")
  abline(h=0, col = "blue", lty = "dotted")  # We are trying to find the value of pi that makes the first der of L(pi) equal to 0
  points(x = save.pi.hat, y = (w-n*save.pi.hat) / (save.pi.hat*(1-save.pi.hat)), pch = 1)  # Each pi^(i)

  # Linear approximation using 1st order Taylor's series expansion: f(x) = f(x0) + (x-x0)*fp(x0) where x0 is what is being expanded
  #  about and fp() is first derivative of f()
  x0<-0.3 #Start value
  fp.x0<- -( (1-x0)^2*w + x0^2*(n-w) ) / (x0^2*(1-x0)^2)   # Slope of tangent line at x0
  f.x0<-(w-n*x0) / (x0*(1-x0))
  yint<-f.x0 - x0*fp.x0
  abline(a = yint, b = fp.x0, lty = "dashed", col = "red")
  legend(x =0.35, y = 14, bty = "n", legend = c(expression(paste(frac(d, paste(d,pi)), " ", paste("log[L(", pi, "|", bold(y), ")]"))),
    expression(paste("Tangent line at ", pi==0.3)), expression(paste(pi, " estimates"))), lty = c("solid", "dashed", NA), col = c("black", "red"),
    pch = c(NA, NA, 1))
  # Where the tangent line intersects x-axis = 0 corresponds to pi^(i+1)



##############################################
# Example using optim

  # Negative of the log-likelihood function (optim() finds a minimum, but we need a maximum so use a negative here)
  LogLik<-function(pi, sum.y, n) {
    -(sum.y*log(pi) + (n-sum.y)*log(1-pi))
  }

  optim(par = 0.3, fn = LogLik, sum.y = sum.y, n = n)
  
  # A function that works for one parameter
  optimize(f = LogLik, interval = c(0,1), sum.y = sum.y, n = n)






#