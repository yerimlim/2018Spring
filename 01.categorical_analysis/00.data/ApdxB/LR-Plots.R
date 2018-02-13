######################################################################
# NAME:  Tom Loughin                                                 #
# DATE:  1-26-11                                                     #
# UPDATE:                                                            #
# Purpose: Compute and plot the likelihood function                  #
#                                                                    #
# NOTES:                                                             #
#                                                                    #
######################################################################

#########################             
# Preliminary calculations

  y1<-c(3,5,6,6,7,10,13,15,18,22) 
  n1<-length(y1) #Sample size
  mle1 <- sum(y1)/n1
  mle1
  maxlik1 <- -n1*mle1 + log(mle1)*sum(y1) - sum(lfactorial(y1)) # log(L) = -n*mu +log(mu)*sum(y_i) - sum(log(y_i !)) evaluated at MLE (mu^)
  maxlik1 #Maximum possible value of log likelihood
  loglik9 <- -n1*9 + log(9)*sum(y1) - sum(lfactorial(y1)) #Suppose mu = 9
  loglik5 <- -n1*5 + log(5)*sum(y1) - sum(lfactorial(y1)) #Suppose mu = 5

  LRT9<--2*loglik9 + 2*maxlik1
  LRT5<--2*loglik5 + 2*maxlik1
  data.frame(LRT9, LRT5)


#########################
# Likelihood Ratio Plot

  x11(width = 6, height = 6, pointsize = 15)
  # pdf(file = "c:\\figures\\FigureB.3-1BW.pdf", width = 6, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  # par(mfrow = c(1,2))
  curve(expr = -n1*x + log(x)*sum(y1) - sum(lfactorial(y1)), from = 2, to = 27, ylim = c(-94,-34), 
        xlab = expression(mu), ylab = "Log-likelihood")
  
  # log[L(mu^|y)]
  segments(x0 = 0, y0 = maxlik1, x1 = mle1, y1 = maxlik1, lty="dotted")
  segments(x0 = mle1, y0 = -100, x1 = mle1, y1 = maxlik1, lty="dotted")
  text(x = mle1+1.5, y = -93, labels = expression(mu == hat(mu)))
  text(x = 7, y = maxlik1+3, labels = expression(paste("log[L(", hat(mu), "|", bold(y), ")]")))
  
  # log[L(mu=9|y)]
  segments(x0 = 0, y0 = loglik9, x1 = 9, y1 = loglik9, lty="dotted")
  segments(x0 = 9, y0 = -100, x1 = 9, y1 = loglik9, lty="dotted")
  text(x = 9-1.5, y = -93.5, labels = expression(mu == 9))
  text(x = 4, y = loglik9-2, labels = expression(paste("log[L(", 9, "|", bold(y), ")]")))
   
  # log[L(mu=5|y)]
  segments(x0 = 0, y0 = loglik5, x1 = 5, y1 = loglik5, lty="dotted")
  segments(x0 = 5, y0 = -100, x1 = 5, y1 = loglik5, lty="dotted")
  text(x = 5-1.5, y = -93.5, labels = expression(mu == 5))
  text(x = 4, y = loglik5+2.5, labels = expression(paste("log[L(", 5, "|", bold(y), ")]")))
  # dev.off()  # Create plot for book


#########################
# LR Rejection Region Plot

  x11(width = 6, height = 6, pointsize = 15)
  # pdf(file = "c:\\figures\\FigureB.3-2BW.pdf", width = 6, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  curve(expr = -n1*x + log(x)*sum(y1) - sum(lfactorial(y1)), from = 2, to = 27, ylim = c(-94,-34),
        xlab = expression(mu), ylab = "Log-likelihood")

  # log[L(mu^|y)]
  segments(x0 = 0, y0 = maxlik1, x1 = mle1, y1 = maxlik1, lty="dotted")
  segments(x0 = mle1, y0 = -100, x1 = mle1, y1 = maxlik1, lty="dotted")
  text(x = mle1+.5, y = -93, labels = expression(mu == hat(mu)))
  text(x = 7, y = maxlik1+3, labels = expression(paste("log[L(", hat(mu), "|", bold(y), ")]")))

  # 0.5*chi^2_1,0.95 line
  abline(h = maxlik1 - qchisq(p = 0.95, df = 1)/2, lty = "dashed")  # log[L(mu|y)] = log[L(mu^|y)] - (chi^2_1,0.95)/2
  arrows(x0 = 4.6, y0 = -41, x1 = 6.2, y1 = maxlik1 - qchisq(p = 0.95, df = 1)/2 - 0.5, length = 0.08)
  text(x = 4, y = maxlik1-5, labels = expression(0.5*chi[paste(1, ",", 0.95)]^2)) 

  
  # In order find the lower and upper bounds for the rejection region, we can solve for mu in
  #  -2log[L(mu|y)] + 2log[L(mu^|y)] - chi^2_1,0.95
  op.func<-function(mu, y, alpha, maxlikMLE) {
    n<-length(y)
    -2*(-n*mu + log(mu)*sum(y) - sum(lfactorial(y))) + 2*maxlikMLE - qchisq(p = 1-alpha, df = 1) 
  } 
     -n1*mle1 + log(mle1)*sum(y1) - sum(lfactorial(y1))
  lower<-uniroot(f = op.func, interval = c(0, mle1), y = y1, alpha = 0.05, maxlikMLE = maxlik1)  # Lower bound
  upper<-uniroot(f = op.func, interval = c(mle1, 28), y = y1, alpha = 0.05, maxlikMLE = maxlik1)  # Upper bound
  optimize(f = op.func, interval = c(0, 28), y = y1, alpha = 0.05, maxlikMLE = maxlik1)  # Used as a check

 
  # Left rejection region
  segments(x0 = lower$root, y0 =-100, x1 = lower$root, y1 = maxlik1-qchisq(p = 0.95, df = 1)/2, lty="dashed")
  text(x = 5.9, y = -88, labels = "Rejection")
  text(x = 5.9, y = -94, labels = "Region")
  arrows(x0 = lower$root-0.5, y0 = -91, x1 = lower$root-0.5-4, y1 = -91, length = 0.08)

  # Right rejection region
  segments(x0 = upper$root, y0 =-100, x1 = upper$root, y1 = maxlik1-qchisq(p = 0.95, df = 1)/2, lty="dashed")
  text(x = 15.5, y = -88, labels = "Rejection")
  text(x = 15.5, y = -94, labels = "Region")
  arrows(x0 = upper$root+0.5, y0 = -91, x1 = upper$root+0.5+4, y1 = -91, length = 0.08)
  # dev.off()  # Create plot for book




  #