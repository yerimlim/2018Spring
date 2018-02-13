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
# Sample #1

  y1<-c(3,5,6,6,7,10,13,15,18,22) 
  n1<-length(y1)  # Sample size
  mle1 <- sum(y1)/n1
  mle1
  maxlik1 <- -n1*mle1 + log(mle1)*sum(y1) - sum(lfactorial(y1))  # log(L) = -n*mu +log(mu)*sum(y_i) - sum(log(y_i !)) evaluated at MLE (mu^)
  maxlik1  # Maximum possible value of log likelihood
  # curve(expr=-n1*x + log(x)*sumy1 - sumlfac1, from=3, to=25, xlab=expression(mu),ylab="Log Likelihood")
  
  mle1^2/sum(y1)  # Var^(mu^)

#########################
# Sample #2

  y2 <- c(9,12)
  n2<-length(y2)
  
  # Example of evaluating log(L)
  mu <- c(1:25)
  loglik2 <- -n2*mu + log(mu)*sum(y2) - sum(lfactorial(y2)) # log(L) = -n*mu +log(mu)*sum(y_i) - sum(log(y_i !)) 
  data.frame(mu,loglik2)
 
  mle2 <- sum(y2)/n2
  mle2
  maxlik2 <- -n2*mle2 + log(mle2)*sum(y2) - sum(lfactorial(y2)) 
  maxlik2  # Maximum possible value of log likelihood

  mle2^2/sum(y2)  # Var^(mu^)

#########################
# Plot 

  x11(width = 7, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\FigureB.2BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mar = c(5, 5, 4, 2) + 0.1)  # Make sure left margin does not cut off ^ on mu
  curve(expr=-n1*x + log(x)*sum(y1) - sum(lfactorial(y1)) - maxlik1, from=3, to=25, xlab=expression(mu),
    ylab = expression(paste("log[L(", mu, "|", bold(y), ")", "]", " - " , "log[L(", hat(mu), "|", bold(y), ")", "]")))
  curve(expr=-n2*x + log(x)*sum(y2) - sum(lfactorial(y2)) - maxlik2, add=TRUE, lty="dashed")
  legend(x=12, y=-40, legend=c("Sample 1", "Sample 2", "MLE"), lty=c("solid", "dashed", "dotted"), bty="n")
  abline(v=mle1, lty="dotted")  # MLE for both samples
  # dev.off()  # Create plot for book
