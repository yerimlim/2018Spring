###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  9-30-10                                                          #
# PURPOSE: C.I.s for pi                                                   #
# NOTES:                                                                  #
###########################################################################

# Basic computations
w<-4  # Sum(y_i)
n<-10
alpha<-0.05
pi.hat<-w/n

# Wald C.I.
var.wald<-pi.hat*(1-pi.hat)/n
lower<-pi.hat - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper<-pi.hat + qnorm(p = 1-alpha/2) * sqrt(var.wald)
round(data.frame(lower, upper), 4)
# More simply:
round(pi.hat + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.wald),4)

# Adjusted estimate of pi
p.tilde<-(w + qnorm(p = 1-alpha/2)^2 /2) / (n + qnorm(p = 1-alpha/2)^2)
p.tilde

# Wilson C.I.
round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(n) / (n+qnorm(p = 1-alpha/2)^2) * sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n)),4)

# Agresti-Coull C.I.
var.ac<-p.tilde*(1-p.tilde) / (n+qnorm(p = 1-alpha/2)^2)
round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.ac),4)

# Binom package
library(package = binom)
binom.confint(x = w, n = n, conf.level = 1-alpha, methods = "all")

# Could just obtain one interval at a time and save into an object
# Agresti-Coull: 
save.ci<-binom.confint(x = w, n = n, conf.level = 1-alpha, methods = "ac")
save.ci

# Clopper-Pearson
round(qbeta(p = c(alpha/2, 1-alpha/2), shape1 = c(w, w+1), shape2 = c(n-w+1, n-w)),4)
# lower<-qbeta(p = alpha/2, shape1 = w, shape2 = n-w+1)  #This is a check
# upper<-qbeta(p = 1-alpha/2, shape1 = w+1, shape2 = n-w)   #This is a check
# cat("The C-P C.I. is:", round(lower,4) , "< pi <", round(upper,4), "\n \n")

# Clopper-Pearson
binom.confint(x = w, n = n, conf.level = 1-alpha, methods = "exact")


########################################
# Other intervals

  ###################
  # LR interval
  
    LRT.int<-binom.confint(x = w, n = n, conf.level = 1-alpha, methods = "lrt")
    LRT.int
  
    # -2log(Lambda)
    LRT<-function(pi.0, w, n) {
      pi.hat<-w/n
      -2*(w*log(pi.0/pi.hat) + (n-w)*log((1-pi.0)/(1-pi.hat))) 
    }
  
    # Plot of -2log(Lambda) - the values of pi below the chi^2_1,1-alpha horizontal line corresponds to the interval
    curve(expr = LRT(pi.0 = x, w = w, n = n), from = 0, to = 1, col = "red", xlab = expression(pi),
      ylab = expression(-2*log(Lambda)))
    abline(h = qchisq(p = 1-alpha, df = 1), lty = "dotted")
  
    # Intersection points for -2log(Lambda) and the chi^2_1,1-alpha horizontal line
    LRT(pi.0 = LRT.int$lower, w = w, n = n)  # Check - should be chi^2_1,1-alpha
    LRT(pi.0 = LRT.int$upper, w = w, n = n)  # Check - should be chi^2_1,1-alpha
 
    # Set -2log(Lambda) - chi^2_1,1-alpha equal to 0 and solve for root to find lower and upper bounds for interval
    LRT2<-function(pi.0, w, n, alpha) {
      pi.hat<-w/n
      -2*(w*log(pi.0/pi.hat) + (n-w)*log((1-pi.0)/(1-pi.hat))) - qchisq(p = 1-alpha, df = 1)
    }
  
    # Confidence interval, differences between here and binom.confint() are very small
    uniroot(f = LRT2, lower = 0, upper = w/n, w = w, n = n, alpha = alpha)  # Lower bound
    uniroot(f = LRT2, lower = w/n, upper = 1, w = w, n = n, alpha = alpha)  # Upper bound
  
  

  # Logit interval
  binom.confint(x = w, n = n, conf.level = 1-alpha, methods = "logit")


  # Blaker interval
  library(package = BlakerCI)
  binom.blaker.limits(x = w, n = n, level = 1-alpha)


  # Mid-p
  library(package = PropCIs)
  midPci(x = w, n = n, conf.level = 1 - alpha)


  
  
#
