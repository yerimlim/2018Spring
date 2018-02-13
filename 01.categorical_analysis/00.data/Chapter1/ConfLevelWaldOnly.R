###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  11-2-10                                                          #
# PURPOSE: True confidence level for Wald only for pi = 0.001 to 0.999    #
# NOTES:                                                                  #
###########################################################################


# Initial settings and calculations
alpha<-0.05
n<-40
w<-0:n
pi.hat<-w/n
pi.seq<-seq(from = 0.001, to = 0.999, by = 0.0005)
# pi.seq<-0.16  # Testing
# pi.seq<-seq(from = 0.1, to = 0.9, by = 0.1)  # Testing

# Wald
var.wald<-pi.hat*(1-pi.hat)/n
lower.wald<-pi.hat - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper.wald<-pi.hat + qnorm(p = 1-alpha/2) * sqrt(var.wald)

# Save true confidence levels in a matrix
save.true.conf<-matrix(data = NA, nrow = length(pi.seq), ncol = 2)

# Create counter for the loop
counter<-1

# Loop over each pi that the true confidence level is calculated on
for(pi in pi.seq) {

  pmf<-dbinom(x = w, size = n, prob = pi)

  save.wald<-ifelse(test = pi>lower.wald, yes = ifelse(test = pi<upper.wald, yes = 1, no = 0), no = 0)
  wald<-sum(save.wald*pmf)

  save.true.conf[counter,]<-c(pi, wald)
  # print(save.true.conf[counter,])  # Uncomment to view results one-by-one
  counter<-counter+1
  
  }
  

# Plot
x11(width = 7, height = 6, pointsize = 12)
plot(x = save.true.conf[,1], y = save.true.conf[,2], main = "Wald", xlab = expression(pi),
  ylab = "True confidence level", type = "l", ylim = c(0.85,1))
abline(h = 1-alpha, lty = "dotted")




######################################################################################

  # Alternatively, these true confidence levels can be found using the binom.coverage()
  #  function in the binom package
  library(binom)
  binom.coverage(p = 0.16, n = n, conf.level = 0.95, method = "asymptotic")
  
  # Plots of the true confidence levels can be found from the binom.plot()
  #  function in the binom package. Note that the method argument uses a function name
  #  that calculates the individual confidence interval (binom.asymp is within the binom 
  #  package. Also, the y-axis is the true confidence level and the x-axis is pi. The 
  #  lattice package is used for plotting so the usual arguments (like xlab = ) for basic 
  #  R plots do not always work.  
  x11(width = 7, height = 6, pointsize = 12)
  binom.plot(n = 40, method = binom.asymp, np = 500, conf.level = 0.95)



#
