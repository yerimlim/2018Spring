###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  11-2-10                                                          #
# PURPOSE: True confidence level for Wald, AC, Wilson, and CP intervals   #
# NOTES:                                                                  #
###########################################################################


# Initial settings and calculations
alpha<-0.05
n<-40
w<-0:n
pi.hat<-w/n
p.tilde<-(w + qnorm(p = 1-alpha/2)^2 /2) / (n+qnorm(1-alpha/2)^2)

# Wald
var.wald<-pi.hat*(1-pi.hat)/n
lower.wald<-pi.hat - qnorm(p = 1-alpha/2) * sqrt(var.wald)
upper.wald<-pi.hat + qnorm(p = 1-alpha/2) * sqrt(var.wald)

# Agresti-Coull
lower.AC<-p.tilde - qnorm(p = 1-alpha/2) * sqrt(p.tilde*(1-p.tilde) / (n+qnorm(1-alpha/2)^2))
upper.AC<-p.tilde + qnorm(p = 1-alpha/2) * sqrt(p.tilde*(1-p.tilde) / (n+qnorm(1-alpha/2)^2))

# Wilson
lower.wilson<-p.tilde - qnorm(p = 1-alpha/2) * sqrt(n) / (n+qnorm(1-alpha/2)^2) * sqrt(pi.hat*(1-pi.hat) + qnorm(1-alpha/2)^2/(4*n))
upper.wilson<-p.tilde + qnorm(p = 1-alpha/2) * sqrt(n) / (n+qnorm(1-alpha/2)^2) * sqrt(pi.hat*(1-pi.hat) + qnorm(1-alpha/2)^2/(4*n))

# Clopper-Pearson - This is a little more complicated due to the y = 0 and n cases
  lower.CP<-numeric(n+1)  # This initializes a vector to save the lower bounds into
  upper.CP<-numeric(n+1)  # This initializes a vector to save the upper bounds into

  # y = 0
  w0<-0  # Set here for emphasis
  lower.CP[1]<-0
  upper.CP[1]<-qbeta(p = 1-alpha/2, shape1 = w0+1, shape2 = n-w0)

  # y = n
  wn<-n  # Set here for emphasis
  lower.CP[n+1]<-qbeta(p = alpha/2, shape1 = wn, shape2 = n-wn+1)
  upper.CP[n+1]<-1
  
  # y = 1, ..., n-1
  w.new<-1:(n-1)
  lower.CP[2:n]<-qbeta(p = alpha/2, shape1 = w.new, shape2 = n-w.new+1)
  upper.CP[2:n]<-qbeta(p = 1-alpha/2, shape1 = w.new+1, shape2 = n-w.new)


# All pi's
pi.seq<-seq(from = 0.001, to = 0.999, by = 0.0005)
# pi.seq<-0.16 #Testing
# pi.seq<-seq(from = 0.1, to = 0.9, by = 0.1) #Testing

# Save true confidence levels in a matrix
save.true.conf<-matrix(data = NA, nrow = length(pi.seq), ncol = 5)

# Create counter for the loop
counter<-1

# Loop over each pi that the true confidence level is calculated on
for(pi in pi.seq) {

  pmf<-dbinom(x = w, size = n, prob = pi)

  # Wald
  save.wald<-ifelse(test = pi>lower.wald, yes = ifelse(test = pi<upper.wald, yes = 1, no = 0), no = 0)
  wald<-sum(save.wald*pmf)

  # Agresti-Coull
  save.AC<-ifelse(test = pi>lower.AC, yes = ifelse(test = pi<upper.AC, yes = 1, no = 0), no = 0)
  AC<-sum(save.AC*pmf)

  # Wilson
  save.wilson<-ifelse(test = pi>lower.wilson, yes = ifelse(test = pi<upper.wilson, yes = 1, no = 0), no = 0)
  wilson<-sum(save.wilson*pmf)

  # Clopper-Pearson
  save.CP<-ifelse(test = pi>lower.CP, yes = ifelse(test = pi<upper.CP, yes = 1, no = 0), no = 0)
  CP<-sum(save.CP*pmf)

  save.true.conf[counter,]<-c(pi, wald, AC, wilson, CP)
  counter<-counter+1
  
}
  

# Plots
x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure1.3.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
par(mfrow = c(2,2))  # 2x2 plotting grid
plot(x = save.true.conf[,1], y = save.true.conf[,2], main = "Wald", xlab = expression(pi),
  ylab = "True confidence level", type = "l", ylim = c(0.85,1))
abline(h = 1-alpha, lty = "dotted")
plot(x = save.true.conf[,1], y = save.true.conf[,3], main = "Agresti-Coull", xlab = expression(pi),
  ylab = "True confidence level", type = "l", ylim = c(0.85,1))
abline(h = 1-alpha, lty = "dotted")
plot(x = save.true.conf[,1], y = save.true.conf[,4], main = "Wilson", xlab = expression(pi),
  ylab = "True confidence level", type = "l", ylim = c(0.85,1))
abline(h = 1-alpha, lty = "dotted")
plot(x = save.true.conf[,1], y = save.true.conf[,5], main = "Clopper-Pearson", xlab = expression(pi),
  ylab = "True confidence level", type = "l", ylim = c(0.85,1))
abline(h = 1-alpha, lty = "dotted")
# dev.off()  # Create plot for book

# Pi = 0.157
save.true.conf[save.true.conf[,1]==0.157,]
# While AC and Wilson have same true confidence levels at pi=0.157, this will not always be the case
sum(save.true.conf[,3] != save.true.conf[,4])  # Number of differences
length(pi.seq)  # Number of true confidence levels


######################################################################################

  # Alternatively, these true confidence levels can be found using the binom.coverage()
  #  function in the binom package
  library(binom)
  binom.coverage(p = 0.16, n = n, conf.level = 0.95, method = c("asymptotic", "agresti-coull", "wilson", "exact"))
  # The above produces a warning message, but still works


  # Alternatively, plots of the true confidence levels can be found from the binom.plot()
  #  function in the binom package. Note that the method argument uses a function name
  #  that calculates the individual confidence interval. Also, the y-axis is the true
  #  confidence level and the x-axis is pi. The lattice package is used for plotting
  #  so the usual arguments (like xlab = ) for basic R plots do not always work.  #
  binom.plot(n = 40, method = binom.asymp, np = 500, conf.level = 0.95)
  binom.plot(n = 40, method = binom.agresti.coull, np = 500, conf.level = 0.95)
  binom.plot(n = 40, method = binom.wilson, np = 500, conf.level = 0.95)
  binom.plot(n = 40, method = binom.exact, np = 500, conf.level = 0.95)



#
