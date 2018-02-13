###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  11-8-10                                                          #
# PURPOSE: C.I.s for pi                                                   #
# NOTES:                                                                  #
###########################################################################

# Basic computations
sum.y<-42
n<-1875
alpha<-0.05
pi.hat<-sum.y/n

# Wald C.I.
wald<-round(pi.hat + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(pi.hat*(1-pi.hat)/n),4)

# Agresti-Coull C.I.
p.tilde<-(sum.y + qnorm(p = 1-alpha/2)^2 /2) / (n+qnorm(1-alpha/2)^2)
p.tilde
AC<-round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(p.tilde*(1-p.tilde) / (n+qnorm(1-alpha/2)^2)),4)

# Wilson C.I.
wilson<-round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(n) / (n+qnorm(1-alpha/2)^2) * sqrt(pi.hat*(1-pi.hat) + qnorm(1-alpha/2)^2/(4*n)),4)

# Clopper-Pearson
CP<-round(qbeta(p = c(alpha/2, 1-alpha/2), shape1 = c(sum.y, sum.y+1), shape2 = c(n-sum.y+1, n-sum.y)),4)

# All intervals
rbind(wald, AC, wilson, CP)

# Fancier way to display the results
CI.names<-c("Wald", "Agresti-Coull", "Wilson", "Clopper-Pearson")
length<-c(wald[2]-wald[1], AC[2]-AC[1], wilson[2]-wilson[1], CP[2]-CP[1])
intervals<-rbind.data.frame(wald, AC, wilson, CP)  # Creates a 4x2 data.frame
names(intervals)<-c("lower limit", "upper limit")  # Adds variable names to the data.frame
data.frame(CI.names, intervals, length)
 

# Binom package
library(binom)
binom.confint(x = sum.y, n = n, conf.level = 1-alpha, methods = "all")

