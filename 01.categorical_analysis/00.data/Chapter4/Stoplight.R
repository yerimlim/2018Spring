###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  7-14-11                                                          #
# PURPOSE: Number of vehicles stopped at stoplight                        #
# NOTES:                                                                  #
###########################################################################

########################################################################
# Read in data

stoplight <- read.csv(file  = "C:\\data\\stoplight.csv")
head(stoplight)


########################################################################
# Compare to Poisson

# Summary statistics
mean(stoplight$vehicles)
var(stoplight$vehicles)

# Frequencies
table(stoplight$vehicles) #Note that y = 0, 1, ..., 8 all have positive counts
rel.freq <- table(stoplight$vehicles)/length(stoplight$vehicles)
rel.freq2 <- c(rel.freq, rep(0, times = 7))

# Poisson calculations
y <- 0:15
prob <- round(dpois(x = y, lambda = mean(stoplight$vehicles)), 4)

# Observed and Poisson
data.frame(y, prob, rel.freq = rel.freq2)

# Plot
x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure4.1color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = y - 0.1, y = prob, type = "h", ylab = "Probability", xlab = "Number of vehicles", lwd = 2,
     xaxt = "n")
axis(side = 1, at = 0:15)
lines(x = y + 0.1, y = rel.freq2, type = "h", lwd = 2, lty = "solid", col = "red")
abline(h = 0)
legend(x = 9, y = 0.15, legend = c("Poisson", "Observed"), lty = c("solid", "solid"), lwd = c(2,2), col = c("black", "red"), bty = "n")
# dev.off()  # Create plot for book

# B/W Version
x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure4.1BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = y - 0.1, y = prob, type = "h", ylab = "Probability", xlab = "Number of vehicles", lwd = 2,
     xaxt = "n")
axis(side = 1, at = 0:15)
lines(x = y + 0.1, y = rel.freq2, type = "h", lwd = 2, lty = "solid", col = "gray70")
abline(h = 0)
legend(x = 9, y = 0.15, legend = c("Poisson", "Observed"), lty = c("solid", "solid"), lwd = c(2,2), col = c("black", "gray70"), bty = "n")
# dev.off()  # Create plot for book

# NOTE: should not use dashed lines due to the type of plot - will not know exactly where top of bar is


########################################################################
# CI

alpha <- 0.05
n <- length(stoplight$vehicles)
mu.hat <- mean(stoplight$vehicles)

# Wald
mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))*sqrt(mu.hat/n)

# Score
(mu.hat + qnorm(p = c(alpha/2, 1 - alpha/2))/(2*n)) + qnorm(p = c(alpha/2, 1 - alpha/2)) * sqrt((mu.hat + qnorm(p = 1 - alpha/2)/(4*n))/n)

# Exact
qchisq(p = c(alpha/2, 1 - alpha/2), df = c(2*n*mu.hat, 2*(n*mu.hat + 1)))/(2*n)
# Other code for exact
# qgamma(p = c(alpha/2, 1-alpha/2), shape = c(n*mu.hat, n*mu.hat+1), scale = 1)/n
# c(qchisq(p = alpha/2, df = 2*n*mu.hat),qchisq(p = 1-alpha/2, df = 2*(n*mu.hat+1)))/(2*n)

# Exact using poisson.test in stats package
poisson.test(x = mu.hat*n)$conf.int / n

# Usual t-distribution based interval
t.test(x = stoplight$vehicles, conf.level = 0.95)

#######################################################################
# Repeating CI calculations using functions from epitools package.
#  For each function, x=sum of data (using n*mu.hat here) and 
#  pt = sample size in our applications.  See help(pois.conf.int).  

library(epitools)

pois.exact(x = mu.hat*n, pt = n, conf.level = 0.95)
pois.approx(x = mu.hat*n, pt = n, conf.level = 0.95)



#