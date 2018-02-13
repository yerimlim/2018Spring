#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 5-20-13                                                     #
# PURPOSE: Demonstration of effects of overdispersion               #
#                                                                   #
# NOTES:                                                            #
#####################################################################

# First start with simple simulation using two cases: Po(100) and P(Z), where Z~N(100,20^2)
# Observe means and variances from 10 simulated data of size 50.

set.seed(389201892)

# Create Poisson random variates whose means vary according to N(100,SD) for various values of SD
#  Break these into 100 data sets (columns of the matrix) of n = 20 obs each.
#  poi**: the ** gives the SD. 
poi0 <- matrix(rpois(n = 500, lambda = rep(x = 100, times = 500)), nrow = 50, ncol = 10)
poi20 <- matrix(rpois(n = 500, lambda = rnorm(n = 500, mean = 100, sd = 20)), nrow = 50, ncol = 10)

# Compute the mean and variance for each data set (columns of each poi** matrix)
mean0 <- apply(X = poi0, MARGIN = 2, FUN = mean)
var0 <- apply(X = poi0, MARGIN = 2, FUN = var)

mean20 <- apply(X = poi20, MARGIN = 2, FUN = mean)
var20 <- apply(X = poi20, MARGIN = 2, FUN = var)

all <- cbind(mean0, var0, mean20, var20)
round(all, digits = 1)
round(apply(X = all, MARGIN = 2, FUN = mean), digits = 1)

#########################################################################
# Extend simulation to a larger example: 
# Same case as above, except that all data are from Po(Z), Z~N(100, s^2),
#  where s = 0:20. (s = 0 corresponds to a constant mean for all data)
# Here we also fit a GLM to the data to estimate the mean and get 
# * deviance/dfmodel
# * model-based confidence intervals for the mean.
# Plotting various quantities, including estimated confidence level for CIs.

# Function to record CI, residual deviance, and df from model that assumes just an intercept
fit.mod <- function(response) {
 mod.fit <- glm(formula = response ~ 1, family = poisson(link = "log"))
 c(mod.fit$deviance, mod.fit$df.residual, confint(mod.fit))
}

# Function that simulates the needed data for a given SD.
# Also computes mean and variance of data and runs the fit.mod function on the data
# to get model statistics
all <- function(sd.value) {
 poi <- matrix(data = rpois(n = 2000, lambda = rnorm(n = 2000, mean = 100, sd = sd.value)), nrow = 20, ncol = 100)
 mean.val <- apply(X = poi, MARGIN = 2, FUN = mean)
 var.val <- apply(X = poi, MARGIN = 2, FUN = var)
 save.dev <- apply(X = poi, MARGIN = 2, FUN = fit.mod)
 cbind(mean.val, var.val, t(save.dev))
}
# Set the SD values to be used and initialize a matrix for the results
sd.set <- c(0:20)
mat.save <- matrix(data = NA, nrow = 100*length(sd.set), ncol = 7)

# Run the simulation and analysis functions for the chosen SD values
count <- 1
for(i in sd.set) {
 mat.save[(100*(count-1)+1):(100*count),] <- cbind(i, all(sd.value = i))
 count <- count+1
}


# Plot sample means from all sims vs. SD used in generating Poisson means
x11(width = 7, height = 6, pointsize = 15)
plot (x = mat.save[,1], y = mat.save[,2], xlab = "Std. Dev. of Poisson means", ylab = "Sample mean")
abline(h = 100)


# Plot sample variances from all sims vs. SD used in generating Poisson means
x11(width = 7, height = 6, pointsize = 15)
plot (x = mat.save[,1], y = mat.save[,3], xlab = "Std. Dev. of Poisson means", ylab = "Sample variance")
abline(h = 100)
curve(expr = 100+x^2, add = TRUE, lty = "dashed")

# Plot sample var/mean ratios from all sims vs. SD used in generating Poisson means
x11(width = 7, height = 6, pointsize = 15)
plot (x = mat.save[,1], y = mat.save[,4]/mat.save[,5], xlab = "Std. Dev. of Poisson means", ylab = "Deviance/DF")
abline(h = 1, lty = "solid")
abline(h = 1+2*sqrt(2/mat.save[,5]), lty = "dotted")
abline(h = 1+3*sqrt(2/mat.save[,5]), lty = "dotted")

# Plot Confidence interval widths vs. SD used in generating Poisson means
x11(width = 7, height = 6, pointsize = 15)
# pdf(file = "c:\\figures\\Figure5.6-1BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 17)   # Create plot for book
plot(x = mat.save[,1], y = exp(mat.save[,7])-exp(mat.save[,6]), xlab = "Std. Dev. of Poisson means", ylab = "Width of Poisson CI for mean")
# dev.off()

# Confidence level of confidence interval
cover <- ifelse(exp(mat.save[,7]) < 100, yes = 0, no = ifelse(exp(mat.save)[,6] > 100, yes = 0, no = 1))
conf <- by(data = cover, INDICES = mat.save[,1], FUN = mean)

x11(width = 7, height = 6, pointsize = 15)
# pdf(file = "c:\\figures\\Figure5.6-2BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 17)   # Create plot for book
plot(x = sd.set, y = conf, xlab = "Std. Dev. of Poisson means", ylab = "Estimated confidence level")
abline(h = 0.95, lty = "solid")
# dev.off()

