#############################################################################
# NAME:  Tom Loughin                                                        #
# DATE:  2012-01-23                                                         #
# Purpose:  Simulation to examine estimation of mean and probability of     #
#           immune in ZIP model                                             #
# NOTES:                                                                    #
#############################################################################

# Data generation logic: 
#   Generate n Bernoullis, 1=susceptible, 0=immune [P(Susc.) = 1-P(Immune)}
#   Generate n Poisson counts 
#   Multiply count by Bernoulli! If Immune, count=0.  Otherwise, count=Poisson

# Analysis function
#   Function to fit ZIP models with no explanatories
#   Get out the estimated mean and P(0) with corresponding CIs

# Note on limitations:
#   Pilot runs highlighted a problem with the use of large values of pi:
#   some data sets were generated that contained 35 counts of 0. The symptom that we 
#    observed were NA values for some of our summary statistics, which led us to 
#    discover the source of the problem. When there are no non-zero counts, there 
#    is no way to distinguish between very small mu and a very large pi, so some 
#    estimates of infinity are produced. We can calculate that 
#    P(All Y's are 0)=P(Y=0)^{35}=[pi+(1-pi)exp(-mu)]^{35}
#    =.009 when pi=0.8 and 0.102 when pi=0.9.  
#   Thus, in generating 1000 data sets from each of these cases, we expect roughly 
#    9 and 102 sets of all-zero data to be generated, respectively. Indeed, we 
#    observed 9 and 101 such sets, respectively, in our pilot study, and none for 
#    other values of pi, for which the probability of all-zero data is <1/1000. 
#   We therefore added code to omit these cases from the calculation of the summary 
#    statistics.

library(pscl)
zipmod <- function(y) {
  mod.fit <- zeroinfl(y ~ 1 | 1, dist = "poisson")
  log.mu.stats <- c(coef(mod.fit)[1], confint(mod.fit)[c(1,3)])
  logit.pi.stats <- c(coef(mod.fit)[2], confint(mod.fit)[c(2,4)])
  mu.stats <- exp(log.mu.stats)
  pi.stats <- exp(logit.pi.stats)/(1 + exp(logit.pi.stats))
  c(mu.stats, pi.stats)
}
  
# Check the function
suscept <- rbinom(n = 10000, size = 1, prob = 0.2)
count <- rpois(n = 10000, lambda = 1)
y = suscept*count

zipmod(y)

# Function to create and analyze data
sim.zip <- function(n, pi, mu, sets, seed) {
  # Set seed number to reproduce results and simulate responses
  set.seed(seed)
  suscept <- rbinom(n = n*sets, size = 1, prob = 1 - pi)
  count <- rpois(n = n*sets, lambda = mu)
  y = suscept*count

  # Restructure y for apply() function
  y.mat <- matrix(data = y, nrow = n, ncol = sets)
  # Get rid of all=zero data sets, which create "INF" estimates
  y.mat <- y.mat[,which(apply(X = y.mat, MARGIN = 2, FUN = max) > 0)]

  # y.mat[1:3,1:5] # Check

  # Analyze data
  save.results <- apply(X = y.mat, MARGIN = 2, FUN = zipmod)
  cat("Legitimate data sets (not all 0) = ", ncol(save.results), "\n")

  # Calculate the true confidence level and investigate problems
  cat("Average estimated mean = ", mean(save.results[1,]), "\n")
  mu.cover <- ifelse(test = mu > save.results[2,], yes = ifelse(test = mu < save.results[3,], yes = 1, no = 0), no = 0)
  cat("Est. true conf. level for mean:", sum(mu.cover)/sets, "\n")
  cat("Median confidence interval for mean = ", median(save.results[2,]), ",", median(save.results[3,]), "\n\n")

  cat("Average estimated P(Immune) = ", mean(save.results[4,]), "\n")
  pi.cover <- ifelse(test = pi > apply(X = save.results[c(5,6),], MARGIN = 2, FUN = min), yes = ifelse(test = pi < apply(X = save.results[c(5,6),], MARGIN = 2, FUN = max), yes = 1, no = 0), no = 0)
  cat("Est. true conf. level for P(Immune):", sum(pi.cover)/sets, "\n")  
  cat("Median confidence interval for P(Immune) = ", median(save.results[5,]), ",", median(save.results[6,]), "\n")
  save.results
}
# Keeping mu=1 and varying pi

save.0 <- sim.zip(n = 35, pi = 0, mu = 1, sets = 1000, seed = 23661882)
save.1 <- sim.zip(n = 35, pi = 0.1, mu = 1, sets = 1000, seed = 985125)
save.2 <- sim.zip(n = 35, pi = 0.2, mu = 1, sets = 1000, seed = 329482929)
save.5 <- sim.zip(n = 35, pi = 0.5, mu = 1, sets = 1000, seed = 601299438)
save.8 <- sim.zip(n = 35, pi = 0.8, mu = 1, sets = 1000, seed = 9287467)
save.9 <- sim.zip(n = 35, pi = 0.9, mu = 1, sets = 1000, seed = 012839299)

# Plot estimates of mean and P(Immune) in Color
x11(width = 15, height = 15, pointsize = 14)
# pdf(file = "c:\\figures\\Figure4.11color.pdf", width = 15, height = 15, colormodel = "cmyk", pointsize = 30)   # Create plot for book
par(mfrow = c(3,2))
line.width <- 4

# mu 1
hist(x = save.1[1,], breaks = c(0:12)/2, xlab = expression(hat(mu)), main = expression(paste(pi, "=0.1, ", mu, "=1")))
abline(v = 1, col = "red", lwd = line.width)
abline(v = mean(save.1[1,]), col = "blue", lwd = line.width)

# pi 0.1
hist(x = save.1[4,], breaks = c(0:10)/10, xlab = expression(hat(pi)), main = expression(paste(pi, "=0.1, ", mu, "=1")))
abline(v = 0.1, col = "red", lwd = line.width)
abline(v = mean(save.1[4,]), col = "blue", lwd = line.width)
legend(x = 0.4, y = 500, legend = c("True Value", "Mean Estimate"), col = c("red", "blue"), lwd = line.width, bty = "n")

# mu 1
hist(x = save.5[1,], breaks = c(0:12)/2, xlab = expression(hat(mu)), main = expression(paste(pi, "=0.5, ", mu, "=1")))
abline(v = 1, col = "red", lwd = line.width)
abline(v = mean(save.5[1,]), col = "blue", lwd = line.width)

# pi 0.5
hist(x = save.5[4,], breaks = c(0:10)/10, xlab = expression(hat(pi)), main = expression(paste(pi, "=0.5, ", mu, "=1")))
abline(v = 0.5, col = "red", lwd = line.width)
abline(v = mean(save.5[4,]), col = "blue", lwd = line.width)
# mu 1
hist(x = save.9[1,], breaks = c(0:12)/2, xlab = expression(hat(mu)), main = expression(paste(pi, "=0.9, ", mu, "=1")))
abline(v = 1, col = "red", lwd = line.width)
abline(v = mean(save.9[1,]), col = "blue", lwd = line.width)

# pi 0.9
hist(x = save.9[4,], breaks = c(0:10)/10, xlab = expression(hat(pi)), main = expression(paste(pi, "=0.9, ", mu, "=1")))
abline(v = 0.9, col = "red", lwd = line.width)
abline(v = mean(save.9[4,]), col = "blue", lwd = line.width)
# dev.off()  # Create plot for book


# Plot estimates of mean and P(Immune) in B/W
x11(width = 15, height = 15, pointsize = 14)
# pdf(file = "c:\\figures\\Figure4.11BW.pdf", width = 15, height = 15, colormodel = "cmyk", pointsize = 30)   # Create plot for book
par(mfrow = c(3,2))

# mu 1
hist(x = save.1[1,], breaks = c(0:12)/2, xlab = expression(hat(mu)), main = expression(paste(pi, "=0.1, ", mu, "=1")))
abline(v = 1, lty = "dotted", lwd = line.width)
abline(v = mean(save.1[1,]), lty = "dashed", lwd = line.width)

# pi 0.1
hist(x = save.1[4,], breaks = c(0:10)/10, xlab = expression(hat(pi)), main = expression(paste(pi, "=0.1, ", mu, "=1")))
abline(v = 0.1, lty = "dotted", lwd = line.width)
abline(v = mean(save.1[4,]), lty = "dashed", lwd = line.width)
legend(x = 0.4, y = 500, legend = c("True Value", "Mean Estimate"), lty = c("dotted", "dashed"), lwd = line.width, bty = "n")

# mu 1
hist(x = save.5[1,], breaks = c(0:12)/2, xlab = expression(hat(mu)), main = expression(paste(pi, "=0.5, ", mu, "=1")))
abline(v = 1, lty = "dotted", lwd = line.width)
abline(v = mean(save.5[1,]), lty = "dashed", lwd = line.width)

# pi 0.5
hist(x = save.5[4,], breaks = c(0:10)/10, xlab = expression(hat(pi)), main = expression(paste(pi, "=0.5, ", mu, "=1")))
abline(v = 0.5, lty = "dotted", lwd = line.width)
abline(v = mean(save.5[4,]), lty = "dashed", lwd = line.width)
# mu 1
hist(x = save.9[1,], breaks = c(0:12)/2, xlab = expression(hat(mu)), main = expression(paste(pi, "=0.9, ", mu, "=1")))
abline(v = 1, lty = "dotted", lwd = line.width)
abline(v = mean(save.9[1,]), lty = "dashed", lwd = line.width)

# pi 0.9
hist(x = save.9[4,], breaks = c(0:10)/10, xlab = expression(hat(pi)), main = expression(paste(pi, "=0.9, ", mu, "=1")))
abline(v = 0.9, lty = "dotted", lwd = line.width)
abline(v = mean(save.9[4,]), lty = "dashed", lwd = line.width)
# dev.off()  # Create plot for book


# Increasing sample size for difficult cases.  
save.9.100 <- sim.zip(n = 100, pi = 0.9, mu = 1, sets = 1000, seed = 927849027)
save.9.500 <- sim.zip(n = 500, pi = 0.9, mu = 1, sets = 1000, seed = 12674726)
save.0.100 <- sim.zip(n = 100, pi = 0, mu = 1, sets = 1000, seed = 67728891)
save.0.500 <- sim.zip(n = 500, pi = 0, mu = 1, sets = 1000, seed = 2356432)



# Trying different combinations of pi and mu that have the same overall mean count.  

sim.zip(n = 35, pi = 0.2, mu = 5/4, sets = 1000, seed = 29950288)
sim.zip(n = 35, pi = 0.5, mu = 2, sets = 1000, seed = 884377129)
sim.zip(n = 35, pi = 0.8, mu = 5, sets = 1000, seed = 448423156)

# Values from Beetle Egg 21C
sim.zip(n = 35, pi = 0.38, mu = 5.14, sets = 1000, seed = 1200201)


