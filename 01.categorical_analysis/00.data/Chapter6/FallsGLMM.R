#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 08-26-2013                                                  #
# PURPOSE: Analysis of head impact for falls using GLMM             #
#                                                                   #
# NOTES:                                                            #
#####################################################################
options(width = 60) #Formatting for book - 60 characters per line

#####################################################################
# Read the data from original file

# Read the data from the subsetted file
fall.head <- read.csv("C:\\Data\\FallHead.csv")
head(fall.head)

fallFreq <- table(fall.head$resident)
# Plot of fall frequencies by resident.
x11(height = 5, width = 8)
# pdf(file = "c:\\figures\\Figure6.5BW.pdf", width = 8, height = 5, colormodel = "cmyk")   # Create plot for book
plot(x = as.numeric(names(fallFreq)), y = fallFreq, xlab = "Resident", ylab = "Fall Frequency", type = "h", lwd = 2)
# dev.off()  # Create plot for book


# Summary table of falls by initial direction and head impact
head.dir <- xtabs(formula = ~ initial + head, data = fall.head)
head.dir

##############################################################
# Model fitting

library(lme4)

# Estimate with varying numbers of quadrature points. 
# Variance components contained in summary()$varcor

mod.glmm.1 <- glmer(formula = head ~ initial + (1|resident), nAGQ = 1, data = fall.head, family = binomial(link = "logit"))
summary(mod.glmm.1)$varcor
summary(mod.glmm.1)$varcor[[1]][1,1]
mod.glmm.2 <- glmer(formula = head ~ initial + (1|resident), nAGQ = 2, data = fall.head, family = binomial(link = "logit"))
summary(mod.glmm.2)$varcor
mod.glmm.3 <- glmer(formula = head ~ initial + (1|resident), nAGQ = 3, data = fall.head, family = binomial(link = "logit"))
summary(mod.glmm.3)$varcor
mod.glmm.5 <- glmer(formula = head ~ initial + (1|resident), nAGQ = 5, data = fall.head, family = binomial(link = "logit"))
summary(mod.glmm.5)$varcor
mod.glmm.10 <- glmer(formula = head ~ initial + (1|resident), nAGQ = 10, data = fall.head, family = binomial(link = "logit"))
summary(mod.glmm.10)$varcor

# Estimates are nearly identical starting with nAQG = 5, and barely different from nAGQ = 2.
#  Only nAGQ = 1 are different, and they are VERY different.
# Will use nAGQ = 5 going forward.

# Explore the object contents
slotNames(mod.glmm.5)

# Show summary output
summ <- summary(mod.glmm.5)
summ
names(summ)
methods(class = "merMod")
# Estimates of fixed-effect parameters
fixef(mod.glmm.5)
# Conditional Modes (b_i) for random effects listed **BY resident ID**, not in data order.
head(ranef(mod.glmm.5)$resident)
# Show that there is one element per resident
nrow(ranef(mod.glmm.5)$resident)
# coef = Fixed + Random effects. Listed **BY resident ID**, not in data order.
head(coef(mod.glmm.5)$resident)
mod.glmm.5@flist$resident[1:15]  # List of residents to help map random effects to observations
# The matrix of explanatory variables: Intercept and columns for levels 2,3,4
head(mod.glmm.5@frame)
# Predicted logit for each observation
logit.i <- round(predict(object = mod.glmm.5, newdata = fall.head, REform = NULL, type = "link"), digits = 3)
# Estimated mean logit for each fall direction, by observation
logit.avg <- round(predict(object = mod.glmm.5, newdata = fall.head, REform = NA, type = "link"), digits = 3)
# Predicted probability for each observation
pi.hat.i <- round(predict(object = mod.glmm.5, newdata = fall.head, REform = NULL, type = "response"), digits = 3)
# Estimated average probability for each fall direction, by observation
pi.hat.avg <- round(predict(object = mod.glmm.5, newdata = fall.head, REform = NA, type = "response"), digits = 3)
# Conditional Modes listed in order of the original data (they are currently ordered by resident)
ranefs <- round(ranef(mod.glmm.5)$resident[fall.head$resident,], digits = 3)
# Print of all predictions and mean estimates together
head(cbind(fall.head, ranefs, logit.i, logit.avg, pi.hat.i, pi.hat.avg))

# Just a check: differences due to rounding error only
summary(plogis(logit.avg) - pi.hat.avg)
summary(plogis(logit.i) - pi.hat.i)

# Plot of what a set of probabilities from this model might look like
# Generate a set of random effects for each resident using model estimated variance component
reffs.norm <- rnorm(n = nrow(ranef(mod.glmm.5)$resident), mean = 0, sd = sqrt(summary(mod.glmm.5)$varcor[[1]][1,1]))
# Create logit and probabilities from estimated means and random effects applied to each resident's falls
logit <- logit.avg + reffs.norm[fall.head$resident]
probs <- plogis(logit)
# Plot Estimated probabilities of head impact from sample random effect values (first) 
#  and from a new set of random effect values generated from the estimated normal distribution (second). 
#  The difference in spread within each group is due to a phenomenon called "shrinkage" (****).
x11(height = 4, width = 6)
stripchart(x = probs ~ fall.head$initial, vertical = TRUE, method = "jitter", pch = 1, cex = 0.5, col = "red", xlab = "Initial Fall Direction", ylab = "Estimated P(Head Impact)")
stripchart(x = pi.hat.avg ~ fall.head$initial, vertical = TRUE, pch = 19, add = TRUE, ylab = "Initial Fall Direction", xlab = "Estimated P(Head Impact)")
x11(height = 4, width = 6)
stripchart(x = pi.hat.i ~ fall.head$initial, vertical = TRUE, method = "jitter", pch = 1, cex = 0.5, col = "red", xlab = "Initial Fall Direction", ylab = "Estimated P(Head Impact)")
stripchart(x = pi.hat.avg ~ fall.head$initial, vertical = TRUE, pch = 19, add = TRUE, ylab = "Initial Fall Direction", xlab = "Estimated P(Head Impact)")


#######################################################################
# Inference on fixed effects

# Because of grouping, this is the wrong thing to do. Showing it for comparison.
chisq.test(head.dir)

# Unfortunately, the anova() function does not work to test the significance of a single model's terms. 
#  LRT must be conducted by comparing pairs of fitted models. 

lrt <- drop1(mod.glmm.5, test = "Chisq")
lrt

# Wald test from Anova in car package. Worst test to use.
library(car)
Anova(mod.glmm.5)

#####
# Parametric Bootstrap

# Prepare for Parametric bootstrap. Calculate the LR statistic for test.
names(lrt)
orig.LRT <- lrt$LRT[2]  # Saves LR Test statistic

# Fit Null model
mod.glmm0 <- glmer(formula = head ~ (1|resident), nAGQ = 5, data = fall.head, family = "binomial")
###### Doing parametric bootstrap with bootMer. Doesn't work with nAGQ>1!
# Define function to compute LRT
###LR.stat <- function(fit){
### m12 <- update(fit, formula. = .~initial + (1|resident))
### anova(fit,m12)$Chisq[2]
###}
# Test on original data. Seems to work: Produces correct LRT stat.
###LR.stat(mod.glmm0)

# Doesn't work with mod.glmm0 using nAGQ = 5 Using nAGQ = 1, it produced bizarre results!
### LRT.boot <- bootMer(x = mod.glmm0, FUN = LR.stat, seed = 16581, nsim = 1000)
### library(boot)
### names(LRT.boot)
# $t = statistics on resample, $t0 = statistic on original sample, $R = nsims
### summary(LRT.boot$t)
### pval <- sum(LRT.boot$t >= LRT.boot$t0)/LRT.boot$R
### pval

# Plot results
### x11(width = 7, height = 5)
### hist(x = LRT.boot$t, breaks = 25, freq = FALSE, xlab = "LRT Statistics", main = NULL, xlim = c(0, 2 + round(max(LRT.boot$t, LRT.boot$t0))))
### abline(v = LRT.boot$t0, col = "red", lwd = 2)
### curve(expr = dchisq(x = x, df = 3), add = TRUE, from = 0, to = 18, col = "blue", lwd = 2)

# Find distribution of LR Test Statistic by Parametric bootstrap simulation
# Generate data from H0 model
# Fit Null and Alternative Models
# Compute test statistic (LRT Stat) for each simulation
# Compute p-value as % of simulations with larger LRT than original.
sims <- 1000
# simulate() generates new responses for the existing explanatory variables and grouping factors.
simfix.h0 <- simulate(mod.glmm0, nsim = sims, seed = 9245982)
# Fit Model and compute test statistic
LRT0 <- numeric(length = sims)
for (i in 1:sims){
 m1 <- glmer(formula = simfix.h0[,i] ~ initial + (1|resident), nAGQ = 5, data = fall.head, family = "binomial")
 LRT0[i] <- drop1(m1, test = "Chisq")$LRT[2]
}
summary(LRT0)
pval <- mean(LRT0 >= orig.LRT)
pval

# Plot results
x11(width = 7, height = 5)
# pdf(file = "c:\\figures\\Figure6.6color.pdf", width = 7, height = 5, colormodel = "cmyk")   # Create plot for book
hist(x = LRT0, breaks = 25, freq = FALSE, xlab = "LRT Statistics", main = NULL, xlim = c(0, 2+round(max(LRT0, orig.LRT))))
abline(v = orig.LRT, col = "red", lwd = 2)
curve(expr = dchisq(x = x, df = 3), add = TRUE, from = 0, to = 18, col = "blue", lwd = 2)
# dev.off()  # Create plot for book

# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure6.6BW.pdf", width = 7, height = 5, colormodel = "cmyk")   # Create plot for book
hist(x = LRT0, breaks = 25, freq = FALSE, xlab = "LRT Statistics", main = NULL, xlim = c(0, 2+round(max(LRT0, orig.LRT))))
abline(v = orig.LRT, col = "black", lwd = 2)
curve(expr = dchisq(x = x, df = 3), add = TRUE, from = 0, to = 18, col = "black", lwd = 2)
# dev.off()  # Create plot for book


# Pairwise comparisons of individual levels (Wald)

library(multcomp)

K <- rbind("D-B" = c(0, 1, 0, 0),
      "F-B" = c(0, 0, 1, 0),
      "S-B" = c(0, 0, 0, 1),
      "F-D" = c(0, -1, 1, 0),
      "S-D" = c(0, -1, 0, 1),
      "S-F" = c(0, 0, -1, 1))

pw.comps <- glht(mod.glmm.5, linfct = K)
# Wald Test of global hypothesis (all contrasts = 0) 
summary(pw.comps, test = Chisqtest())
# Tests using Individual error rates = 0.95
summary(pw.comps, test = adjusted("none"))
# Tests using Familywise error rates = 0.95
summary(pw.comps)
# Confidence intervals using Individual error rates = 0.95
ci.logit.I <- confint(pw.comps, calpha = qnorm(0.975))
round(exp(ci.logit.I$confint),2)
# Confidence intervals using Familywise error rates = 0.95
ci.logit.F <- confint(pw.comps, level = 0.95)
round(exp(ci.logit.F$confint), 2)

# Parametric bootstrap intervals
# Using "bootstrap t" approach to yield good intervals (Davison and Hinkley 1997)
# Basic idea is to replace Z in standard confidence interval formula,
#    estimate +/- Z(1-alpha/2) * Standard error,
#  with a simulated value Z*.
# So need to compute values of Z* = (estimate - parameter)/Standard error in each simulation.
# *** The trick is that the "parameter" in the simulation model 
#   is the value of the estimate from the original data.

# Showing different simulation approach, since only one model needs to be fit.
# "refit()" supposedly fits model faster than creating a new "glmer()" call

####################### Warning: the simulate() method for mer-class objects is a bit limited! 
#######################  You can only specify the family name, not also the link, and it must be in quotes.
######################mod.glmm.5a <- glmer(formula = head ~ initial + (1|resident), nAGQ = 5, data = fall.head, family = "binomial")

# Simulate one data set from model "modsim" and refit model (modfit) in one step.
#  (These can be the same model, as they will be here for confidence intervals,
#  but could also be different models, as for hypothesis test)
# Then compute covariance matrix for whole set of functions
# Finally compute Z as "zz"

sims = 1000
simfull <- simulate(mod.glmm.5, nsim = sims, seed = 86824165)
# Create matrix to store Z-statistics for each resample
zz <- matrix(data = NA, nrow = sims, ncol = nrow(K))

# Fit Model and compute test statistic
for (i in c(1:sims)){
 m1 <- glmer(formula = simfull[,i] ~ initial + (1|resident), nAGQ = 5, data = fall.head, family = binomial)
 var.pw <- diag(K %*% vcov(m1) %*% t(K))
 zz[i,] <- (K %*% (fixef(m1) - fixef(mod.glmm.5)))/sqrt(var.pw)
}
# Reduce results to only those cases that provided estimates
zz <- na.omit(zz)
nz <- nrow(zz)
summary(zz)

# Compute critical values (quantiles) from each column of zz
crits <- apply(X = zz, MARGIN = 2, FUN = function(y){quantile(x = y, probs = c(0.025, 0.975))})
crits  # Compare to Normal 1.96
# Manually compute confidence intervals
estDiffs <- K %*% fixef(mod.glmm.5)
var.eD <- diag(K %*% vcov(mod.glmm.5) %*% t(K))

pw.CI <- cbind(estDiffs, lower = estDiffs - crits[2,]*sqrt(var.eD), upper = estDiffs - crits[1,]*sqrt(var.eD))
# Exponentiating to present as odds ratios for comparisons between initial fall direction
round(exp(pw.CI), 2)


# Automatic confidence intervals for model parameters
# Not so useful here because parameters are differences between levels, 
# but could be helpful in regression settings.
confint(mod.glmm.5, method = "profile", level = 0.95)
confint(mod.glmm.5, method = "Wald", level = 0.95)
confint(mod.glmm.5, method = "boot", level = 0.95) # Does not work!
confint(mod.glmm.1, method = "boot", level = 0.95) # Works!! Why???




#######################################################################
# Inferences on Random Effects

# Test significance of variance component by simulation

# Fit model without random effect (i.e. under H0: variance component = 0)
mod.glm <- glm(formula = head ~ initial, data = fall.head, family = binomial(link = "logit"))
summary(mod.glm)
# Simulate many data sets from this model
# This runs for a few minutes using sims = 1000. Could use fewer and see if p-value is already clear in its message.
sims <- 1000
simmod.h0 <- simulate(mod.glm, nsim = sims, seed = 28662819)
# Fit mixed model to the data and estimate the variance component when it is known to be 0
orig.vc <- summary(mod.glmm.5)$varcor[[1]][1,1]
# Initialize varcomps0 as matrix to preserve NA when estimation fails
varcomps0 <- matrix(data = NA, nrow = sims, ncol = 1)
for (i in c(1:sims)){
 mm <- glmer(formula = simmod.h0[,i] ~ initial + (1|resident), nAGQ = 5, data = fall.head, family = binomial(link = "logit"))
 varcomps0[i,] <- summary(mm)$varcor[[1]][1,1]
}
varcomps0 <- na.omit(varcomps0)
nrow(varcomps0)
summary(varcomps0)
pval <- sum(varcomps0 >= orig.vc)/nrow(varcomps0)
pval

x11(width = 7, height = 5)
# pdf(file = "c:\\figures\\Figure6.7color.pdf", width = 7, height = 5, colormodel = "cmyk")   # Create plot for book
hist(x = varcomps0, breaks = 25, freq = FALSE, xlab = "Variance component value", main = NULL)
abline(v = orig.vc, col = "red", lwd = 2)
# dev.off()  # Create plot for book

# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure6.7BW.pdf", width = 7, height = 5, colormodel = "cmyk")   # Create plot for book
hist(x = varcomps0, breaks = 25, freq = FALSE, xlab = "Variance component value", main = NULL)
abline(v = orig.vc, col = "black", lwd = 2)
# dev.off()  # Create plot for book


# Compare to large-sample LR test,

LRstat.vc <- deviance(mod.glm) - deviance(mod.glmm.5)
# p-value 
(1 - pchisq(LRstat.vc, df = 1))/2

# Confidence interval for variance component by parametric bootstrap
# Note that LR confidence interval was produced earlier by confint(, method = "profile)
# Simulate data from model that we are using for fit.
# Note: this could be integrated into the precious parametric bootstrap for fixed effects. 
# Both use simulations from the same model (contained above in the object "simfull")
# We recreate the object here
 
sims <- 1000
varcomps1 <- matrix(data = NA, nrow = sims, ncol = 1)
simfull <- simulate(mod.glmm.5, nsim = sims, seed = 86824165)
for(i in c(1:sims)){
 m1 <- glmer(formula = simfull[,i] ~ initial + (1|resident), nAGQ = 5, data = fall.head, family = binomial)
 varcomps1[i,] <- summary(m1)$varcor[[1]][1,1]
}

summary(varcomps1)

# Percentile confidence interval is just the 1 - alpha/2 quantiles
quantile(x = varcomps1, probs = c(0.025,0.975), na.rm = TRUE)

# BCa interval requires two extra quantities, ahat and z0:
z0 <- qnorm(p = mean(varcomps1 <= orig.vc))
ahat <- sum((varcomps1 - mean(varcomps1))^3) / (6*(sum((varcomps1 - mean(varcomps1))^2))^(3/2)) 
zsums <- z0 + qnorm(p = c(0.025, 0.975))
p1 <- pnorm(zsums/(1 - ahat*zsums) + z0) 

quantile(x = varcomps1, probs = p1, na.rm = TRUE)

# Alternative LR Confidence interval. 
# Note: Variance components are listed as "sig" parameters 
confint(object = mod.glmm.5, level = 0.95, method = "profile")