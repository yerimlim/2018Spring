#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 5-20-13                                                     #
# PURPOSE: Examine models for placekicking data more closely        #
#                                                                   #
# NOTES:                                                            # 
#####################################################################
options(width = 60)  # Formatting for book - 60 characters per line

placekick <- read.table(file = "C:\\data\\placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)

#####################################################################
# Examine the binomial form of the data and re-fit the model

# Find the observed proportion of successes at each distance
w <- aggregate(formula = good ~ distance, data = placekick, FUN = sum)
n <- aggregate(formula = good ~ distance, data = placekick, FUN = length)
w.n <- data.frame(distance = w$distance, success = w$good, trials = n$good, prop = round(w$good/n$good,4))
head(w.n)
tail(w.n)

mod.fit.bin <- glm(formula = success/trials ~ distance, weights = trials, family = binomial(link = logit), data = w.n)
summary(mod.fit.bin)

# Show how to find the hat matrix using matrix algebra
X <- model.matrix(mod.fit.bin)
V <- diag(mod.fit.bin$weights)
# mod.fit.bin$weights[1]  # n*hat(pi)*(1-hat(pi))
# w.n$trials[1]*mod.fit.bin$fitted.values[1]*(1-mod.fit.bin$fitted.values[1])
H <- sqrt(V)%*%X%*%solve(t(X)%*%V%*%X)%*%t(X)%*%sqrt(V)
head(diag(H))
head(hatvalues(mod.fit.bin))  # Matches



#####################################################################
# Create plot with fit overlaid onto confidence intervals for each distance
pi.hat <- predict(mod.fit.bin, type = "response")
p.res <- residuals(mod.fit.bin, type = "pearson")
s.res <- rstandard(mod.fit.bin, type = "pearson")
lin.pred <- mod.fit.bin$linear.predictors
w.n <- data.frame(w.n, pi.hat, p.res, s.res, lin.pred)
round(head(w.n), digits = 3)

library(package = binom)

wilson <- matrix(NA, nrow = nrow(w.n), ncol = 2)
for(i in c(1:nrow(w.n))){
 wil.i <- binom.confint(x = w.n$success[i], n = w.n$trials[i], conf.level = .95, methods = "wilson")[c(5,6)]
 wilson[i,] <- as.matrix(wil.i, nrow = 1)
}

x11()
# Overlay estimated curve onto plot with points and 95% confidence limits.
col.miss <- ifelse((wilson[,1] < w.n$pi.hat) & (w.n$pi.hat < wilson[,2]), yes = "black", no = "red")
plot(x = w.n$distance, y = w.n$prop, xlab = "Distance", ylab = "Estimated probability of Success", 
   col = col.miss, main = "Plot of estimated fit and observed proportions with 95% Wilson CI")
# Put estimated logistic regression model on the plot
curve(expr = predict(object = mod.fit.bin, newdata = data.frame(distance = x), type = "response"), 
   col = "blue", add = TRUE, xlim = c(18, 66))
segments(x0 = w.n$distance, y0 = wilson[,1], y1 = wilson[,2], lty = "dotted", col = col.miss)


####################################################################
# Residual plots
x11(height = 7, width = 13, pointsize=20)
# pdf(file = "c:\\figures\\Figure5.3color.pdf", width = 11, height = 6, colormodel = "cmyk", pointsize = 20)   # Create plot for book

par(mfrow = c(1,3))

# Standardized Pearson residual vs X plot
plot(x = w.n$distance, y = w.n$s.res, xlab = "Distance", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. \n X")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ distance, data = w.n, weights = trials)
# Make sure that loess estimates are ordered by "X" for the plots, so that they are displayed properly
order.dist <- order(w.n$distance)
lines(x = w.n$distance[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)

# Standardized Pearson residual vs pi plot
plot(x = w.n$pi.hat, y = w.n$s.res, xlab = "Estimated probability of success", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. \n pi.hat")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ pi.hat, data = w.n, weights = trials)
# Make sure that loess estimates are ordered by "X" for the plots, so that they are displayed properly
order.pi.hat <- order(w.n$pi.hat)
lines(x = w.n$pi.hat[order.pi.hat], y = predict(smooth.stand)[order.pi.hat], lty = "solid", col = "red", lwd = 1)

# Standardized Pearson residual vs linear predictor plot
plot(x = w.n$lin.pred, y = w.n$s.res, xlab = "Linear predictor", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. \n Linear predictor")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ lin.pred, data = w.n, weights = trials)
# Make sure that loess estimates are ordered by "X" for the plots, so that they are displayed properly
order.lin.pred <- order(w.n$lin.pred)
lines(x = w.n$lin.pred[order.lin.pred], y = predict(smooth.stand)[order.lin.pred], lty = "solid", col = "red", lwd = 1)
# dev.off()  # Create plot for book


# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure5.3BW.pdf", width = 11, height = 6, colormodel = "cmyk", pointsize = 20)   # Create plot for book
par(mfrow = c(1,3))

# Standardized Pearson residual vs X plot
plot(x = w.n$distance, y = w.n$s.res, xlab = "Distance", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. \n X")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "black")
lines(x = w.n$distance[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "black", lwd = 1)

# Standardized Pearson residual vs pi plot
plot(x = w.n$pi.hat, y = w.n$s.res, xlab = "Estimated probability of success", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. \n pi.hat")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "black")
lines(x = w.n$pi.hat[order.pi.hat], y = predict(smooth.stand)[order.pi.hat], lty = "solid", col = "black", lwd = 1)

# Standardized Pearson residual vs linear predictor plot
plot(x = w.n$lin.pred, y = w.n$s.res, xlab = "Linear predictor", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. \n Linear predictor")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "black")
lines(x = w.n$lin.pred[order.lin.pred], y = predict(smooth.stand)[order.lin.pred], lty = "solid", col = "black", lwd = 1)
# dev.off()  # Create plot for book


##############################################################
# Goodness-of-Fit Tests
# 
# First the Deviance/DF
rdev <- mod.fit.bin$deviance 
dfr <- mod.fit.bin$df.residual 
ddf <- rdev/dfr 
thresh2 <- 1 + 2*sqrt(2/dfr) 
thresh3 <- 1 + 3*sqrt(2/dfr) 
c(rdev, dfr, ddf, thresh2, thresh3)

sum(p.res^2)
#
# Three goodness-of-fit tests are shown below: Hosmer and Lemeshow, Osius-Rojek, and Stukel. 
#  Each is contained in a separate function that assumes that a glm-class object 
#  has been created for the logistic regression. These functions are wrapped up into one 
#  function, AllGOFTests.R, that must be sourced.  All expect the model fit from glm to 
#  be in EVP form (there is no internal aggregation in the functions). Also, the 
#  OsiusRojek and Stukel expect that all interactions are actually listed as named 
#  individual variables (i.e. that they are included in the model as cross-product 
#  variables, like "X1:X2", rather than implicit interactions of other variables in the model.

# Before running, source the script:

source("C:\\data\\AllGOFTests.R")  
HL <- HLTest(obj = mod.fit.bin, g = 10)
# Print out observed and expected counts in bins
cbind(HL$observed, round(HL$expect, digits = 1))
HL
# Pearson residuals for each group
round(HL$pear, digits = 1) 


o.r.test(obj = mod.fit.bin)

stukel.test(obj = mod.fit.bin)

###################################################################
# Influence analysis
# Using our glmInflDiag(mod.fit) function contained in the glmDiagnostics.R program
# Assuming it resides in current working directory; otherwise add path

source("C:\\data\\glmDiagnostics.R")
save.diag <- glmInflDiag(mod.fit = mod.fit.bin, print.output = TRUE, which.plots = c(1,2))
round(head(save.diag, n = 3), digits = 2)
names(save.diag)
s.res[c(1:3, 34)]  # Standardized residuals

