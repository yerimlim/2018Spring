#############################################################################
# NAME: Tom Loughin                                                         #
# DATE: 1-10-13                                                             #
# PURPOSE: Simulation of regression to compare linear models with different #
#     numbers of variables.                                                 #
# NOTES:                                                                    #
#############################################################################

# Model is Y~Binomial(1,pi), logit(pi)) = -beta0 + beta1 X1 + beta2 X2 + beta3 X3
# Program starts with beta0 = 0, beta1 = 1, beta2 = 1 so that probabilities are symmetric around 0.5.
# Three variables are measured: x1,x2,x3. All are generated U(0,1)

# We fit 3 models: 
#  1: X1  2: X1, X2  3: X1, X2, X3
# We then compute model-estimated probability values at a small grid of points:
# (.1,.3,.5,.7,.9) for each explanatory variable.
# We compute the bias at each X combo by comparing the estimated vs. true probabilities. 
# We compute the variance of the estimated probabilities at each X combo 
# We compute the mean squared error ( = bias + variance^2) measuring the combined 
#  distance between the predicted and true probabilities.
# WE then summarize these results in plots and with summary statistics

set.seed(294830493)

reps <- 200 # Number of data sets
N <- 20  # Sample size
beta0 <- -1; beta1 <- 1; beta2 <- 1; beta3 <- 0 # Parameter values

# Create test data
test <- expand.grid(x1 = c(.1,.3,.5,.7,.9), x2 = c(.1,.3,.5,.7,.9), x3 = c(.1,.3,.5,.7,.9))

# Create vector of true means
test$logit <- beta0 + beta1*test$x1 + beta2*test$x2 + beta3*test$x3
test$pi <- exp(test$logit) / (1 + exp(test$logit))
summary(test)


# Prepare for looping over reps
counter <- 1
save.pred <- matrix(data = NA, ncol = 3*nrow(test), nrow = reps)
save.se <- matrix(data = NA, ncol = 3*nrow(test), nrow = reps)
save.coef <- matrix(data = NA, ncol = 9, nrow = reps)
save.conv <- matrix(data = NA, ncol = 5, nrow = reps)

# Loop to generate data, analyze, and save results
while(counter <= reps){
# Generating Uniform X's
 x1 <- runif(n = N)
 x2 <- runif(n = N)
 x3 <- runif(n = N)
 
# Generating binary data using true model
 y <- rbinom(n = N, size = 1, prob = exp(beta0 + beta1 * x1 + beta2 *x2 + beta3 * x3)
                   /(1 + exp(beta0 + beta1 * x1 + beta2 *x2 + beta3 * x3)))

# reg* is model-fit object, pred* is list of predicted values over grid for model 
# coef* are the regression parameter estimates (Not currently used in later summaries)
 reg1 <- glm(formula = y ~ x1, family = binomial(link = "logit"))
 pred1 <- predict(reg1, newdata = test[,1:3], type = "response", se.fit = TRUE)
 coef1 <- reg1$coefficients

 reg2 <- glm(formula = y ~ x1 + x2, family = binomial(link = "logit"))
 pred2 <- predict(reg2, newdata = test[,1:3], type = "response", se.fit = TRUE)
 coef2 <- reg2$coefficients
 
 reg3 <- glm(formula = y ~ x1 + x2 + x3, family = binomial(link = "logit"))
 pred3 <- predict(reg3, newdata = test[,1:3], type = "response", se.fit = TRUE)
 coef3 <- reg3$coefficients

# Saving all results into storage objects and incrementing row counter
 save.pred[counter,] <- c(pred1$fit, pred2$fit, pred3$fit)
 save.se[counter,] <- c(pred1$se.fit, pred2$se.fit, pred3$se.fit)
 save.coef[counter,] <- c(coef1, coef2, coef3)
 save.conv[counter,] <- c(counter, reg1$converged, reg2$converged, reg3$converged, counter + reg3$converged)
 counter <- ifelse(test = reg3$converged, yes = counter+1, no = counter)
}

# Estimate bias, variance, and MSE of predictions at each X-combo
mean.pred <- apply(save.pred, MARGIN = 2, FUN = mean)
bias <- mean.pred - rep(test$pi, times = 3) # Bias = (average estimate - truth)
var <- apply(save.pred, MARGIN = 2, FUN = var)
MSE <- bias^2 + var

# Vector of model numbers
model <- rep(c(1,2,3), each = nrow(test))

# Plots
x11(height = 5, width = 5, pointsize = 15)
stripchart(x = bias ~ model, method = "jitter", jitter = .1, vertical = TRUE, pch = 20,
      main = "Prediction bias vs # variables")
abline(h = 0, lty = "solid")

x11(height = 5, width = 5, pointsize = 15)
stripchart(x = var ~ model, method = "jitter", jitter = .1, vertical = TRUE, pch = 20,
      main = "Prediction variance vs # variables")

x11(height = 5, width = 5, pointsize = 15)
stripchart(x = MSE ~ model, method = "jitter", jitter = .1, vertical = TRUE, pch = 20,
      main = "Prediction MSE vs # variables")

# Summary statistics for variances and MSEs for prediction by model
# (Smaller is better)
cat("Average squared bias for x1:    ", mean(bias[which(model == 1)]^2))
cat("Average squared bias for x1,x2:  ", mean(bias[which(model == 2)]^2))
cat("Average squared bias for x1,x2,x3: ", mean(bias[which(model == 3)]^2))
cat("Average variance for x1:    ", mean(var[which(model == 1)]))
cat("Average variance for x1,x2:  ", mean(var[which(model == 2)]))
cat("Average variance for x1,x2,x3: ", mean(var[which(model == 3)]))
cat("Average MSE for x1:    ", mean(MSE[which(model == 1)]))
cat("Average MSE for x1,x2:  ", mean(MSE[which(model == 2)]))
cat("Average MSE for x1,x2,x3: ", mean(MSE[which(model == 3)]))
