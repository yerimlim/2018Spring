#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 7 Feb 13                                                    #
# PURPOSE: Model Averaging techniques on Placekick data             #
#                                                                   #
# NOTES:                                                            #
#####################################################################

placekick <- read.table(file = "C:\\data\\Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)

########################################################################################
# Model Averaging with glmulti()

# Unfortinately, have to deactivate a system variable in order to get rJava to work. 
# See http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
# True for 32 bit as well
if (Sys.getenv("JAVA_HOME")!= "")
 Sys.setenv(JAVA_HOME = "")
#
library(glmulti)

# Search the model space
search.1.bic <- glmulti(y = good ~ ., data = placekick, fitfunction = "glm", 
             level = 1, method = "h", crit = "bic", family = binomial(link = "logit"))
# Look at best model
print(search.1.bic)
# Look at top 6 models
head(weightable(search.1.bic))
# Plot of model probabilities for top 100 models

x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure5.2color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(search.1.bic, type = "w")
#dev.off()

# Analysis of parameter estimates
parms <- coef(search.1.bic)
# Renaming columns to fit in book output display
colnames(parms) <- c("Estimate", "Variance", "n.Models", "Probability", "95%CI +/-")
round(parms, digits = 3)
# Reorder parameters in decreasing order of probability
parms.ord <- parms[order(parms[,4], decreasing = TRUE),]
# Confidence intervals for parameters (coef() contains a column with the confidence interval add-ons)
ci.parms <- cbind(lower = parms.ord[,1] - parms.ord[,5], upper = parms.ord[,1] + parms.ord[,5])
round(cbind(parms.ord[,1], ci.parms), digits = 3)
# Expressed as odds ratios
round(exp(cbind(OR = parms.ord[,1], ci.parms))[-1,], digits = 2)

# CI for OR for 10-unit decrease in distance
# (Multiplying by negative distance causes confidence interval labels to be backwards)
round(exp(-10*c(OR = parms.ord[2,1], ci.parms[2,])), digits = 2)


# Get out predicted values (linear predictor scale), and get standard errors
preds <- predict(search.1.bic, se.fit = TRUE)
preds.av <- t(preds$averages)
preds.var <- preds$variability
# Confidence intervals on linear predictor scale; 
# second column of $variability contains needed calculations
preds.ci <- cbind(preds.av - preds.var[,2], preds.av + preds.var[,2])

#Transform to probability scale and print out a few rows.
pred.prob <- exp(preds.av)/(1 + exp(preds.av))
pred.prob.ci <- exp(preds.ci)/(1 + exp(preds.ci))
colnames(pred.prob.ci) <- c("lower", "upper")

round(head(cbind(pred.prob, pred.prob.ci)), digits = 3)

# Comparison to fitting best model only, good ~ 1 + distance + PAT

best.fit <- glm(formula = good ~ distance + PAT, data = placekick, family = binomial(link = "logit"))
linear.pred <- predict(object = best.fit, newdata = placekick, type = "link", se = TRUE)
pred.best <- exp(linear.pred$fit) / (1 + exp(linear.pred$fit)) 
alpha <- 0.05
linear.ci <- cbind(linear.pred$fit + qnorm(p = c(alpha/2))*linear.pred$se,
          linear.pred$fit + qnorm(p = c(1 - alpha/2))*linear.pred$se)
pred.best.ci <- exp(linear.ci)/(1+exp(linear.ci))
round(head(cbind(pred.best, lower = pred.best.ci[,1], upper = pred.best.ci[,2])), digits = 3)

#####################################################################################
# Model averaging using BMA
# Note: Can do BIC only using bic.glm()
# bic.glm from the BMA package uses a more computationally efficient algorithm than glmulti().
# It has options that reduce number of models estimated and speed up computations:
#  occam.window = TRUE (default) is a argument that can quickly identify some models that
#   will have very low probability and eliminate them from the candidate set without 
#   fitting them.
#  OR = 20 (default) is the probability ratio for excluding models using occam.window. If a 
#   model has probability less than 1/OR times the best model's probability, it is not 
#   considered. It is also used following model evaluations to select the set of models 
#   upon which the model probabilities will be based. The default of 20 can produce very 
#   small sets. Larger values let in models with smaller probabilities.
#  See four examples below. Computational time is not much of an issue here with only 8 variables.

library(BMA)

# Using the window to select models with OR = 20 (defaults) 
search.bma.OR <- bic.glm(f = good ~., glm.family = "binomial", data = placekick[,-10])
summary(search.bma.OR)
# NOT Using the window to select models with OR = 20 
search.bma.NOR <- bic.glm(f = good ~., glm.family = "binomial", data = placekick[,-10], occam.window = FALSE)
summary(search.bma.NOR)
# Using the window to select models with OR = 50 
search.bma.OR50 <- bic.glm(f = good ~., glm.family = "binomial", data = placekick[,-10], OR = 50)
summary(search.bma.OR50)
# NOT Using the window to select models with OR = 50 
search.bma.NOR50 <- bic.glm(f = good ~., glm.family = "binomial", data = placekick[,-10], OR = 50, occam.window = FALSE)
summary(search.bma.NOR50)

# Note: In this problem we suggest a higher OR value, because the best model has probability of about 0.66 in full 
# enumeration. 1/20 of this is still 0.033, and if there are several models betweeo .01 and .03, these could account 
# for a notable proportion of the probability. 


aa <- bic.glm(f = good ~., glm.family = "binomial", data = placekick[,-10], OR = 80)
summary(aa)

# Confidence intervals for the parameters. Can be converted into confidence intervals for 
# the corresponding odds ratios by exponentiating.

bhat.bma <- aa$postmean
aa$postsd
alpha <- 0.05
ci.param <- cbind(bhat.bma + qnorm(p = alpha/2) * aa$postsd,
         bhat.bma + qnorm(p = 1 - alpha/2) * aa$postsd)
# Parameter estimates and confidence intervals
round(cbind(bhat.bma, ci.param), digits = 3)
# Odds ratios from each variable and confidence intervals
round(exp(cbind(bhat.bma, ci.param))[-1,], digits = 2)

 
