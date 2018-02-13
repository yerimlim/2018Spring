#####################################################################
# NAME:  Tom Loughin and Chris Bilder                               #
# DATE:  01-23-12                                                   #
# PURPOSE: Use Poisson regression to compare means in Ecuadorean    #
#          bird count data                                          #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Enter the data

alldata <- read.table(file = "C:\\data\\BirdCounts.csv", sep = ",", header = TRUE)
head(alldata)

#####################################################################
# Model using regular likelihood

# Fit Poisson Regression and test whether all means are equal
M1 <- glm(formula = Birds ~ Loc, family = poisson(link = "log"), data = alldata)
summary(M1)
library(car)
Anova(M1, test = "Chisq") #Also could use Anova() from car package

# Get predicted means and CIs
pred.data <- data.frame(Loc = c("ForA", "ForB", "Frag", "Edge", "PasA", "PasB"))
means <- predict(object = M1, newdata = pred.data, type = "link", se.fit = TRUE)
alpha <- 0.05

# Wald CI for log means
lower.logmean <- means$fit + qnorm(alpha/2)*means$se.fit
upper.logmean <- means$fit + qnorm(1 - alpha/2)*means$se.fit

# Combine means and confidence intervals in count scale
mean.wald.ci <- data.frame(pred.data, round(cbind(exp(means$fit), exp(lower.logmean), exp(upper.logmean)), digits = 2))
colnames(mean.wald.ci) <- c("Location", "Mean", "Lower", "Upper")
mean.wald.ci

######################################################################
# Get LR intervals for means.  Each log-mean is a combination of intercept and a parameter.
#   Want means ordered by level of forestation: Forests, Fragment, Edge, Pastures 
#  
library(mcprofile)
K <- matrix(data = c(1, 1, 0, 0, 0, 0,
                     1, 0, 1, 0, 0, 0,
                     1, 0, 0, 1, 0, 0,
                     1, 0, 0, 0, 0, 0,
                     1, 0, 0, 0, 1, 0,
                     1, 0, 0, 0, 0, 1), nrow = 6, ncol = 6, byrow = TRUE)
K
# Profile LR
linear.combo <- mcprofile(object = M1, CM = K)
ci.log.mu <- confint(object = linear.combo, level = 0.95, adjust = "none")

mean.LR.ci1 <- data.frame(Loc = pred.data, Estimate = exp(ci.log.mu$estimate), Lower = exp(ci.log.mu$confint[,1]), Upper = exp(ci.log.mu$confint[,2]))
mean.LR.ci1

mean.LR.ci1$Loc2 <- factor(mean.LR.ci1$Loc, levels = levels(mean.LR.ci1$Loc)[c(2,3,4,1,5,6)])
# StripChart in color
x11(width = 7, height = 5, pointsize = 12)   # Use dev.new() for non-Windows based computers
# pdf(file = "c:\\figures\\Figure4.7color.pdf", width = 7, height = 5, colormodel = "cmyk")   # Create plot for book
stripchart(Lower ~ Loc2, data = mean.LR.ci1, vertical = FALSE, xlim = c(20,150), col = "red", pch = "(", main = "", xlab = "Bird Count", ylab = "Location")
stripchart(Upper ~ Loc2, data = mean.LR.ci1, vertical = FALSE, col = "red", pch = ")", add = TRUE)
stripchart(Estimate ~ Loc2, data = mean.LR.ci1, vertical = FALSE, col = "red", pch = "+", add = TRUE)
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
abline(v = mean(alldata$Birds), col = "darkblue", lwd = 4)
# dev.off()  # Create plot for book

# StripChart in B/W
x11(width = 7, height = 5, pointsize = 12)
# pdf(file = "c:\\figures\\Figure4.7BW.pdf", width = 7, height = 5, colormodel = "cmyk")   # Create plot for book
stripchart(Lower ~ Loc2, data = mean.LR.ci1, vertical = FALSE, xlim = c(20,150), pch = "(", main = "", xlab = "Bird Count", ylab = "Location")
stripchart(Upper ~ Loc2, data = mean.LR.ci1, vertical = FALSE, pch = ")", add = TRUE)
stripchart(Estimate ~ Loc2, data = mean.LR.ci1, vertical = FALSE, pch = "+", add = TRUE)
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
abline(v = mean(alldata$Birds), lwd = 4)
# dev.off()  # Create plot for book

######################################################################
# Alternative parameterization for model to get LR intervals for means
# "-1" in formula removes intercept and causes model to estimate log-means directly 
M1.rp <- glm(formula = Birds~ Loc - 1, family = poisson(link = "log"), data = alldata)
LR.ci <- exp(confint(M1.rp))
mean.LR.ci2 <- data.frame(Estimate = exp(M1.rp$coefficients), Lower = LR.ci[,1], Upper = LR.ci[,2])
mean.LR.ci2

#####################################################################
# Plots based on Wald Intervals

  #Redefining location group factor so that plot does not alphabetize
  mean.wald.ci$Loc2 <- factor(mean.wald.ci$Location, levels = c("ForA", "ForB", "Frag", "Edge", "PasA", "PasB"))
  mean.wald.ci

  # StripChart in color
  x11(width = 7, height = 5, pointsize = 12)
  # NOTE: vertical = FALSE shows intervals horizontally
  stripchart(Lower ~ Loc2, data = mean.wald.ci, vertical = FALSE, xlim = c(20,150), col = "red", pch = "(", main = "", xlab = "Bird Count", ylab = "Location")
  stripchart(Upper ~ Loc2, data = mean.wald.ci, vertical = FALSE, col = "red", pch = ")", add = TRUE)
  stripchart(Mean ~ Loc2, data = mean.wald.ci, vertical = FALSE, col = "red", pch = "+", add = TRUE)
  grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
  abline(v = mean(alldata$Birds), col = "darkblue", lwd = 4)

  # StripChart in B/W
  x11(width = 7, height = 5, pointsize = 12)
  stripchart(Lower ~ Loc2, data = mean.wald.ci, vertical = FALSE, xlim = c(20,150), pch = "(", main = "", xlab = "Bird Count", ylab = "Location")
  stripchart(Upper ~ Loc2, data = mean.wald.ci, vertical = FALSE, pch = ")", add = TRUE)
  stripchart(Mean ~ Loc2, data = mean.wald.ci, vertical = FALSE, pch = "+", add = TRUE)
  grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
  abline(v = mean(alldata$Birds), lwd = 4)

#####################################################################
# Confidence intervals for linear combinations of parameters

  #Create coefficients for Lin Combos
  contr.mat <- rbind(c(0,.5,.5,0,0,0), c(0,.5,.5,-1,0,0), c(0,.5,.5,0,-.5,-.5), 
                     c(0,0,0,1,0,0), c(0,0,0,1,-.5,-.5), c(0,0,0,0,-.5,-.5))
  rownames(contr.mat) <- c("For-Edge", "For-Frag", "For-Past", "Frag-Edge", "Frag-Past", "Edge-Past")
  contr.mat

# Wald inferences using multcomp package
  library(multcomp)
  loc.test <- glht(model = M1, linfct = contr.mat)

  # Defaults use multiplicity adjustment for simultaneous confidence level
  summary(loc.test)
  exp(confint(loc.test)$confint)
  # Options to get unadjusted (univariate) tests and CIs
  summary(loc.test, test = univariate())
  exp(confint(loc.test, calpha = qnorm(0.975))$confint)

  # Do the same thing with LR in mcprofile
  linear.combo <- mcprofile(object = M1, CM = contr.mat)
  summary(linear.combo)  
  exp(confint(linear.combo)$confint)
  summary(linear.combo, adjust = "none")
  exp(confint(linear.combo, adjust = "none")$confint)

  ################################
  # Using deltaMethod

  library(package = car)
  # My original comparison of (mu2 + mu3)/2 - mu4
  lin.comb <- deltaMethod(object = M1, g = "exp(b0 + b2)/2 + exp(b0 + b3)/2 - exp(b0 + b4)",
    parameterNames = c("b0", "b2", "b3", "b4", "b5", "b6"))  #Remember there is no beta1
  # "For-Frag" comparison as done in the example
  lin.comb <- deltaMethod(object = M1, g = "(exp(b0 + b2)*exp(b0 + b3))^0.5 / exp(b0 + b4)",
    parameterNames = c("b0", "b2", "b3", "b4", "b5", "b6"))  #Remember there is no beta1
  names(lin.comb)
  lin.comb
  z0 <- lin.comb$Estimate/lin.comb$SE
  pvalue <- 2*(1-pnorm(q = abs(z0)))
  data.frame(z0, pvalue)
  lin.comb$Estimate + qnorm(p=0.975)*lin.comb$SE
  
  ################################
  # Wald CIs by manual computation

  # Get out coefficients and variances
  beta <- matrix(coef(M1), ncol = 1)
  v.beta <- vcov(M1)
  # Estimate Lin Combos and standard errors as matrix computations
  log.contrasts <- contr.mat %*% beta
  SElog.contrasts <- matrix(sqrt(diag(contr.mat %*% v.beta %*% t(contr.mat))), ncol = 1)
  # Compute confidence intervals in linear predictor scale
  alpha <- 0.05
  lower.log <- log.contrasts + qnorm(alpha/2)*SElog.contrasts
  upper.log <- log.contrasts + qnorm(1-alpha/2)*SElog.contrasts
  # Combine Lin Combo coefficients, estimates of contrasts, and confidence intervals in mean scale
  wald.ci <- round(cbind(exp(log.contrasts), exp(lower.log), exp(upper.log)), digits = 2)
  colnames(wald.ci) <- c("Estimate", "Lower", "Upper")
  wald.ci




