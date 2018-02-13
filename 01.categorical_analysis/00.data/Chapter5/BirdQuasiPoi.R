#####################################################################
# NAME: Tom Loughin and Chris Bilder                                #
# DATE: 01-23-12                                                    #
# PURPOSE: Examine overdispersion in Poisson regression on          #
#      Ecuadorean bird count data. Also use quasi-Poisson           #
#      model to adjust for overdispersion                           #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Enter the data

alldata <- read.table(file = "C:\\data\\BirdCounts.csv", sep = ",", header = TRUE)
head(alldata)

#####################################################################
# Model using regular likelihood

# Fit Poisson Regression 
Mpoi <- glm(formula = Birds ~ Loc, family = poisson(link = "log"), data = alldata)
summary(Mpoi)

# Residual plots vs. predicted
pred <- predict(Mpoi, type = "response")
# Standardized Pearson residuals
stand.resid <- rstandard(model = Mpoi, type = "pearson") 

x11(height = 7, width = 12, pointsize = 12)
# pdf(file = "c:\\figures\\Figure5.7color.pdf", width = 10, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book
par(mfrow = c(1,2))

plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardized Pearson residuals",
   main = "Residuals from regular likelihood", ylim = c(-5,5))
abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "red")

#####################################################################
# Model using quasi-likelihood

# Fit Quasi-Poisson model
Mqp <- glm(formula = Birds ~ Loc, family = quasipoisson(link = "log"), data = alldata)
summary(Mqp)

# Demonstrate calculation of dispersion parameter
pearson <- residuals(Mpoi, type = "pearson") 
sum(pearson^2)/Mpoi$df.residual

# Residual plots vs. predicted
pred <- predict(Mqp, type = "response")
stand.resid <- rstandard(model = Mqp, type = "pearson") # Standardized Pearson residuals

plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardized Pearson residuals",
   main = "Residuals from quasi-likelihood", ylim = c(-5,5))
abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "red")
# dev.off()

# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure5.7BW.pdf", width = 10, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book
par(mfrow = c(1,2))
Mpoi <- glm(formula = Birds ~ Loc, family = poisson(link = "log"), data = alldata)
pred <- predict(Mpoi, type = "response")
stand.resid <- rstandard(model = Mpoi, type = "pearson")
plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardized Pearson residuals",
   main = "Residuals from regular likelihood", ylim = c(-5,5))
abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "black")
Mqp <- glm(formula = Birds ~ Loc, family = quasipoisson(link = "log"), data = alldata)
pred <- predict(Mqp, type = "response")
stand.resid <- rstandard(model = Mqp, type = "pearson") # Standardized Pearson residuals
plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardized Pearson residuals",
   main = "Residuals from quasi-likelihood", ylim = c(-5,5))
abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "black")
# dev.off()



#####################################################################
# Comparison of inferences

anova(Mpoi, test = "Chisq") 
anova(Mqp, test = "Chisq") 

# Predicted means and CIs
pred.data <- data.frame(Loc = c("ForA", "ForB", "Frag", "Edge", "PasA", "PasB"))
means.poi <- predict(object = Mpoi, newdata = pred.data, type = "link", se.fit = TRUE)
means.qp <- predict(object = Mqp, newdata = pred.data, type = "link", se.fit = TRUE)

# Wald CI for log means
alpha <- 0.05
lower.logmean.poi <- means.poi$fit + qnorm(alpha/2)*means.poi$se.fit
upper.logmean.poi <- means.poi$fit + qnorm(1-alpha/2)*means.poi$se.fit
lower.logmean.qp <- means.qp$fit + qnorm(alpha/2)*means.qp$se.fit
upper.logmean.qp <- means.qp$fit + qnorm(1-alpha/2)*means.qp$se.fit

# Combine means and confidence intervals in count scale
mean.wald.ci.poi <- data.frame(pred.data, round(cbind(exp(means.poi$fit), exp(lower.logmean.poi), exp(upper.logmean.poi)), digits = 2))
colnames(mean.wald.ci.poi) <- c("Location", "Mean", "Lower", "Upper")
mean.wald.ci.poi
mean.wald.ci.qp <- data.frame(pred.data, round(cbind(exp(means.qp$fit), exp(lower.logmean.qp), exp(upper.logmean.qp)), digits = 2))
colnames(mean.wald.ci.qp) <- c("Location", "Mean", "Lower", "Upper")
mean.wald.ci.qp
