#####################################################################
# NAME: Tom Loughin and Chris Bilder                                #
# DATE: 04-11-13                                                    #
# PURPOSE: Demonstrating and adjusting for overdispersion in        #
#     Ecuadorean bird count data                                    #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Enter the data

alldata <- read.table(file = "C:\\data\\BirdCounts.csv", sep = ",", header = TRUE)
head(alldata)

# Fit Poisson Regression 
Mpoi <- glm(formula = Birds ~ Loc, family = poisson(link = "log"), data = alldata)
summary(Mpoi)

# Residual plots vs. predicted
pred <- predict(Mpoi, type = "response")
stand.resid <- rstandard(model = Mpoi, type = "pearson")  #  Standardized Pearson residuals

x11(height = 7, width = 12, pointsize = 12)
par(mfrow = c(1,2))

plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardized Pearson residuals",
   main = "Residuals from regular likelihood", ylim = c(-5,5))
abline(h = c(-3, -2, 2, 3), lty = "dotted", col = "red")

#####################################################################
# Model using quasi-likelihood

# Fit Quasi-Poisson model
Mqp <- glm(formula = Birds ~ Loc, family = quasipoisson(link = "log"), data = alldata)
(sumq <- summary(Mqp))
# Demonstrate calculation of dispersion parameter
pearson <- residuals(Mpoi, type = "pearson") 
sum(pearson^2)/Mpoi$df.residual
# From summary()
sumq$dispersion

# Residual plots vs. predicted
pred <- predict(Mqp, type = "response")
stand.resid <- rstandard(model = Mqp, type = "pearson")  #  Standardized Pearson residuals

plot(x = pred, y = stand.resid, xlab = "Predicted count", ylab = "Standardized Pearson residuals",
   main = "Residuals from quasi-likelihood", ylim = c(-5,5))
abline(h = c(qnorm(0.995), 0, qnorm(0.005)), lty = "dotted", col = "red")

#####################################################################
# Comparison of inferences

# Note: The confint() function computes LR confidence intervals for model parameters.
# Here, these parameters are not as meaningful as means. 

confint(Mpoi)
confint(Mqp)

anova(Mpoi, test = "Chisq") 
anova(Mqp, test = "F") 

# The parameters are based on contrasts that are differences between means.
# Re-parameterizing the model by removing the intercept causes the model to 
#  estimate the 6 log-means directly.
# Confidence intervals for these are now meaningful!
Mpoi.rp <- glm(formula = Birds ~ Loc - 1, family = poisson(link = "log"), data = alldata)
summary(Mpoi.rp)

Mqp.rp <- glm(formula = Birds ~ Loc - 1, family = quasipoisson(link = "log"), data = alldata)
summary(Mqp.rp)

round(exp(cbind(mean = coef(Mpoi.rp), confint(Mpoi.rp))), 2)
round(exp(cbind(mean = coef(Mqp.rp), confint(Mqp.rp))), 2)

# Alternatively, Wald confidence intervals can be computed on the predicted values 
# (means) from the original model

# Predicted means and CIs
pred.data <- data.frame(Loc = c("ForA", "ForB", "Frag", "Edge", "PasA", "PasB"))
means.poi <- predict(object = Mpoi, newdata = pred.data, type = "link", se.fit = TRUE)
means.qp <- predict(object = Mqp, newdata = pred.data, type = "link", se.fit = TRUE)
df.qp <- Mqp$df.residual

# Wald CI for log means
alpha <- 0.05
lower.logmean.poi <- means.poi$fit + qnorm(alpha/2)*means.poi$se.fit
upper.logmean.poi <- means.poi$fit + qnorm(1-alpha/2)*means.poi$se.fit
lower.logmean.qp <- means.qp$fit + qt(alpha/2, df = df.qp)*means.qp$se.fit
upper.logmean.qp <- means.qp$fit + qt(1-alpha/2, df = df.qp)*means.qp$se.fit

# Combine means and confidence intervals in count scale
mean.wald.ci.poi <- data.frame(pred.data, round(cbind(exp(means.poi$fit), exp(lower.logmean.poi), exp(upper.logmean.poi)), digits = 2))
colnames(mean.wald.ci.poi) <- c("Location", "Mean", "Lower", "Upper")
mean.wald.ci.poi
mean.wald.ci.qp <- data.frame(pred.data, round(cbind(exp(means.qp$fit), exp(lower.logmean.qp), exp(upper.logmean.qp)), digits = 2))
colnames(mean.wald.ci.qp) <- c("Location", "Mean", "Lower", "Upper")
mean.wald.ci.qp

#####################################################################
# Negative Binomial fit
#
# The negative binomial is fit with several functions. 
# The MASS package has two ways to do this: 
#  glm.nb() is a model-fitting function that estimates the dispersion parameter theta.
#   A log link is assumed, but alternative link = can be specified.
#  family = negative.binomial(theta = ) fits the negative binomial using the speficied value of theta


library(MASS)

M.nb <- glm.nb(formula = Birds ~ Loc, data = alldata)
summary(M.nb)
anova(M.nb, test = "Chisq")
# library(car)
# Anova(M.nb)

M.nb.rp <- glm.nb(formula = Birds ~ Loc - 1, data = alldata)
round(exp(cbind(mean = coef(M.nb.rp), confint(M.nb.rp))), 2)



# Plot of squared residuals plots vs. predicted
names(M.nb)

res.sq <- residuals(object = Mpoi, type = "response")^2
set1 <- data.frame(res.sq, mu.hat = Mpoi$fitted.values)

fit.lin <- lm(formula = res.sq ~ mu.hat, data = set1)
fit.quad <- lm(formula = res.sq ~ mu.hat + I(mu.hat^2), data = set1)
summary(fit.quad)

x11(height = 7, width = 6)
# pdf(file = "c:\\figures\\Figure5.10color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = set1$mu.hat, y = set1$res.sq, xlab = "Predicted count", ylab = "Squared Residual")
curve(expr = predict(object = fit.lin, newdata = data.frame(mu.hat = x), type = "response"), col = "blue",
   add = TRUE, lty = "solid")
curve(expr = predict(object = fit.quad, newdata = data.frame(mu.hat = x), type = "response"), col = "red",
   add = TRUE, lty = "dashed")
legend(x = 50, y = 1000, legend = c("Linear", "Quadratic"), col = c("red", "blue"),
    lty = c("solid", "dashed"), bty = "n")
# dev.off()  # Create plot for book

# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure5.10BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = set1$mu.hat, y = set1$res.sq, xlab = "Predicted count", ylab = "Squared Residual")
curve(expr = predict(object = fit.lin, newdata = data.frame(mu.hat = x), type = "response"), col = "black",
   add = TRUE, lty = "solid")
curve(expr = predict(object = fit.quad, newdata = data.frame(mu.hat = x), type = "response"), col = "black",
   add = TRUE, lty = "dashed")
legend(x = 50, y = 1000, legend = c("Linear", "Quadratic"), col = c("black", "black"),
    lty = c("solid", "dashed"), bty = "n")
# dev.off()  # Create plot for book


