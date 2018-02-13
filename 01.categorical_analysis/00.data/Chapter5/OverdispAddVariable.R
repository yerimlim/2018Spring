#######################################################################
# NAME: Tom Loughin                                                   #
# DATE: 1-10-13                                                       #
# PURPOSE: Adding a variable to reduce overdispersion: Placekick data #
#                                                                     #
# NOTES:                                                              #
#######################################################################

placekick <- read.table(file = "C:\\data\\Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)

# Putting data into explanatory variable pattern form for wind + distance 
#  so that deviance/DF statistics can be compared.
w <- aggregate(formula = good ~ distance + wind , data = placekick, FUN = sum)
n <- aggregate(formula = good ~ distance + wind , data = placekick, FUN = length)
w.n <- data.frame(success = w$good, trials = n$good, 
        distance = w$distance, wind = w$wind)

# Fit logistic regression model with wind, but not distance.
mod.fit.nodist <- glm(formula = success/trials ~ wind, weights = trials,
           family = binomial(link = logit), data = w.n)
summary(mod.fit.nodist) 
pred <- predict(mod.fit.nodist)
stand.resid <- rstandard(model = mod.fit.nodist, type = "pearson")  # Standardized Pearson residuals

x11(height = 7, width = 12, pointsize = 12)
# pdf(file = "c:\\figures\\Figure5.8color.pdf", width = 9, height = 6, colormodel = "cmyk")   # Create plot for book
par(mfrow = c(1,2))

# Standardized pearson residual vs predicted without distance
plot(x = pred, y = stand.resid, xlab = "Estimated logit(P(success))", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. Estimated logit", ylim = c(-6, 13))
abline(h = c(-3,-2,0,2,3), lty = "dotted", col = "red")

# Standardized pearson residual vs Distance
plot(w.n$distance, y = stand.resid, xlab = "Distance", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. Distance", ylim = c(-6, 13))
abline(h = c(-3,-2,0,2,3), lty = "dotted", col = "red")
dist.ord <- order(w.n$distance)
lines(x = w.n$distance[dist.ord], y = predict(loess(formula = stand.resid ~ w.n$distance, weights = w.n$trials))[dist.ord], col = "blue")
# dev.off()  # Create plot for book


# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure5.8BW.pdf", width = 9, height = 6, colormodel = "cmyk")   # Create plot for book
par(mfrow = c(1,2))
plot(x = pred, y = stand.resid, xlab = "Estimated logit(P(success))", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. Estimated logit", ylim = c(-6, 13))
abline(h = c(-3,-2,0,2,3), lty = "dotted", col = "black")
plot(w.n$distance, y = stand.resid, xlab = "Distance", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. Distance", ylim = c(-6, 13))
abline(h = c(-3,-2,0,2,3), lty = "dotted", col = "black")
lines(x = w.n$distance[dist.ord], y = predict(loess(formula = stand.resid ~ w.n$distance, weights = w.n$trials))[dist.ord], col = "black")
# dev.off()  # Create plot for book



# Adding distance to the model.
mod.fit.dist <- glm(formula = success/trials ~ distance + wind, weights = trials,
         family = binomial(link = logit), data = w.n)
summary(mod.fit.dist) 
stand.resid <- rstandard(model = mod.fit.dist, type = "pearson")  # Standardized Pearson residuals
pred <- predict(mod.fit.dist)

pred.ord <- order(predict(mod.fit.dist))

# Standardized pearson residual vs predicted with distance
x11(height = 7, width = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure5.9color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = pred, y = stand.resid, xlab = "Estimated logit(P(Success))", ylab = "Standardized Pearson residuals", ylim = c(-6, 13))
abline(h = c(-3,-2,0,2,3), lty = "dotted", col = "red")
# Add a loess fit to the residuals
pred.ord <- order(pred)
lines(x = pred[pred.ord], y = predict(loess(formula = stand.resid ~ pred, weights = w.n$trials))[pred.ord], col = "blue")
# dev.off()  # Create plot for book


# pdf(file = "c:\\figures\\Figure5.9BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = pred, y = stand.resid, xlab = "Estimated logit(P(Success))", ylab = "Standardized Pearson residuals", ylim = c(-6, 13))
abline(h = c(-3,-2,0,2,3), lty = "dotted", col = "black")
# Add a loess fit to the residuals
pred.ord <- order(pred)
lines(x = pred[pred.ord], y = predict(loess(formula = stand.resid ~ pred, weights = w.n$trials))[pred.ord], col = "black")
# dev.off()  # Create plot for book


# Standardized pearson residual vs Distance
par(mfrow = c(1,1))
stand.resid <- rstandard(model = mod.fit.nodist, type = "pearson")  # Standardized Pearson residuals
plot(w.n$distance, y = stand.resid, xlab = "Distance", ylab = "Standardized Pearson residuals",
   main = "Standardized residuals vs. Distance", ylim = c(-6, 13))
abline(h = c(-3,-2,0,2,3), lty = "dotted", col = "red")
dist.ord <- order(w.n$distance)
lines(x = w.n$distance[dist.ord], y = predict(loess(formula = stand.resid ~ w.n$distance, weights = w.n$trials))[dist.ord], col = "blue")
