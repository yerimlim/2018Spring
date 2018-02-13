#####################################################################
# NAME:  Tom Loughin and Chris Bilder                               #
# DATE:  01-23-12                                                   #
# PURPOSE: Use Poisson regression models with the alcohol           #
#          consumption data                                         #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Read the data

  dehart <- read.table("C:\\Data\\DeHartSimplified.csv", header = TRUE, sep = ",", na.strings = " ")
  head(dehart)

  # Reduce data to what is needed for examples
  saturday <- dehart[dehart$dayweek == 6, c(1,4,7,8)]
  head(round(x = saturday, digits = 3))
  dim(saturday)

#####################################################################
# Use negative events to estimate the mean number of drinks

  # Fit model of Drinks vs. Neg Events.
  mod.neg <- glm(formula = numall ~ negevent, family = poisson(link = "log"), data = saturday)

  # Analyze the model fit with various functions
  summary(mod.neg)
  100*(exp(mod.neg$coefficients[2]) - 1)
  beta1.int <- confint(mod.neg, parm = "negevent", level = 0.95)
  100*(exp(beta1.int) - 1)
  library(car)
  Anova(mod.neg)

  # Matching confidence level with p-value of LR test just to demonstrate equivalence)
  confint(mod.neg, parm = "negevent", level = 1 - .04688)

  # Plot of data and estimated mean: B/W
  x11(height = 6, width = 7, pointsize = 15)
  # pdf(file = "c:\\figures\\Figure4.5color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  plot(x = saturday$negevent, y = saturday$numall, xlab = "Negative Event Index", ylab = "Alcoholic Drinks Consumed")
  curve(expr = exp(mod.neg$coefficients[1] + x*mod.neg$coefficients[2]), add = TRUE, lwd = 2)

  # Function to find confidence interval
  ci.mu <- function(newdata, mod.fit.obj, alpha) {
    lin.pred.hat <- predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
    lower <- exp(lin.pred.hat$fit - qnorm(1-alpha/2) * lin.pred.hat$se)
    upper <- exp(lin.pred.hat$fit + qnorm(1-alpha/2) * lin.pred.hat$se)
    list(lower = lower, upper = upper)
  }

  # Test
  ci.mu(newdata = data.frame(negevent = 2), mod.fit.obj = mod.neg, alpha = 0.05)

  # Add confidence interval bands
  curve(expr = ci.mu(newdata = data.frame(negevent = x), mod.fit.obj = mod.neg, alpha = 0.05)$lower,
    col = "blue", add = TRUE, lty = "dotdash")
  curve(expr = ci.mu(newdata = data.frame(negevent = x), mod.fit.obj = mod.neg, alpha = 0.05)$upper,
    col = "blue", add = TRUE, lty = "dotdash")
  # dev.off()  # Create plot for book


  # Black-and-white version of plot
  # pdf(file = "c:\\figures\\Figure4.5BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  plot(x = saturday$negevent, y = saturday$numall, xlab = "Negative Event Index", ylab = "Alcoholic Drinks Consumed")
  curve(expr = exp(mod.neg$coefficients[1] + x*mod.neg$coefficients[2]), add = TRUE, lwd = 2)
  curve(expr = ci.mu(newdata = data.frame(negevent = x), mod.fit.obj = mod.neg, alpha = 0.05)$lower,
    col = "black", add = TRUE, lty = "dotdash")
  curve(expr = ci.mu(newdata = data.frame(negevent = x), mod.fit.obj = mod.neg, alpha = 0.05)$upper,
    col = "black", add = TRUE, lty = "dotdash")
  # dev.off()  # Create plot for book





#####################################################################
# Use negative and positive events to estimate the mean number of drinks

  # Fit and analyze model with NEG and POS events, plus interaction
  mod.negpos <- glm(formula = numall ~ negevent*posevent, family = poisson(link = "log"), data = saturday)
  summary(mod.negpos)
  confint(mod.negpos)
  Anova(mod.negpos)

  # Prep work to make plots: creating grid of points: x1=neg events, y1=pos events
  #  Then creating matrix of estimated means.  Matrix form required by contour()
  x1 <- seq(from = 0, to = 2.5, by = .01)
  y1 <- seq(from = 0, to = 3.5, by = .01)
  xy1 <- data.frame(expand.grid(negevent = x1, posevent = y1))

  surface = matrix(predict(object = mod.negpos, newdata = xy1, type = "response"), nrow = length(x1))

  # Plot of POS events vs. NEG events, using shading to indicate number of drinks
  # BW
  x11(height = 6, width = 7, pointsize = 15)
  # pdf(file = "c:\\figures\\Figure4.6-1BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  drink.colors <- gray(1 - saturday$numall/max(saturday$numall))
  plot(x = saturday$negevent, y = saturday$posevent, xlim = c(0,2.5), ylim = c(0,3.5), xlab = "Negative Event Index", ylab = "Positive Event Index", pch = 21, bg = drink.colors, cex = 1.5, main = "Number of Drinks vs. Negative and Positive Events")
  contour(x = x1, y = y1, z = surface, xlim = c(0,2.5), ylim = c(0,3.5), labcex = 1, levels = c(1,2,3,4,5,6,7,8,9,10,15,20,30,40,50,60,80), add = TRUE)
  # dev.off()  # Create plot for book
  # Color
  x11(height = 6, width = 7, pointsize = 15)
  # Did not use in book - pdf(file = "c:\\figures\\Figure4.6-1color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  colorpal <- cm.colors(max(saturday$numall))
  drink.col2 <- colorpal[saturday$numall]
  plot(x = saturday$negevent, y = saturday$posevent, xlim = c(0,2.5), ylim = c(0,3.5), xlab = "Negative Event Index", ylab = "Positive Event Index", pch = 21, bg = drink.col2, cex = 1.5, main = "Number of Drinks vs. Negative and Positive Events")
  contour(x = x1, y = y1, z = surface, xlim = c(0,2.5), ylim = c(0,3.5), labcex = 1, levels = c(1,2,3,4,5,6,7,8,9,10,15,20,30,40,50,60,80), add = TRUE, color = "gray10")
  # dev.off()  # Create plot for book

  # 3D plot to show surface more clearly.
  library(rgl)
  open3d()
  persp3d(x = x1, y = y1, z  =  surface, col = "red", xlab = "Negative Event Index", ylab = "Positive Event Index", zlab = "Predicted Drinks")
  points3d(x = saturday$negevent, y = saturday$posevent, z = saturday$numall, col = "blue")

  # Plot of data and estimated mean at quartiles of POSEVENT
  posev.quart <- summary(saturday$posevent)
  posev.quart

  x11(height = 6, width = 7, pointsize = 15)
  # pdf(file = "c:\\figures\\Figure4.6-2BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  posev.colors <- gray(1 - saturday$posevent/max(saturday$posevent))

  plot(x = saturday$negevent, y = saturday$numall, xlab = "Negative Event Index", ylab = "Alcoholic Drinks Consumed", pch = 21, bg = posev.colors, cex = 1.5, main = "Number of Drinks vs. Negative Events")
  curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[2]*mod.negpos$coefficients[3] + x*posev.quart[2]*mod.negpos$coefficients[4]), add = TRUE, lwd = 2)
  curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[3]*mod.negpos$coefficients[3] + x*posev.quart[3]*mod.negpos$coefficients[4]), add = TRUE, lty = "dashed", lwd = 2)
  curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[5]*mod.negpos$coefficients[3] + x*posev.quart[5]*mod.negpos$coefficients[4]), add = TRUE, lty = "dotted", lwd = 2)
  legend(x = 1.0, y = 20, legend = c("1st", "2nd", "3rd"), lty = c("solid", "dashed", "dotted"), lwd = 2, title = "Quartile of Positive Event Index", bty = "n")
  # dev.off()  # Create plot for book

  x11(height = 6, width = 7, pointsize = 15)
  # pdf(file = "c:\\figures\\Figure4.6-2color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
  posev.colors <- gray(1 - saturday$posevent/max(saturday$posevent))

  plot(x = saturday$negevent, y = saturday$numall, xlab = "Negative Event Index", ylab = "Alcoholic Drinks Consumed", pch = 21, bg = posev.colors, cex = 1.5, main = "Number of Drinks vs. Negative Events")
  curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[2]*mod.negpos$coefficients[3] + x*posev.quart[2]*mod.negpos$coefficients[4]), add = TRUE, lwd = 2, col = "red")
  curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[3]*mod.negpos$coefficients[3] + x*posev.quart[3]*mod.negpos$coefficients[4]), add = TRUE, lty = "dashed", lwd = 2, col = "red")
  curve(expr = exp(mod.negpos$coefficients[1] + x*mod.negpos$coefficients[2] + posev.quart[5]*mod.negpos$coefficients[3] + x*posev.quart[5]*mod.negpos$coefficients[4]), add = TRUE, lty = "dotted", lwd = 2, col = "red")
  legend(x = 1.0, y = 20, legend = c("1st", "2nd", "3rd"), lty = c("solid", "dashed", "dotted"), col = c("red","red","red"), lwd = 2, title = "Quartile of Positive Event Index", bty = "n")
  # dev.off()  # Create plot for book

  # Estimate percentage change per unit negevent at three quartiles of posevent
  mean.ratio <- exp(mod.negpos$coefficients[2] + posev.quart[c(2,3,5)]*mod.negpos$coefficients[4])
  mean.ratio
  100*(mean.ratio - 1)
  
  # Profile LR interval using mcprofile
  library(package = mcprofile)
  K <- matrix(data = c(0, 1, 0, 1*posev.quart[2],
                       0, 1, 0, 1*posev.quart[3],
                       0, 1, 0, 1*posev.quart[5]), nrow = 3, ncol = 4, byrow = TRUE)
  linear.combo <- mcprofile(object = mod.negpos, CM = K)  #Calculate -2log(Lambda)
  ci.beta <- confint(object = linear.combo, level = 0.95)
  # ci.beta$confint
  100*(exp(ci.beta$estimate) - 1) #Verifies got same answer as above
  100*(exp(ci.beta$confint) - 1)

