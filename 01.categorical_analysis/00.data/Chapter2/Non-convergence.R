#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-23-11                                                    #
# PURPOSE: Show examples of non-convergence with glm()              #
#                                                                   #
# NOTES:                                                            #
#####################################################################

################################################################################
# Example #1

  set1<-data.frame(x1 = c(1,2,3,4,5,6,7,8,9,10), y = c(0,0,0,0,0, 1,1,1,1,1))
  set1

  mod.fit1<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), trace = TRUE)
  summary(mod.fit1)
  mod.fit1$coefficients


  x11(width = 10, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure2.7color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mfrow = c(1,2))
  plot(x = set1$x1, y = set1$y, main = "Plot for set1", ylab = "Estimated probability", xlab = expression(x[1]),
     panel.first = grid(col = "gray", lty = "dotted"))
  curve(expr = predict(object = mod.fit1, newdata = data.frame(x1 = x), type = "response"),
      col = "red", add = TRUE, lwd = 2, n = 1000)


  # Firth model
  library(package = logistf)
  mod.fit.firth1<-logistf(formula = y ~ x1, data = set1)
  options(digits = 4)  # Controls printing in R Console window - the width argument can also be used
  summary(mod.fit.firth1)
  names(mod.fit.firth1)  # Check what is in the object
  data.frame(x = set1$x1, pi.hat = mod.fit.firth1$predict)
  options(digits = 7)  # Go back to default

  # Put model on plot
  beta.hat<-mod.fit.firth1$coefficients
  curve(expr = exp(beta.hat[1]+beta.hat[2]*x)/(1 + exp(beta.hat[1]+beta.hat[2]*x)), col = "blue", add = TRUE, n = 1000)
  legend(x = 0.5, y = 0.8, legend = c("glm()", "logistf()"), lty = c(1,1), lwd = c(2,1), col = c("red", "blue"), bty = "n")

  # Check class and availability of method functions
  class(mod.fit.firth1)
  methods(class = logistf)

  # LRT with penalized likelihood
  # logistftest(formula = y ~ x1, test = ~ x1 - 1, values = 0, data = set1)  # Old syntax
  logistftest(object = mod.fit.firth1, test = ~ x1 - 1, values = 0)  # New syntax


################################################################################
# Example #2

  set2<-data.frame(x1 = c(1,2,3,4,6,5,7,8,9,10), y = c(0,0,0,0,0, 1,1,1,1,1))
  set2

  mod.fit2<-glm(formula = y ~ x1, data = set2, family = binomial(link = logit), trace = TRUE)
  summary(mod.fit2)
  mod.fit1$coefficients

  plot(x = set2$x1, y = set2$y, main = "Plot for set2", ylab = "Estimated probability",
     panel.first = grid(col = "gray", lty = "dotted"), xlab = expression(x[1]))
  curve(expr = predict(object = mod.fit2, newdata = data.frame(x1 = x), type = "response"),
      col = "red", add = TRUE, lwd = 2, n = 1000)

  mod.fit.firth2<-logistf(formula = y ~ x1, data = set2)
  summary(mod.fit.firth2)
  beta.hat<-mod.fit.firth2$coefficients
  curve(expr = exp(beta.hat[1]+beta.hat[2]*x)/(1 + exp(beta.hat[1]+beta.hat[2]*x)), col = "blue", add = TRUE, n = 1000)
  legend(x = 0.5, y = 0.8, legend = c("glm()", "logistf()"), lty = c(1,1), lwd = c(2,1), col = c("red", "blue"), bty = "n")
  # dev.off()  # Create plot for book


################################################################################
# Black-and-white plot

  # pdf(file = "c:\\figures\\Figure2.7BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mfrow = c(1,2))
  plot(x = set1$x1, y = set1$y, main = "Plot for set1", ylab = "Estimated probability", xlab = expression(x[1]))
  curve(expr = predict(object = mod.fit1, newdata = data.frame(x1 = x), type = "response"),
    col = "black", add = TRUE, lwd = 2, n = 1000)
  curve(expr = exp(beta.hat[1]+beta.hat[2]*x)/(1 + exp(beta.hat[1]+beta.hat[2]*x)), col = "black", add = TRUE, n = 1000)
  legend(x = 0.5, y = 0.8, legend = c("glm()", "logistf()"), lty = c(1,1), lwd = c(2,1), col = c("black", "black"), bty = "n")

  plot(x = set2$x1, y = set2$y, main = "Plot for set2", ylab = "Estimated probability", xlab = expression(x[1]))
  curve(expr = predict(object = mod.fit2, newdata = data.frame(x1 = x), type = "response"),
    col = "black", add = TRUE, lwd = 2, n = 1000)
  curve(expr = exp(beta.hat[1]+beta.hat[2]*x)/(1 + exp(beta.hat[1]+beta.hat[2]*x)), col = "black", add = TRUE, n = 1000)
  legend(x = 0.5, y = 0.8, legend = c("glm()", "logistf()"), lty = c(1,1), lwd = c(2,1), col = c("black", "black"), bty = "n")
  # dev.off()  # Create plot for book


################################################################################
# Further investigation of how many iterations R will do with data set #1

  mod.fit3<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 3)
  logLik(mod.fit3)
  mod.fit10<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 10)
  logLik(mod.fit10)
  mod.fit20<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 20)
  logLik(mod.fit20)
  mod.fit26<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 26)
  logLik(mod.fit26)
  mod.fit<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 100,
    epsilon = 10^(-12))  # 34 iterations used
  
  data.frame(logLik = c(logLik(mod.fit3), logLik(mod.fit10), logLik(mod.fit20), logLik(mod.fit26), logLik(mod.fit)),
    G = c(mod.fit3$deviance, mod.fit10$deviance, mod.fit20$deviance, mod.fit26$deviance, mod.fit$deviance))
  
  mod.fit32<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 32,
    epsilon = 10^(-100))
  mod.fit33<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 33,
    epsilon = 10^(-100))
  mod.fit34<-glm(formula = y ~ x1, data = set1, family = binomial(link = logit), maxit = 34,
    epsilon = 10^(-100))
  abs(mod.fit33$deviance - mod.fit32$deviance)/(0.1 + mod.fit33$deviance)
  abs(mod.fit34$deviance - mod.fit33$deviance)/(0.1 + mod.fit34$deviance)

  mod.fit32$deviance
  mod.fit33$deviance
  mod.fit34$deviance




#