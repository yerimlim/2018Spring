#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  5-9-11                                                     #
# PURPOSE: Examine the sampling distribution of beta^_1             #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Settings where logit(pi) = beta0 + beta1*x and x = 0 or 1

  # What is beta_1 when x = 0?
  pi.x0<-0.01
  beta0<-log(pi.x0/(1-pi.x0))
  beta0

  # What is beta_1 when x = 1?
  pi.x1<-0.99
  beta1<-log(pi.x1/(1-pi.x1)) - beta0
  beta1

  # Check
  exp(beta0 + beta1*c(0,1)) / (1 + exp(beta0 + beta1*c(0,1)))

  # Plot of model
  # pdf(file = "c:\\figures\\Figure2.8color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  curve(expr = exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x)), xlim = c(0,1), ylab = expression(pi),
    n = 1000, lwd = 3, xlab = expression(x[1]) )


#####################################################################
# Simulate one data set for n = 500

  # Explanatory variable values and corresponding pi's
  set.seed(8238)
  x1<-runif(n = 500, min = 0, max = 1)
  pi<-exp(beta0 + beta1*x1) / (1 + exp(beta0 + beta1*x1))

  # Set seed number to reproduce results and simulate responses
  set.seed(1829)
  y<-rbinom(n = length(x1), size = 1, prob = pi)
  head(data.frame(y, x1, pi), n = 10)  # Check
  
  # Estimate model
  mod.fit<-glm(formula = y ~ x1, family = binomial(link = logit))  # no data argument needed
  mod.fit$coefficients
  cov.mat<-vcov(mod.fit)
  sqrt(cov.mat[2,2])  # sqrt( Var^(beta^_1) )
 
  # Plot of estimated model
  beta.hat0<-mod.fit$coefficients[1]
  beta.hat1<-mod.fit$coefficients[2]
  curve(expr = exp(beta.hat0 + beta.hat1*x) / (1 + exp(beta.hat0 + beta.hat1*x)), xlim = c(0,1), ylab = expression(pi),
    add = TRUE, col = "red", n = 1000)
  legend(x = 0, y = 1, legend = c("True", "Estimated"), lty = c(1,1), col = c("black", "red"), lwd = c(3, 1),
    bty = "n")
  # dev.off()  # Create plot for book

   
  # Black-and-white version of plot
  # pdf(file = "c:\\figures\\Figure2.8BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  curve(expr = exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x)), xlim = c(0,1), ylab = expression(pi),
    n = 1000, lwd = 3, xlab = expression(x[1]) )
  curve(expr = exp(beta.hat0 + beta.hat1*x) / (1 + exp(beta.hat0 + beta.hat1*x)), xlim = c(0,1), ylab = expression(pi),
    add = TRUE, col = "black", lty = "dotted", n = 1000)
  legend(x = 0, y = 1, legend = c("True", "Estimated"), lty = c("solid", "dotted"), col = c("black", "black"), lwd = c(3, 1),
    bty = "n")
  # dev.off()  # Create plot for book

  # Confidence intervals for beta1
  profileLR.beta<-confint(mod.fit, parm = "x1", level = 0.95)  # profile LR interval
  profileLR.beta 
  wald.beta<-confint.default(mod.fit, parm = "x1", level = 0.95)  # Wald interval
  wald.beta
    
 
#####################################################################
# Simulate more than one data set and calculate true confidence level 

sim.all<-function(sample.size, number.data.sets, x, seed.numb, color = TRUE, linetype = "solid") {

  if(color == TRUE) {line.color = "red"} else {line.color = "black"}  # Allows for black-and-white plots to use in printed version of book
  # print(line.color)

  # Set seed number to reproduce results and simulate responses
  set.seed(seed.numb)
  y<-rbinom(n = length(x)*number.data.sets, size = 1, prob = pi)

  # Check out simulated data
  all.x<-rep(x, times = number.data.sets)

  # Restructure y for apply() function
  y.mat<-matrix(data = y, nrow = length(x), ncol = number.data.sets)
  # y.mat[1:3,1:5]  # Check


  # Function to estimate model and obtain pi^'s - could put this outside of sim.all() too
  calc2<-function(y, x) {
    mod.fit<-glm(formula = y ~ x, family = binomial(link = logit))
    den.logLik<-logLik(mod.fit)
    mod.fit2<-glm(formula = y ~ offset(beta1*x), family = binomial(link = logit))  # Used to get beta~_0 estimate
    num.logLik<-logLik(mod.fit2)
    profileLR.beta<-confint(mod.fit, parm = "x", level = 0.95)  # profile LR interval
    wald.beta<-confint.default(mod.fit, parm = "x", level = 0.95)  # Wald interval
    cov.mat<-vcov(mod.fit)
    c(as.numeric(profileLR.beta),  wald.beta, mod.fit$coefficients, mod.fit$converged,
      cov.mat[1,1], cov.mat[2,2], den.logLik, num.logLik)
  }

  # round(calc2(y = y.mat[,1], x = x),4)  # Check
  
  # Apply the function to every column of the y.mat matrix.
  save.results<-apply(X = y.mat, MARGIN = 2, FUN = calc2, x = x)
  

  #Calculate the true confidence level and investigate problems
  profile.ck<-ifelse(test = beta1 > save.results[1,], yes = ifelse(test = beta1 < save.results[2,], yes = 1, no = 0), no = 0)
  cat("Profile est. true conf. level", sum(profile.ck, na.rm = TRUE)/sum(!is.na(profile.ck)), "\n")  # Replaced number.data.sets with sum(!is.na(profile.ck)) due to the data sets where NA is produced for a confidence interval limit.
  cat("Profile problems = ", sum(is.na(profile.ck)), "\n")  # Number of data sets where profile LR interval did not work
  # sum(is.na(save.results[2,]))  # Problem with upper limit?
  # sum(is.na(save.results[1,]))  # Problem with lower limit?
  
  wald.ck<-ifelse(test = beta1 > save.results[3,], yes = ifelse(test = beta1 < save.results[4,], yes = 1, no = 0), no = 0)
  cat("Wald est. true conf. level", sum(wald.ck)/number.data.sets, "\n")
  
  
  # Plots of the first 100 intervals
  x11(width = 7, height = 8, pointsize = 12)
  par(mfrow = c(1,2))
  plot(x = save.results[1,1:100], y = 1:100, type = "p", pch = "(", xlim = c(min(save.results[c(1,3),1:100], na.rm = TRUE), max(save.results[c(2,4),1:100], na.rm = TRUE)),
       xlab = expression(beta[1]), ylab = "Data set number", main = "Profile likelihood ratio", cex = 0.75) 
  points(x = save.results[2,1:100], y = 1:100, pch = ")", cex = 0.75)
  segments(x0 = save.results[1,1:100], x1 =  save.results[2,1:100], y0 = 1:100, y1 = 1:100, lty = "dotted")
  abline(v = beta1, lty = "solid", col = line.color)
  cat("First 100 for profile = ", sum(profile.ck[1:100])/100, "\n")

  plot(x = save.results[3,1:100], y = 1:100, type = "p", pch = "(", xlim = c(min(save.results[c(1,3),1:100], na.rm = TRUE), max(save.results[c(2,4),1:100], na.rm = TRUE)),
       xlab = expression(beta[1]), ylab = "Data set number", main = "Wald", cex = 0.75) 
  points(x = save.results[4,1:100], y = 1:100, pch = ")", cex = 0.75)
  segments(x0 = save.results[3,1:100], x1 =  save.results[4,1:100], y0 = 1:100, y1 = 1:100, lty = "dotted")
  abline(v = beta1, lty = "solid", col = line.color)
  cat("First 100 for wald = ", sum(wald.ck[1:100])/100, "\n")
    
  # Length
  # mean(save.results[2,]-save.results[1,], na.rm = TRUE)
  # mean(save.results[4,]-save.results[3,])
  # median(save.results[2,]-save.results[1,], na.rm = TRUE)
  # median(save.results[4,]-save.results[3,])
  

  # Histogram of parameter estimates with normal distribution overlay
  x11(width = 11, height = 8, pointsize = 12)
  par(mfrow = c(1,2))

  # beta^_0
  hist(x = save.results[5,], xlab = expression(hat(beta)[0]), main = paste("n = ", sample.size), freq = FALSE)
  curve(expr = dnorm(x = x, mean = mean(save.results[5,]), sd = sqrt(var(save.results[5,]))), col = line.color, add = TRUE, lwd = 2)
  # beta^_1
  hist(x = save.results[6,], xlab = expression(hat(beta)[1]), main = paste("n = ", sample.size), freq = FALSE)
  curve(expr = dnorm(x = x, mean = mean(save.results[6,]), sd = sqrt(var(save.results[6,]))), col = line.color, add = TRUE, lwd = 2)

  # Z = (beta^_1 - beta_1)/sqrt(var(beta^_1))
  x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure2.10color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mfrow = c(1,1))
  z<-(save.results[6,]-beta1)/sqrt(save.results[9,])
  hist(x = z,  freq = FALSE, main = paste("n = ", sample.size),,
    xlab = expression((hat(beta)[1] - beta[1]) / sqrt(widehat(Var)(hat(beta)[1])) ),
    xlim = c(-4,4))
  curve(expr = dnorm(x = x, mean = 0, sd = 1), col = line.color, add = TRUE, lwd = 2)
# dev.off()  # Create plot for book
  cat("z and standard normal quantiles: \n")
  print(quantile(x = z, probs = c(0.005, 0.05, 0.025, 0.95, 0.975, 0.995)))
  print(qnorm(p = c(0.005, 0.05, 0.025, 0.95, 0.975, 0.995)))

  # -2log(Lambda)
  x11(width = 7, height = 6, pointsize = 12)
  tran.stat<--2*(save.results[11,] - save.results[10,])
# pdf(file = "c:\\figures\\Figure2.11color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  hist(x = tran.stat, xlab = expression(-2*log(L(tilde(beta)[0], beta[1])/L(hat(beta)[0], hat(beta)[1]))),
    main = paste("n = ", sample.size), freq = FALSE, nclass = 50)  # For n = 500, helpful to have more classes than default
  curve(expr = dchisq(x = x, df=1), col = line.color, add = TRUE, lwd = 2, n = 1000, xlim = c(0.01,16))  # Starting at 0.01 eliminates the line being plotted at 0
# dev.off()  # Create plot for book
  cat("-2log(Lambda) and chi^2_1 quantiles: \n")
  print(quantile(x = tran.stat, probs = c(0.9, 0.95, 0.995)))
  print(qchisq(p = c(0.9, 0.95, 0.995), df = 1))


  # Plot of beta.hat's
  x11(width = 11, height = 8, pointsize = 12)
  par(mfrow = c(1,2))
  beta.lab<-c("Intercept", "Slope")
  plot.data<-data.frame(name = rep(x = beta.lab, each = number.data.sets), beta.hat = c(save.results[5,], save.results[6,]))
  par(mfrow = c(1,2))
  boxplot(formula = beta.hat ~ name, data = plot.data, col = "lightblue", main = paste("Box plot, n = ", sample.size), ylab = expression(hat(beta)),
    xlab = " ")
  stripchart(x = beta.hat ~ name, data = plot.data, method = "jitter", vertical = TRUE, pch = 1,
    main = paste("Dot plot, n = ", sample.size), ylab = expression(hat(beta)), xlab = "")

  # paste("n = ", sample.size)
  # Plot of first 100 models
  x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure2.9color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  save.results100<-save.results[,1:100]
  par(mfrow = c(1,1))
  curve(expr = exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x)), xlim = c(0,1), ylab = expression(pi), lwd = 1,
    main = "", type = "n", xlab = expression(x[1]))  # Put graph up without anything in it (type = "n")
  # Estimated models
  for (i in 1:100) {
    beta.hat0<-save.results[5,i]
    beta.hat1<-save.results[6,i]
    curve(expr = exp(beta.hat0 + beta.hat1*x) / (1 + exp(beta.hat0 + beta.hat1*x)), xlim = c(0,1), ylab = expression(pi),
      add = TRUE, col = line.color, lty = linetype)  # Use solid line for book
  }
  # Plot true model on top of every other models plotted - this makes sure it is not covered
  curve(expr = exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x)), xlim = c(0,1), ylab = expression(pi), lwd = 5, n = 1000,
     add = TRUE)
# dev.off()  # Create plot for book

  # Check convergence
  cat("Convergence information: ", sum(save.results[7,]), "\n")

  save.results
}

  sample.size<-500
  set.seed(8238)
  x<-runif(n = sample.size, min = 0, max = 1)
  pi<-exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x))

  # Used for colored version of book
  # save.it<-sim.all(sample.size = sample.size, number.data.sets = 10000, x = x, seed.numb = 1829)

  # Used for black-and-white version of book
  # save.it<-sim.all(sample.size = sample.size, number.data.sets = 10000, x = x, seed.numb = 1829,
  #   color = FALSE, linetype = "dotted")

  # Smaller number of data sets for testing purposes
  save.it<-sim.all(sample.size = sample.size, number.data.sets = 100, x = x, seed.numb = 1829,
    color = FALSE, linetype = "dotted")
  

  #For those intervals that achieve the correct true confidence level, we would expect
  # their estimated true confidence levels to be within
  number.data.sets<-1000
  0.95 + qnorm(p = c(0.005, 0.995)) * sqrt(0.95*(1-0.95)/number.data.sets)


#