#####################################################################
# NAME: Chris Bilder                                                #
# DATE: 6-5-13                                                      #
# PURPOSE: Bayesian methods with the placekicking data set          #
#                                                                   #
# NOTES:                                                            #
#####################################################################

placekick <- read.table(file = "C:\\data\\Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)


#####################################################################
# Frequentist approach

 mod.fit <- glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
 summary(object = mod.fit)
 
 # Modified function for C.I.s from Chapter 2
 ci.pi <- function(newdata, mod.fit.obj, alpha){
  linear.pred <- predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
  CI.lin.pred.lower <- linear.pred$fit - qnorm(p = 1-alpha/2)*linear.pred$se
  CI.lin.pred.upper <- linear.pred$fit + qnorm(p = 1-alpha/2)*linear.pred$se
  CI.pi.lower <- exp(CI.lin.pred.lower) / (1 + exp(CI.lin.pred.lower))
  CI.pi.upper <- exp(CI.lin.pred.upper) / (1 + exp(CI.lin.pred.upper))
  list(pi.hat = plogis(linear.pred$fit), lower = CI.pi.lower, upper = CI.pi.upper)
 }

 # Test cases
 ci.pi(newdata = data.frame(distance = 20), mod.fit.obj = mod.fit, alpha = 0.05)
 ci.pi(newdata = data.frame(distance = 50), mod.fit.obj = mod.fit, alpha = 0.05)



#####################################################################
# Estimate model using Bayesian methods

 library(package = MCMCpack)

 # Plot of prior distribution
 curve(expr = dnorm(x = x, mean = 0, sd = sqrt(0.001^(-1))), xlim = c(-100, 100), xlab = expression(beta),
  ylab = expression(f(beta)))

 # Normal prior where beta0 and beta1 ~ N(0, 0.001^-1)
 #  Turn off buffered output (MISC > BUFFERED OUTPUT) if you want to see the results from verbose
 #  after every set
 mod.fit.Bayes <- MCMClogit(formula = good ~ distance, data = placekick, seed = 8712,
  b0 = 0, B0 = 0.001, burnin = 10000, verbose = 10000, mcmc = 100000)
 summary(mod.fit.Bayes)
 HPDinterval(obj = mod.fit.Bayes, prob = 0.95)

 # Show that the output in summary() can be reproduced here
 head(mod.fit.Bayes)
 tail(mod.fit.Bayes)
 colMeans(mod.fit.Bayes)  # Same as Mean column in summary()
 apply(X = mod.fit.Bayes, MARGIN = 2, FUN = sd)  # Same as SD column in summary()

 # Where is beta1 = 0 relative to the distribution - like a p-value
 beta1 <- mod.fit.Bayes[,2]
 min(beta1)
 max(beta1)
 mean(beta1 >= 0)  # 0/100000

 # OR for a 10 yard decrease in distance
 beta0 <- mod.fit.Bayes[,1]
 beta1 <- mod.fit.Bayes[,2]
 OR10 <- exp(-10*beta1)
 x11(width = 7, height = 6, pointsize = 12)
 hist(x = OR10)
 densplot(x = OR10, show.obs = FALSE)  # coda function, second argument removes a rug plot from the bottom
 mean(OR10)
 quantile(x = OR10, probs = c(0.025, 0.975))  # 95% equal-tail
 HPDinterval(obj = OR10, prob = 0.95)  # 95% HPD

 # pi for 20 and 50 yard placekicks
 pi20 <- plogis(q = beta0 + beta1*20)
 mean(pi20)
 hist(x = pi20)
 densplot(x = pi20, show.obs = FALSE)
 quantile(x = pi20, probs = c(0.025, 0.975))  # 95% equal-tail
 HPDinterval(obj = pi20, prob = 0.95)  # 95% HPD

 pi50 <- plogis(q = beta0 + beta1*50)
 mean(pi50)
 hist(x = pi50)
 densplot(x = pi50, show.obs = FALSE)  # coda function
 quantile(x = pi50, probs = c(0.025, 0.975))  # 95% equal-tail
 HPDinterval(obj = pi50, prob = 0.95)  # 95% HPD


#####################################################################
# Diagnostics

 # Class of object and method functions available
 class(mod.fit.Bayes)
 methods(class = "mcmc")

 # Example of how to thin after running MCMClogit()
 temp <- window(x = mod.fit.Bayes, start = 20000, thin = 10)  # stats package
 head(temp)
 
 # Trace and density plots
 plot(mod.fit.Bayes)  # Entire chain
 mod.fit.Bayes.temp <- window(x = mod.fit.Bayes, start = 10001, end = 20000)  # Pull out first 10,000
 head(mod.fit.Bayes.temp)
 tail(mod.fit.Bayes.temp)
 x11(width = 10, height = 6, pointsize = 12)
 # pdf(file = "c:\\figures\\Figure6.10BW.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book
 plot(mod.fit.Bayes.temp)
 # dev.off()  # Create plot for book


 # Diagnostic tests
 heidel.diag(x = mod.fit.Bayes)
 geweke.diag(x = mod.fit.Bayes)
 geweke.plot(x = mod.fit.Bayes, frac1 = 0.1, frac2 = 0.5, nbins = 20,
      pvalue = 0.05, auto.layout = TRUE)  # Provides some help to determine when convergence does occur if the previous test indicated problems

 # Effective sample size
 effectiveSize(mod.fit.Bayes)
 effectiveSize(mod.fit.Bayes.temp)

 # acceptance rate
 1 - rejectionRate(x = mod.fit.Bayes)


 ##################
 # Additional chains

  # Find new starting values +- 1SD from MLE
  beta.hat.mle <- mod.fit$coefficients
  SE.mle <- sqrt(diag(vcov(mod.fit)))  # sqrt(Var^(beta^))
  beta.hat.mle - SE.mle
  beta.hat.mle + SE.mle

  # Obtain a second set of samples from the posterior
  mod.fit.Bayes2 <- MCMClogit(formula = good ~ distance, data = placekick, seed = 2221,
   b0 = 0, B0 = 0.001, burnin = 10000, verbose = 10000, mcmc = 100000,
   beta.start = beta.hat.mle - SE.mle)
  summary(mod.fit.Bayes2)
  plot(mod.fit.Bayes2)

  # Obtain a third set of samples from the posterior
  mod.fit.Bayes3 <- MCMClogit(formula = good ~ distance, data = placekick, seed = 8112,
   b0 = 0, B0 = 0.001, burnin = 10000, verbose = 10000, mcmc = 100000,
   beta.start = beta.hat.mle + SE.mle)
  summary(mod.fit.Bayes3)
  plot(mod.fit.Bayes3)

  # Diagnostics
  mod.fit.mult <- mcmc.list(mod.fit.Bayes, mod.fit.Bayes2, mod.fit.Bayes3)
  gelman.diag(mod.fit.mult)


 # Additional plots
 autocorr.plot(mod.fit.Bayes)  # Plot of autocorrelations
 acf(mod.fit.Bayes)  # This is autocorrelation plot used in time series analysis.
 crosscorr.plot(mod.fit.Bayes)  # Plot of crosscorrelations
 # cumuplot(mod.fit.Bayes)  # This produces plots for the entire chain, which can take a while
 x11(width = 10, height = 6, pointsize = 12)
 # pdf(file = "c:\\figures\\Figure6.11BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 cumuplot(mod.fit.Bayes.temp)  # First 10000
 # dev.off()  # Create plot for book

 # Improper prior distribution example - once again, similar results
 mod.fit.Bayes4 <- MCMClogit(formula = good ~ distance, data = placekick, seed = 2399,
  b0 = 0, B0 = 0, burnin = 10000, verbose = 10000, mcmc = 100000)
 summary(mod.fit.Bayes4)
 plot(mod.fit.Bayes4)






#