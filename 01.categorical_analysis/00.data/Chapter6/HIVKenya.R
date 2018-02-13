###########################################################################
# NAME: Chris Bilder                                                      #
# DATE: 5-1-13                                                            #
# PURPOSE: HIV Kenya data analysis                                        #
# NOTES:                                                                  #
###########################################################################

set1 <- read.csv(file = "c:\\data\\HIVKenya.csv")
head(set1)

Se <- 0.98  # non-perfect testing
Sp <- 0.98
# Se <- 1  # Use perfect testing to confirm that one obtains the same answer with all model fitting methods
# Sp <- 1


###########################################################################
# Estimate the model using glm() and no testing error

 mod.fit <- glm(formula = hiv ~ age, data = set1, family = binomial(link = logit))
 round(summary(mod.fit)$coefficients, 4)
 logLik(mod.fit)
 X <- model.matrix(mod.fit)
 # library(package = car)
 # Anova(mod.fit)


###########################################################################
# Estimate the model using optim()

 logL <- function(beta, X, Y, Se, Sp) {
  # pi.tilde <- exp(X%*%beta)/(1+exp(X%*%beta))  # Same as plogis()
  pi.tilde <- plogis(X%*%beta)
  # Non-matrix algebra alternative for an intercept and one explanatory variable
  # pi.tilde <- exp(beta[1] + beta[2]*X[,2])/(1+exp(beta[1] + beta[2]*X[,2]))
  pi <- Se*pi.tilde + (1 - Sp)*(1 - pi.tilde)
  sum(Y*log(pi) + (1-Y)*log(1-pi))
 }

 mod.fit.opt <- optim(par = mod.fit$coefficients, fn = logL, hessian = TRUE,
  X = X, Y = set1$hiv, control = list(fnscale = -1), Se = Se, Sp = Sp, method = "BFGS")
 mod.fit.opt$par # beta.hats
 mod.fit.opt$value # log(L)
 mod.fit.opt$convergence # 0 means converged
 cov.mat <- -solve(mod.fit.opt$hessian) # Estimated covariance matrix; multiply by -1 because of fnscale
 cov.mat
 sqrt(diag(cov.mat)) # SEs
 z <- mod.fit.opt$par[2]/sqrt(diag(cov.mat))[2] # Wald statistic
 2*(1-pnorm(q = abs(z))) # p-value

 # Compare to optim results
 summary(mod.fit)$coefficients


################################################################################
# Estimate the model using the m.logit() function and glm()

 # Used posting at https://stat.ethz.ch/pipermail/r-help/2006-April/103799.html and the work of
 #  Boan Zhang in Zhang, Bilder, and Tebbs (Statistics in Medicine, 2013) for motivation
 # mu = E(Y) = pi
 my.link <- function(Se, Sp) {
  linkfun <- function(mu) {
   pi.tilde <- (mu + Sp - 1)/(Se + Sp - 1)
   log(pi.tilde/(1-pi.tilde))
  }
  linkinv <- function(eta) {
   (exp(eta)*Se - Sp + 1)/(1 + exp(eta))
  }
  mu.eta <- function(eta) {
   exp(eta)*(Se + Sp - 1)/(1 + exp(eta))^2
  }
  save.it <- list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta)
  class(save.it) <- "link-glm"
  save.it
 }

 mod.fit2 <- glm(formula = hiv ~ age, data = set1, family = binomial(link = my.link(Se, Sp)))
 round(summary(mod.fit2)$coefficients, 4)




#