#####################################################################
# NAME:  Chris Bilder  / Tom Loughin                                #
# DATE:  12-30-11 / 5-17-12                                         #
# PURPOSE: Analyze tomato virus data                                #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Read in data

  tomato <- read.csv(file  = "C:\\data\\TomatoVirus.csv")
  head(tomato)
  tomato
  

#####################################################################
# Estimate model

  # Check out the Control variable
  class(tomato$Control)
  levels(tomato$Control)
  contrasts(tomato$Control)

  # Check out the Infest explanatory variable
  class(tomato$Infest)
  levels(tomato$Infest)
  class(factor(tomato$Infest))
  levels(factor(tomato$Infest))
  contrasts(factor(tomato$Infest))
  
  # Change Infest to a factor class
  tomato$Infest<-factor(tomato$Infest)
  head(tomato)
  class(tomato$Infest)

  # Additional examples of working with the factor() function
  temp<-factor(tomato$Control, levels = c("C", "B", "N"))  # Change ordering of levels like relevel()
  levels(temp)
  temp2<-factor(tomato$Control, labels = c("B1", "C1", "N1"))  # Change names of levels
  levels(temp2)


  # Estimate the model
  mod.fit<-glm(formula = Virus8/Plants ~ Infest + Control, family = binomial(link = logit), data = tomato,
    weights = Plants)
  summary(mod.fit)

  mod.fit$xlevels  # Another way to see the levels of categorical explanatory variables

  # Estimate the model with an interaction
  mod.fit.inter<-glm(formula = Virus8/Plants ~ Infest + Control + Infest:Control, family = binomial(link = logit), data = tomato,
    weights = Plants)
  summary(mod.fit.inter)

  # LRT
  library(package = car)
  Anova(mod.fit.inter)



#####################################################################
# Odds ratios for model without interaction term

  #############################
  # Estimated OR for Infest

    exp(mod.fit$coefficients[2])
    1/exp(mod.fit$coefficients[2])
    # as.numeric(exp(mod.fit$coefficients[2]))
    # attributes(mod.fit$coefficients[2])

    # Wald interval
    exp(confint.default(object = mod.fit, parm = "Infest2", level = 0.95))
    # as.numeric(exp(confint.default(object = mod.fit, parm = "Infest2", level = 0.95)))

    # Profile likelihood ratio interval
    exp(confint(object = mod.fit, parm = "Infest2", level = 0.95))


  #############################
  # Estimated OR for Control

    exp(mod.fit$coefficients[3:4])

    # Wald interval
    exp(confint.default(object = mod.fit, parm = c("ControlC", "ControlN"), level = 0.95))

    # Profile likelihood ratio interval
    exp(confint(object = mod.fit, parm = c("ControlC", "ControlN"), level = 0.95))

    # Wald interval for Control N vs. Control C
    beta.hat<-mod.fit$coefficients[-1]  # Matches up beta indices with [i] to help avoid mistakes
    exp(beta.hat[3] -  beta.hat[2])
    cov.mat<-vcov(mod.fit)[2:4,2:4]
    var.N.C<-cov.mat[3,3] + cov.mat[2,2] - 2*cov.mat[3,2]
    CI.betas<- beta.hat[3]- beta.hat[2] + qnorm(p = c(0.025, 0.975))*sqrt(var.N.C)
    exp(CI.betas)

    # A little easier way
    tomato$Control.reorder<-relevel(x = tomato$Control, ref = "C")
    mod.fit2<-glm(formula = Virus8/Plants ~ Infest + Control.reorder, family = binomial(link = logit), data = tomato,
      weight = Plants)
    # summary(mod.fit2)
    exp(confint.default(object = mod.fit2, parm = c("Control.reorderB", "Control.reorderN"), level = 0.95))
    exp(confint(object = mod.fit2, parm = c("Control.reorderB", "Control.reorderN"), level = 0.95))


    # Using mcprofile
    library(package = mcprofile)
    K<-matrix(data = c(0, 0,  1,  0,
                       0, 0,  0,  1),  nrow = 2, ncol = 4, byrow = TRUE)
    linear.combo<-mcprofile(object = mod.fit, CM = K)
    ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
    ci.log.OR
    comparison<-c("C vs. B", "N vs. B")
    data.frame(comparison, OR = exp(ci.log.OR$confint))  # These match previous results

    # N vs. C - Can also include in the above code too
    K<-matrix(data = c(0, 0, -1,  1),  nrow = 1, ncol = 4, byrow = TRUE)
    linear.combo<-mcprofile(object = mod.fit, CM = K)
    ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
    ci.log.OR
    data.frame(comparison = "N vs. C", OR = exp(ci.log.OR$confint))  # These match previous results
    save.wald<-wald(object = linear.combo)
    ci.logit.wald<-confint(object = save.wald, level = 0.95, adjust = "none")
    data.frame(lower = exp(ci.logit.wald$confint[,1]), upper = exp(ci.logit.wald$confint[,2]))  # These match previous results


#####################################################################
# Odds ratios for model with interaction

  #############################
  # Estimated OR for Control

    # Extract the beta^'s so that the resulting vector will have the same indices as the subscripts
    #  for the parameter estimates
    beta.hat<-mod.fit.inter$coefficients[-1]

    N.B.Infest2.0<-exp(beta.hat[3])
    N.B.Infest2.1<-exp(beta.hat[3] + beta.hat[5])
    C.B.Infest2.0<-exp(beta.hat[2])
    C.B.Infest2.1<-exp(beta.hat[2] + beta.hat[4])
    N.C.Infest2.0<-exp(beta.hat[3] - beta.hat[2])
    N.C.Infest2.1<-exp(beta.hat[3] - beta.hat[2] + beta.hat[5] - beta.hat[4])
    comparison<-c("N vs. B", "N vs. B", "C vs. B", "C vs. B", "N vs. C", "N vs. C")
    data.frame(Infest2 = c(0, 1, 0, 1, 0, 1),
               Control = comparison,
               OR.hat = round(c(N.B.Infest2.0, N.B.Infest2.1, C.B.Infest2.0, C.B.Infest2.1,
                 N.C.Infest2.0, N.C.Infest2.1),2))

    # Using mcprofile
    # library(package = mcprofile)  # If had not done yet
    K<-matrix(data = c(0, 0,  0,  1,  0,  0,
                       0, 0,  0,  1,  0,  1,
                       0, 0,  1,  0,  0,  0,
                       0, 0,  1,  0,  1,  0,
                       0, 0, -1,  1,  0,  0,
                       0, 0, -1,  1, -1,  1),  nrow = 6, ncol = 6, byrow = TRUE)
    linear.combo<-mcprofile(object = mod.fit.inter, CM = K)
    ci.log.OR<-confint(object = linear.combo, level = 0.95, adjust = "none")
    ci.log.OR
    data.frame(Infest2 = c(0, 1, 0, 1, 0, 1), comparison, OR = round(exp(ci.log.OR$estimate),2),
       OR.CI = round(exp(ci.log.OR$confint),2))
    # Bonferroni
    ci.log.bon<-confint(object = linear.combo, level = 0.95, adjust = "bonferroni")
    # Single Step
    ci.log.ss<-confint(object = linear.combo, level = 0.95, adjust = "single-step")
    data.frame(Infest2 = c(0, 1, 0, 1, 0, 1), comparison, bon = round(exp(ci.log.bon$confint),2), ss = round(exp(ci.log.ss$confint),2))
  

    save.wald<-wald(object = linear.combo)
    ci.logit.wald<-confint(object = save.wald, level = 0.95, adjust = "none")
    data.frame(Infest2 = c(0, 1, 0, 1, 0, 1), comparison, OR = round(exp(ci.log.OR$estimate),2),
      lower = round(exp(ci.logit.wald$confint[,1]),2), upper = round(exp(ci.logit.wald$confint[,2]),2))
    # Bonferroni
    ci.wald.bon<-confint(object = save.wald, level = 0.95, adjust = "bonferroni")
    # Single Step
    ci.wald.ss<-confint(object =save.wald, level = 0.95, adjust = "single-step")
    data.frame(Infest2 = c(0, 1, 0, 1, 0, 1), comparison, bon = round(exp(ci.wald.bon$confint),2), ss = round(exp(ci.wald.ss$confint),2))
  

    K<-matrix(data = c(0, 0,  0,  1,  0,  0,
                       0, 0,  0,  1,  0,  1,
                       0, 0,  1,  0,  0,  0,
                       0, 0,  1,  0,  1,  0),  nrow = 4, ncol = 6, byrow = TRUE)   # No warnings
    mcprofile(object = mod.fit.inter, CM = K)









#
