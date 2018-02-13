#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  6-23-11                                                    #
# PURPOSE: Test for independence with data from                     #
#  http://lib.stat.cmu.edu/DASL/Stories/HighFiberDietPlan.html      #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Read in data

  diet <- read.csv(file = "C:\\data\\Fiber.csv")

  # Match order given at DASL
  diet$fiber<-factor(x = diet$fiber, levels = c("none", "bran", "gum", "both"))
  diet$bloat<-factor(x = diet$bloat, levels = c("none", "low", "medium", "high"))

  diet.table<-xtabs(formula = count ~ fiber + bloat, data = diet)
  diet.table

# BE CAREFUL - xtabs() re-orders.


#####################################################################
# Hypothesis tests for independence

  class(diet.table)
  summary(diet.table)

  ind.test<-chisq.test(x = diet.table, correct = FALSE)
  ind.test
  ind.test$expected
  ind.test$stdres  # Standardized Pearson residuals

  library(package = vcd)
  assocstats(x = diet.table)

  # Calculate -2log(Lambda) long way
     # Because log(0) is undefined, a small adjustment is made to these cells
     diet.table.adj<-diet.table  + ifelse(test = diet.table == 0, yes = 0.01, no = 0)
     # Because 0*log(0.01) = 0, the adjustment does not affect the final outcome
     test.stat<- 2*sum( diet.table * log(diet.table.adj/ind.test$expected) )
     test.stat

  qchisq(p = 0.95, df = 9)
  ind.test$residuals # Pearson residuals easier way
  abs(ind.test$residuals)>qnorm(p = 0.995)


#####################################################################
# Example of entering data into program

  c.table<-array(data = c(0, 5, 2, 0,
                          1, 3, 3, 2,
                          4, 2, 5, 4,
                          7, 2, 2, 6),
    dim=c(4,4), dimnames = list(fiber = c("bran", "gum", "combo", "control"),
       bloat = c("high", "medium", "low", "none")))
  c.table
  ind.test<-chisq.test(c.table, correct = FALSE)
  ind.test


#####################################################################
 #Monte Carlo simulation

  n<-sum(diet.table)  # Sample size
  pi.hat<-ind.test$expected/n  # Estimated expected pi^_ij under Ho
  pi.hat
  c(pi.hat)  # Notice the order here - need to know later for calc.stat()
  sum(pi.hat)  # Double check that it adds to 1
  

  B<-10000  # Number of simulated data sets
  set.seed(1298)
  table.count<-rmultinom(n = B, size = n, prob = pi.hat)  # Each column contains the counts for one contingency table
  table.count[,1:2]  # First two sets of contingency table counts

  rowMeans(table.count)/n  # Close to c(pi.hat), which shows the data simulated is o.k.

  # Function calculates X^2 for a data set and checks the contingency table size
  calc.stat<-function(data) {
    c.table<-array(data = data, dim=c(4,4), dimnames = list(fiber = c("bran", "gum", "combo", "control"),
       bloat = c("high", "medium", "low", "none")))
    ind.test<-chisq.test(c.table, correct = FALSE)
    ck.0.row<-sum(rowSums(c.table) == 0)  # Number of rows with all 0 entries
    ck.0.col<-sum(colSums(c.table) == 0)  # Number of columns with all 0 entries
    c(ind.test$statistic, ck.0.row, ck.0.col)
  }

  # Example application of the function (could do this with the observed data too)
  calc.stat(data = table.count[,1])

  # Calculate X^2* for each simulated data set
  save.star<-apply(X = table.count, MARGIN = 2, FUN = calc.stat)
  # The warnings are for "Chi-squared approximation may be incorrect"

  # We do not want to include contingency tables that have an all 0 row or all 0 column
  #  because this changes the contingency table size (and degrees of freedom for the chi^2 distribution)
  sum(save.star[2,])
  sum(save.star[3,])

  # The non 4x4 table
  c.table<-array(data = table.count[,691], dim=c(4,4), dimnames = list(fiber = c("bran", "gum", "combo", "control"),
       bloat = c("high", "medium", "low", "none")))

  # Just obtain the 4x4 tables
  X.sq.star<-save.star[1, save.star[2,] == 0 & save.star[3,] == 0]
  
  # Quantiles
  p<-c(0.01, 0.05, 0.10)
  quantile(x = X.sq.star, probs = 1 - p, type = 1)
  qchisq(p = 1 - p, df = 9)

  
  #####################################################################
  # Plots
  
    x11(width = 10, height = 6, pointsize = 12)
    # pdf(file = "c:\\figures\\Figure3.1color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book

    par(mfrow=c(1,2))

    # Histogram
    df<-9
    hist(x = X.sq.star, main = "Histogram", freq = FALSE, xlab = expression(X^"2*"))
    curve(expr = dchisq(x = x, df = df), col = "red", add = TRUE)
    # segments(x0 = ind.test$statistic, y0 = -10, x1 = ind.test$statistic, y1 = 10)

    # Compare CDFs
    plot.ecdf(x = X.sq.star, verticals = TRUE, do.p = FALSE, main = "CDFs", lwd = 2, col = "black",
            xlab = expression(X^"2*"), panel.first = grid(col="gray", lty="dotted"),
            ylab = "CDF")
    curve(expr = pchisq(q = x, df = df), col = "red", add = TRUE, lwd = 1)
    legend(x = 15, y = 0.4, legend = c(expression(X^"2*"), expression(chi[9]^2)), lwd = c(2,1),
      col = c("black", "red"), bty = "n")
    # dev.off()  # Create plot for book


    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure3.1BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    par(mfrow=c(1,2))
    hist(x = X.sq.star, main = "Histogram", freq = FALSE, xlab = expression(X^"2*"))
    curve(expr = dchisq(x = x, df = df), col = "black", add = TRUE)
    plot.ecdf(x = X.sq.star, verticals = TRUE, do.p = FALSE, main = "CDFs", lwd = 1, col = "black",
            xlab = expression(X^"2*"), ylab = "CDF")
    curve(expr = pchisq(q = x, df = df), col = "black", add = TRUE, lwd = 1, lty = "dotted")
    legend(x = 15, y = 0.4, legend = c(expression(X^"2*"), expression(chi[9]^2)), lwd = c(1,1),
      col = c("black", "black"), lty = c("solid", "dotted"), bty = "n")
    # dev.off()  # Create plot for book


    # QQ-Plot
    par(mfrow=c(1,1))
    B.adj<-length(X.sq.star)
    chi.quant<-qchisq(p = seq(from = 1/(B.adj+1), to = 1-1/(B.adj+1), by = 1/(B.adj+1)), df = df)
    plot(x = sort(X.sq.star), y = chi.quant, main = "QQ-Plot", xlab = expression(X^"2*"),
      ylab = expression(paste(chi[9]^2, "quantile")))
    abline(a = 0, b = 1)

  # Bootstrap p-value
  mean(X.sq.star > ind.test$statistic)  # bootstrap p-value


#####################################################################
# Multinomial regression model

  library(package = nnet)
  library(package = car)

  # Estimate mulitnomial regression model
  mod.fit.nom<-multinom(formula = bloat ~ fiber, weights = count, data = diet)
  summary(mod.fit.nom)
  logLik(mod.fit.nom)
  # Notice the different parameter estimates corresponding to 0 counts
  # mod.fit.nom<-multinom(formula = bloat ~ fiber, weights = count, data = diet, reltol = 10^(-12))
  # summary(mod.fit.nom)

  # LRT
  Anova(mod.fit.nom)

  # Another way for LRT
  mod.fit.Ho<-multinom(formula = bloat ~ 1, weights = count, data=diet)
  summary(mod.fit.Ho)

  # -2log(Lambda)
  mod.fit.Ho$deviance - mod.fit.nom$deviance  # Same test statistic as with Anova()


  # ORs
  round(exp(coefficients(mod.fit.nom)[,-1]), 2)
  exp(13.3561038 - (-4.1103893)) # bloating: high to none, fiber: gum vs. bran
  diet.table[1,1]*diet.table[2,2]/(diet.table[1,2]*diet.table[2,1]) # Matches (1,1) OR^
  diet.table[1,1]*diet.table[2,2]/(diet.table[1,2]*diet.table[2,1]) # Matches (3,1) OR^

  conf.beta<-confint(object = mod.fit.nom, level = 0.95)
  conf.beta
  ci.OR.low<-exp(conf.beta[2:4,1:2,1])
  ci.OR.medium<-exp(conf.beta[2:4,1:2,2])
  ci.OR.high<-exp(conf.beta[2:4,1:2,3])
  round(data.frame(low = ci.OR.low[,1], up = ci.OR.low[,2]), 2)
  # exp(log(6*4/(4*7)) + qnorm(c(0.025,0.975))*sqrt(1/6 + 1/4 + 1/4 + 1/7))  # Matches bran row
  round(data.frame(low = ci.OR.medium[,1], up = ci.OR.medium[,2]), 2)
  round(data.frame(low = ci.OR.high[,1], up = ci.OR.high[,2]), 2)


  # Estimated probabilities
  pi.hat<-predict(object = mod.fit.nom, newdata = diet[1:4,], type = "probs")
  pi.hat
  data.frame(fiber = diet[1:4,1], round(pi.hat,4))
  # Conditional probabilities
  diet.table<-xtabs(formula = count ~ fiber + bloat, data = diet)
  round(diet.table/rowSums(diet.table),4)


  # Using mcprofile - Can not be used - see error message produced
  library(package = mcprofile)
  K<-matrix(data = c(0, 1, -1,  0,  0, 0,  0,  0,  0, 0,  0,  0,
                     0, 1,  0, -1,  0, 0,  0,  0,  0, 0,  0,  0,
                     0, 0,  0,  0,  0, 1, -1,  0,  0, 0,  0,  0,
                     0, 0,  0,  0,  0, 1,  0, -1,  0, 0,  0,  0,
                     0, 0,  0,  0,  0, 0,  0,  0,  0, 1, -1,  0,
                     0, 0,  0,  0,  0, 0,  0,  0,  0, 1,  0, -1),  nrow = 6, ncol = 12, byrow = TRUE)
  linear.combo<-mcprofile(object = mod.fit.nom, CM = K)

  # Using multcomp - Can not be used due to the matrix form (rather than a vector) of coef(mod.fit.nom)
  library(package = multcomp)
  linear.combo<-glht(model = mod.fit.nom, linfct = K)  # Find the linear combinations and creates object for hypothesis tests and confidence intervals

  # Illustrates a model under independence
  mod.fit.nom.ind<-multinom(formula = bloat ~ 1, weights = count, data=diet, method = "logistic")
  summary(mod.fit.nom.ind)
  data.frame(diet, mod.fit.nom.ind$fitted.values)

  # Estimates under independence - pi^_+j
  colSums(c.table)/sum(c.table)


  # Add 0.5 to 0 cells and re-estimate model
  diet$count2<-ifelse(test = diet$count == 0, yes = diet$count + 0.5, no = diet$count)
  mod.fit.nom2<-multinom(formula = bloat ~ fiber, weights = count2, data = diet)
  sum.fit<-summary(mod.fit.nom2)
  names(sum.fit)
  round(sum.fit$coefficients, 4)
  round(sum.fit$standard.errors, 4)
  round(exp(coefficients(mod.fit.nom2)[,-1]), 2)
  conf.beta<-confint(object = mod.fit.nom2, level = 0.95)
  round(exp(conf.beta[2:4,,1]),1)  # compare low to no bloating
  round(exp(conf.beta[2:4,,2]),1)  # compare medium to no bloating
  round(exp(conf.beta[2:4,,3]),2)  # compare high to no bloating

  # Show how to use the VGAM package - note that the parameter estimates corresponding to
  #  log(pi_high/pi_none) are different from multinom() due to the zero cell counts in the table.
  #  The refLevel = 1 is used to let Y = 1 (none) be the base level. Without this argument, the refLevel = J.
  library(package = VGAM)
  mod.fit.nom3<-vglm(formula = bloat ~ fiber, family = multinomial(refLevel = 1),
    weights = count, data = diet[diet$count != 0,])
  summary(mod.fit.nom3)

  # This matches the parameter estimates and standard errors in from mod.fit.nom2
  mod.fit.nom4<-vglm(formula = bloat ~ fiber, family = multinomial(refLevel = 1),
    weights = count2, data = diet)
  summary(mod.fit.nom4)



#####################################################################
# Proportional odds model

  library(package = MASS)

  mod.fit.ord<-polr(formula = bloat ~ fiber, weights = count, data=diet, method = "logistic")
  summary(mod.fit.ord)
  Anova(mod.fit.ord)  # LRT
  
  # ORs
  round(exp(-coefficients(mod.fit.ord)), 2)
  round(1/exp(-coefficients(mod.fit.ord)), 2)
  
  conf.beta<-confint(object = mod.fit.ord, level = 0.95)
  ci<-exp(-conf.beta)
  round(data.frame(low = ci[,2], up = ci[,1]), 2)
  round(data.frame(low = 1/ci[,1], up = 1/ci[,2]), 2)

  diet$fiber2<-relevel(x = diet$fiber, ref = "bran")
  mod.fit.ord2<-polr(formula = bloat ~ fiber2, weights = count, data=diet, method = "logistic")
  conf.beta2<-confint(object = mod.fit.ord2, level = 0.95)
  ci2<-exp(-conf.beta2)
  round(data.frame(low = ci2[,2], up = ci2[,1]), 2)
  round(data.frame(low = 1/ci2[,1], up = 1/ci2[,2]), 2)

  # Illustrates a model under independence
  mod.fit.ord.ind<-polr(formula = bloat ~ 1, weights = count, data=diet, method = "logistic")
  summary(mod.fit.ord.ind)
  data.frame(diet, mod.fit.ord.ind$fitted.values)

  # Estimates under independence - pi^_+j
  colSums(c.table)/sum(c.table)


  ################################
  # Estimate probability of being in a particular category

    pi.hat.ord<-predict(object = mod.fit.ord, newdata = diet[1:4,], type = "probs")
    pi.hat.ord
    data.frame(fiber = diet[1:4,1], round(pi.hat.ord,4))

    # Conditional probabilities
    diet.table<-xtabs(formula = count ~ fiber + bloat, data = diet)
    round(diet.table/rowSums(diet.table),4)

    # ORs
    round(exp(-mod.fit.ord$coefficients),2)
    round(1/exp(-mod.fit.ord$coefficients),2)
    conf.beta<-confint(object = mod.fit.ord, level = 0.95)
    ci<-exp(-conf.beta)
    round(data.frame(low = ci[,2], up = ci[,1]), 2)
    round(data.frame(low = 1/ci[,1], up = 1/ci[,2]), 2)


#####################################################################
# vglm() in VGAM package for proportional odds model

  library(package = VGAM)  # If this has not been done already
  
  mod.fit.po<-vglm(formula = bloat ~ fiber, family = cumulative(parallel = TRUE),
    weights = count, data = diet[diet$count != 0,])
  summary(mod.fit.po)
  #options(width = 65)  # Formatting for book - 60 characters per line
  slotNames(mod.fit.po)  # Like names( ) in S3
  mod.fit.po@coefficients
  options(width = 80)
  mod.fit.po@df.residual
  #showMethods(class = "vglm") #Like method(class = " ") in S3

  mod.fit.npo<-vglm(formula = bloat ~ fiber, family = cumulative(parallel = FALSE),
    weights = count, data = diet[diet$count != 0,])
  #options(width = 60)  # Formatting for book - 60 characters per line
  #summary(mod.fit.npo)  # Not shown to save space
  round(mod.fit.npo@coefficients, 2)
  options(width = 80)

  #anova(mod.fit.po, mod.fit.npo)

  tran.LR<-deviance(mod.fit.po) - deviance(mod.fit.npo)
  df<-mod.fit.po@df.residual - mod.fit.npo@df.residual
  p.value<-1 - pchisq(q = tran.LR, df = df)
  data.frame(tran.LR, df, p.value)

  lrtest(mod.fit.npo, mod.fit.po) # Also gives the test

  # This also estimates the proportional odds model
  mod.fit.temp<-vglm(formula = bloat ~ fiber, family = propodds,
    weights = count, data = diet[diet$count != 0,])
  summary(mod.fit.temp)

  # Example of estimating pi_j
  predictvglm(object = mod.fit.po, newdata = data.frame(fiber = "bran"))
  predictvglm(object = mod.fit.po, newdata = data.frame(fiber = "bran"), type = "response")
  predictvglm(object = mod.fit.po, newdata = data.frame(fiber = "bran"), se.fit = TRUE)



#####################################################################
# Other ordinal models

  library(package = MASS)

  mod.fit.ord.probit<-polr(formula = bloat ~ fiber, weights = count, data=diet, method = "probit")
  summary(mod.fit.ord.probit)


#####################################################################
# Treat data as coming from a 2 factorial experiment

  diet$bran<-factor(ifelse(test = diet$fiber == "bran" | diet$fiber == "both", yes = "yes", no = "no"))
  diet$gum<-factor(ifelse(test = diet$fiber == "gum" | diet$fiber == "both", yes = "yes", no = "no"))
  diet
  
  #Multinomial regression model
  #mod.fit.nom.main<-multinom(formula = bloat ~ bran + gum, weights = count, data = diet)
  #summary(mod.fit.nom.main)

  # Multinomial regression model
  mod.fit.nom.inter<-multinom(formula = bloat ~ bran + gum + bran:gum, weights = count, data = diet)
  mod.fit.nom.inter<-multinom(formula = bloat ~ bran + gum + bran:gum, weights = count, data = diet,
    reltol = 10^(-12))
  summary(mod.fit.nom.inter)
  logLik(mod.fit.nom.inter)

  # Find df for residual deviance - Because df is not given by summary(mod.fit.nom.inter),
  #  we use anova() with this model and a separate model (simply, the model under complete ind.)
  #  to show the df
  mod.fit.temp<-multinom(formula = bloat ~ 1, weights = count, data = diet)
  summary(mod.fit.temp)
  anova(mod.fit.temp, mod.fit.nom.inter)

  # LRT
  library(package = car)  # If not done already
  Anova(mod.fit.nom.inter)

  # Observed and predicted counts are equal
  set1<-data.frame(diet[,-1], round(predict(object = mod.fit.nom.inter, type = "probs"),4) )
  set1[,5:8]<-set1[,5:8]*12
  set1$predict<-c(set1$high[1:4], set1$medium[5:8], set1$low[9:12], set1$none[13:16])
  set2<-set1[,-c(5:8)]
  set2
  

  # vlgm from VGAM
  # [diet$count != 0,]
  mod.fit.nom.inter2<-vglm(formula = bloat ~ bran + gum + bran:gum, family = multinomial(refLevel = 1),
    weights = count, data = diet[diet$count != 0,])
  mod.fit.nom.inter2<-vglm(formula = bloat ~ 1, family = multinomial(refLevel = 1),
    weights = count, data = diet[diet$count != 0,])
  summary(mod.fit.nom.inter2)
  round(predictvglm(object = mod.fit.nom.inter2, newdata = diet, type = "response"),4)

  # Proportional odds regression model
  mod.fit.ord.inter<-polr(formula = bloat ~ bran + gum + bran:gum, weights = count, data = diet, method = "logistic")
  summary(mod.fit.ord.inter)
  Anova(mod.fit.ord.inter)



#####################################################################
# Likelihood function evaluated at parameter values for model in mod.fit.nom.inter

  # Matrix of explanatory variable values expanded to have 48 rows to match the 48 observations
  X1<-model.matrix(mod.fit.nom.inter)
  X2<-X1[rep(1:nrow(X1), times = diet$count),]   # Idea from http://tolstoy.newcastle.edu.au/R/e2/help/07/03/12176.html

  # Matrix of responses expanded to have 48 rows to match the 48 observations
  high<-ifelse(test = diet$bloat == "high", yes = 1, no = 0)
  medium<-ifelse(test = diet$bloat == "medium", yes = 1, no = 0)
  low<-ifelse(test = diet$bloat == "low", yes = 1, no = 0)
  none<-ifelse(test = diet$bloat == "none", yes = 1, no = 0)
  resp16<-cbind(none, low, medium, high)
  resp<-resp16[rep(1:nrow(resp16), times = diet$count),]
  
  # Calculate estimated probabilities
  den<-1+exp(X2%*%coefficients(mod.fit.nom.inter)[1,]) + exp(X2%*%coefficients(mod.fit.nom.inter)[2,]) +
     exp(X2%*%coefficients(mod.fit.nom.inter)[3,])
  pi1<-1/den  # None
  pi2<-exp(X2%*%coefficients(mod.fit.nom.inter)[1,])/den  # low
  pi3<-exp(X2%*%coefficients(mod.fit.nom.inter)[2,])/den  # medium
  pi4<-exp(X2%*%coefficients(mod.fit.nom.inter)[3,])/den  # high
  # cbind(pi1, pi2, pi3, pi4)

  LogL<-sum(resp[,1]*log(pi1) + resp[,2]*log(pi2) + resp[,3]*log(pi3) + resp[,4]*log(pi4))
  LogL  # Matches multinom()
  logLik(mod.fit.nom.inter)






















#