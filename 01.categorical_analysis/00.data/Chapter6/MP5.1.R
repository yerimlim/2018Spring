#####################################################################
# NAME: Chris Bilder                                                #
# DATE: 5-17-13                                                     #
# PURPOSE: Example 5.1 of Mehta and Patel (1995)                    #
#                                                                   #
# NOTES:                                                            #
#####################################################################

# Number disease free (w) out of total (n) per explanatory variable pattern
set1 <- data.frame(LI = c(0,0,0,0, 1,1,1,1), gender = c(0,0,1,1,0,0,1,1),
 AOP = c(0,1,0,1,0,1,0,1), w = c(3,2,4,1,5,3,5,6), n = c(3,2,4,1,5,5,9,17))
head(set1)
sum(set1$n)  # Sample size
sum(set1$w)

# Transform data to one person per row (Bernoulli format)
set1.y1 <- set1[rep(1:nrow(set1), times = set1$w), -c(4:5)]
set1.y1$y <- 1
set1.y0 <- set1[rep(1:nrow(set1), times = set1$n-set1$w), -c(4:5)]
set1.y0$y <- 0
set1.long <- data.frame(rbind(set1.y1, set1.y0), row.names = NULL)
nrow(set1.long)  # Sample size
sum(set1.long$y)
head(set1.long)
tail(set1.long)

ftable(formula = y ~ LI + gender + AOP, data = set1.long)
# ftable(x = set1.long)  # Same


#########################################################################
# Regular logistic regression

 mod.fit <- glm(formula = w/n ~ LI + gender + AOP, data = set1, family = binomial(link = logit),
  weights = n, trace = TRUE, epsilon = 1e-8)  # Default value of epsilon specified
 # summary(mod.fit)
 round(summary(mod.fit)$coefficients, 4)

 # A strictor convergence criteria shows the non-convergence
 mod.fit <- glm(formula = w/n ~ LI + gender + AOP, data = set1, family = binomial(link = logit),
  weights = n, trace = TRUE, epsilon = 0.00000000001)
 # summary(mod.fit)
 round(summary(mod.fit)$coefficients, 4)

 # Fitting the model to the Bernoulli data format
 mod.fit <- glm(formula = y ~ LI + gender + AOP, data = set1.long, family = binomial(link = logit),
  trace = TRUE)
 summary(mod.fit)



#########################################################################
# logistiX

 library(package = logistiX)

 mod.fit.logistiX <- logistiX(x = set1.long[,1:3], y = set1.long[,4], alpha = 0.05)
 summary(mod.fit.logistiX)  # No intercept, but otherwise very similar to Table 3 in the paper
 # The p-value is a little different because they are using a little different testing method
 names(mod.fit.logistiX)
 mod.fit.logistiX$estout  # Estimates from four different methods
 mod.fit.logistiX$ciout   # CIs for four different methods, TST-Pmid row gives the mid-p correction
 mod.fit.logistiX$distout  # varnum = 1 matches Table 2 in the paper
 mod.fit.logistiX$tobs   # Observed values of the sufficient statistics

 # Exact distribution for sufficient statistic of beta1
 just.for.beta1 <- mod.fit.logistiX$distout$varnum == 1
 distL1 <- mod.fit.logistiX$distout[just.for.beta1, ]
 distL1$rel.freq <- round(distL1$counts/sum(distL1$counts), 4)
 distL1
 mod.fit.logistiX$tobs[2]  # Sufficient stat for beta1 - t1 = 19 (counts for it are distL1$count[1])
 distL1$count[1]/sum(distL1$counts)  # One-sided p-value bottom p. 2151
 sum(distL1$counts[c(1,6:8)])/sum(distL1$counts)   # Two-tail test, matches Table 3

 # Exact distribution for sufficient statistic of beta2
 just.for.beta2 <- mod.fit.logistiX$distout$varnum == 2
 distgender <- mod.fit.logistiX$distout[just.for.beta2, ]
 distgender$rel.freq <- round(distgender$counts/sum(distgender$counts), 4)
 distgender
 mod.fit.logistiX$tobs[3]  # Sufficient stat for beta2 - t2 = 16
 sum(distgender$count[1:3])/sum(distgender$counts)  # One-sided p-value
 sum(distgender$counts[c(1:3,8:10)])/sum(distgender$counts)   # Two-tail test, matches Table 3
 2*sum(distgender$count[1:3])/sum(distgender$counts)  # P-value given by summary()

 # mid-p - see "TST-Pmid" rows in mod.fit.logistiX$ciout
 pmf.gender <- distgender$count/sum(distgender$counts)  # PMF
 2*(sum(pmf.gender[1:2]) + 0.5*pmf.gender[3])

 # Exact CI
 confint(object = mod.fit.logistiX, level = 0.95, type = "exact")  # Another way to extract CI
 
 x11()
 plot(x = mod.fit.logistiX, var = 1)  # Exact distribution of sufficient statistic
 x11()
 barplot(height = distL1$counts/sum(distL1$counts), names.arg = distL1$t.stat)  # Same

 
#########################################################################
# elrm

 library(package = elrm)

 # set.seed(8718)  # This does not help you reproduce the same sample.
 #  The sampling is performed by a C program called by elrm(). It looks like
 #  no seed number is passed into this program.
 mod.fit.elrm1 <- elrm(formula = w/n ~ LI + gender + AOP, interest = ~ LI, iter = 101000,
  dataset = set1, burnIn = 1000, alpha = 0.05)
 summary(mod.fit.elrm1)
 
 # Estimate of exact distribution corresponding to beta1's sufficient statistic
 mod.fit.elrm1$distribution  # Similar to what was obtained by logistiX, but just in a different order
 sum(mod.fit.elrm1$distribution$LI[1:3,2])  # Two tail test
 mod.fit.elrm1$distribution$LI[3,2]  # Left-tail test


 plot(mod.fit.elrm1)
 options(width = 60)
 names(mod.fit.elrm1)
 options(width = 115)
 mod.fit.elrm1$coeffs
 mod.fit.elrm1$obs.suff.stat
 sum(set1$y*set1$x1)
 # mod.fit.elrm1$distribution$LI[,2]
 # mod.fit.elrm1$distribution$LI[,1] == 19


#########################################################################
# Firth

 library(package = logistf)
 mod.fit.firth <- logistf(formula = y ~ LI + gender + AOP, data = set1.long)
 mod.fit.firth
 summary(mod.fit.firth)




#
