#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 09-11-2013                                                  #
# PURPOSE: Analysis of head impact for falls using GEE              #
#                                                                   #
# NOTES:                                                            #
#####################################################################
options(width = 60)  # Formatting for book - 60 characters per line

#####################################################################
# Read the data

# Read the data from the subsetted file
fall.head <- read.table("C:\\Data\\FallHead.csv", header = TRUE, sep = ",", na.strings = " ")
head(fall.head)

##############################################################
# Model fitting

library(geepack)
# Need to sort data in order of the clusters first. 
fall.head.o <- fall.head[order(fall.head$resident),]

# Need to specify scale.fix = TRUE, or else quasi-binomial will be fit.
mod.gee.i <- geeglm(formula = head ~ initial, id = resident, data = fall.head.o, scale.fix = TRUE, family = binomial(link = "logit"), corstr = "independence")
summ <- summary(mod.gee.i)
summ
names(summ)
summ$cov.scaled

# anova() method performs Wald chi-square test.  
anova(mod.gee.i)

# Recreate anova Wald statistic
(ll <- cbind(c(0,0,0), diag(c(1,1,1))))
(bhat <- summ$coefficients[,1])

(W <- t(ll %*% bhat) %*% solve( ll %*% summ$cov.scaled %*% t(ll), diag(c(1,1,1))) %*% (ll %*% bhat))


###################################################################
# Wald Pairwise comparisons as in FallsGLMM.R

# Defining a vcov() mmethod for use in multcomp. Currently doesn't exist.
vcov(mod.gee.i)  # Fails
summ$cov.scaled  # This is what we want

vcov.geeglm <- function(obj){summary(obj)$cov.scaled}
vcov(mod.gee.i)  # Now we get it!

library(multcomp)

K <- rbind("B-F" = c(0, 1, 0, 0),
      "S-F" = c(0, 0, 1, 0),
      "D-F" = c(0, 0, 0, 1),
      "S-B" = c(0, -1, 1, 0),
      "D-B" = c(0, -1, 0, 1),
      "D-S" = c(0, 0, -1, 1))

pw.comps <- glht(mod.gee.i, linfct = K)
# Tests using Individual error rates = 0.95
summary(pw.comps, test = adjusted("none"))
# Tests using Familywise error tates = 0.95
summary(pw.comps)
# Confidence intervals using Individual error rates = 0.95
ci.logit.I <- confint(pw.comps, calpha = qnorm(0.975))
round(exp(ci.logit.I$confint),2)
# Confidence intervals using Familywise error rates = 0.95
ci.logit.F <- confint(pw.comps)
round(exp(ci.logit.F$confint),2)

####################################################################
# Confidence intervals for probabilities of head impact
K.fit <- rbind("Forwards"  = c(1, 0, 0, 0),
      "Backwards" = c(1, 1, 0, 0),
      "Sideways"  = c(1, 0, 1, 0),
      "Down"    = c(1, 0, 0, 1))

fits <- glht(mod.gee.i, linfct = K.fit)
# Confidence intervals using Individual error rates = 0.95
ci.eta.I <- confint(fits, calpha = qnorm(0.975))
round(plogis(ci.eta.I$confint),3)
# Confidence intervals using Familywise error rates = 0.95
ci.eta.F <- confint(fits)
round(plogis(ci.eta.F$confint),3)

###################################################################
# Refit using exchangeable correlation structure 
# (equal correlation for all falls within a subject)
mod.gee.e <- geeglm(formula = head ~ initial, id = resident, data = fall.head.o, scale.fix = TRUE, family = binomial(link = "logit"), corstr = "exchangeable")
summary(mod.gee.e)


