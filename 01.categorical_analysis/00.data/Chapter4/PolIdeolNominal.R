######################################################################
# NAME:  Tom Loughin                                                 #
# DATE:  2012-01-23                                                  #
# Purpose: Analyze political ideology 3-way table using Poisson      #
#     regression/Loglinear Models, treating all factors as nominal   #
# NOTES:                                                             #
######################################################################

#####################################################################
# Enter the data
alldata <- read.table("C:\\Data\\PolIdeolData.csv", sep = ",", header = TRUE)
head(alldata)

#####################################################################
# Fit Models

#Saturated Model: GPI
mod.sat <- glm(formula = count ~ gender*party*ideol, family = poisson(link = "log"), data = alldata)

#Homogeneous association model in all 3 associations: GP,GI,PI
mod.homo <- glm(formula = count ~ (gender + party + ideol)^2, family = poisson(link = "log"), data = alldata)
anova(mod.homo, mod.sat, test = "Chisq")

# Model assuming only PI association G,PI
mod.homo.PI <- glm(formula = count ~ gender + party*ideol, family = poisson(link = "log"), data = alldata)
anova(mod.homo.PI, mod.homo, test = "Chisq")

# Model assuming pairwise independence: G,P,I
mod.indep <- glm(formula = count ~ gender + party + ideol, family = poisson(link = "log"), data = alldata)
anova(mod.indep, mod.homo.PI, test = "Chisq")

# Summary and LR tests of homogeneous association model 
round(summary(mod.homo)$coefficients, digits = 3)
library(car)
Anova(mod.homo)

# Additional sub-models of homogeneous association
mod.homo.PIGI <- glm(formula = count ~ gender*ideol + party*ideol, family = poisson(link = "log"), data = alldata)
Anova(mod.homo.PIGI)
mod.homo.PIGP <- glm(formula = count ~ gender*party + party*ideol, family = poisson(link = "log"), data = alldata)
Anova(mod.homo.PIGP)

#####################################################################
# Confidence intervals: LR

# Already did math to determine which coefficients contribute to ORs.
# Develop contrast matrices for the different sets of ORs
# We split them into sets according to the interaction to better define a "family" for simultaneous inference
contr.mat.GP <- rbind(c(rep(0, 7), 1,0,0,0,0,0,0,0,0))
row.names(contr.mat.GP) <- c("GP Rep | M:F")
library(mcprofile)
LRCI <- mcprofile(mod.homo, CM = contr.mat.GP)
exp(confint(LRCI, adjust = "none"))
exp(confint(LRCI)) #Same as unadjusted here because only one OR in family

contr.mat.GI <- rbind(c(rep(0, 7), 0,-1, 0, 1, 0, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 0, 0, 1, 0, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 0,-1, 1, 0, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 0, 0, 1,-1, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 1, 0, 0, 0, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 1,-1, 0, 0, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 1, 0, 0,-1, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 0,-1, 0, 0, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 0, 0, 0,-1, 0, 0, 0, 0),
                      c(rep(0, 7), 0, 0, 1, 0,-1, 0, 0, 0, 0))
row.names(contr.mat.GI) <- c("GI VC:SC | M:F", "GI VC:M | M:F", "GI VC:SL | M:F", "GI VC:VL | M:F", "GI SC:M | M:F", "GI SC:SL | M:F", "GI SC:VL | M:F", "GI M:SL | M:F", "GI M:VL | M:F", "GI SL:VL | M:F")
LRCI <- mcprofile(mod.homo, CM = contr.mat.GI)
exp(confint(LRCI, adjust = "none"))
exp(confint(LRCI)) 

contr.mat.PI <- rbind(c(rep(0, 7), 0, 0, 0, 0, 0,-1, 0, 1, 0),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 0, 0, 1, 0),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 0,-1, 1, 0),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 0, 0, 1,-1),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 1, 0, 0, 0),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 1,-1, 0, 0),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 1, 0, 0,-1),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 0,-1, 0, 0),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 0, 0, 0,-1),
                      c(rep(0, 7), 0, 0, 0, 0, 0, 0, 1, 0,-1))
row.names(contr.mat.PI) <- c("PI VC:SC | R:D", "PI VC:M | R:D", "PI VC:SL | R:D", "PI VC:VL | R:D", "PI SC:M | R:D", "PI SC:SL | R:D", "PI SC:VL | R:D", "PI M:SL | R:D", "PI M:VL | R:D", "PI SL:VL | R:D")
LRCI <- mcprofile(mod.homo, CM = contr.mat.PI)
exp(confint(LRCI, adjust = "none"))
exp(confint(LRCI)) 

#####################################################################
# Wald Confidence intervals using multcomp

library(multcomp)
wald.GP <- glht(mod.homo, linfct = contr.mat.GP)
wald.GI <- glht(mod.homo, linfct = contr.mat.GI)
wald.PI <- glht(mod.homo, linfct = contr.mat.PI)

# Defaults use multiplicity adjustment for simultaneous confidence level
summary(wald.GP)
exp(confint(wald.GP)$confint)
# Options to get unadjusted (univariate) tests and CIs
summary(wald.GP, test = univariate())
exp(confint(wald.GP, calpha = qnorm(0.975))$confint)

# Defaults use multiplicity adjustment for simultaneous confidence level
summary(wald.GI)
exp(confint(wald.GI)$confint)
# Options to get unadjusted (univariate) tests and CIs
summary(wald.GI, test = univariate())
exp(confint(wald.GI, calpha = qnorm(0.975))$confint)

# Defaults use multiplicity adjustment for simultaneous confidence level
summary(wald.PI)
exp(confint(wald.PI)$confint)
# Options to get unadjusted (univariate) tests and CIs
summary(wald.PI, test = univariate())
exp(confint(wald.PI, calpha = qnorm(0.975))$confint)

#####################################################################
# Wald CIs by manual calculation

# Not doing multiplicity adjustments, so put all contrasts together into one matrix
contr.mat <- rbind(contr.mat.GP, contr.mat.GI, contr.mat.PI)
# Get out coefficients and variances
beta <- matrix(coef(mod.homo), ncol = 1)
v.beta <- vcov(mod.homo)
# Estimate Lin Combos and standard errors as matrix computations
log.contrasts <- contr.mat %*% beta
SElog.contrasts <- matrix(sqrt(diag(contr.mat %*% v.beta %*% t(contr.mat))), ncol = 1)
# Compute confidence intervals in linear predictor scale
alpha = 0.05
lower.log <- log.contrasts + qnorm(alpha/2)*SElog.contrasts
upper.log <- log.contrasts + qnorm(1 - alpha/2)*SElog.contrasts
# Combine Lin Combo coefficients, estimates of contrasts, and confidence intervals in mean scale
wald.ci <- round(data.frame(exp(log.contrasts), exp(lower.log), exp(upper.log)), digits = 2)
# Attach contrast names to and columns.                  
colnames(wald.ci) <- c("Estimate", "Lower CI", "Upper CI")
wald.ci






