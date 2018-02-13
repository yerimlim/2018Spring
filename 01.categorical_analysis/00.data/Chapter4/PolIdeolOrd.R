######################################################################
# NAME:  Tom Loughin                                                 #
# DATE:  2012-01-23                                                  #
# Purpose: Analyze political ideology 3-way table using Poisson      #
#     regression/Loglinear Models, treating Ideology as Ordinal      #
# NOTES:                                                             #
######################################################################

# Political Ideology Example

# Enter the data
alldata <- read.table("C:\\Data\\PolIdeolData.csv", sep = ",", header = TRUE)
lin.score1 <- c(rep(x = c(1,2,3,4,5), times = 4))
lin.score2 <- c(rep(x = c(0,2,3,4,6), times = 4))
extrm.score <- c(rep(x = c(2,1,0,1,2), times = 4))
alldata <- data.frame(alldata, lin.score1, lin.score2, extrm.score)
head(alldata)


# Homogeneous, Linear in PI: Score Set 1-2-3-4-5
mod.homo.lin1.PI <- glm(formula = count ~ gender*party + gender*ideol + party*lin.score1, family = poisson(link = "log"), data = alldata)
summary(mod.homo.lin1.PI)
library(car)
Anova(mod.homo.lin1.PI)

# Print out model matrix to understand the singularity on linear association model
model.matrix(mod.homo.lin1.PI)

# Now estimate associations for Linear Association model and compute Wald CIs
contr.mat <- rbind(c(rep(0, 7), 1, 0, 0, 0, 0, 0), 
                   c(rep(0, 7), 0,-1, 0, 1, 0, 0),
                   c(rep(0, 7), 0, 0, 0, 1, 0, 0),
                   c(rep(0, 7), 0, 0,-1, 1, 0, 0),
                   c(rep(0, 7), 0, 0, 0, 1,-1, 0),
                   c(rep(0, 7), 0, 1, 0, 0, 0, 0),
                   c(rep(0, 7), 0, 1,-1, 0, 0, 0),
                   c(rep(0, 7), 0, 1, 0, 0,-1, 0),
                   c(rep(0, 7), 0, 0,-1, 0, 0, 0),
                   c(rep(0, 7), 0, 0, 0, 0,-1, 0),
                   c(rep(0, 7), 0, 0, 1, 0,-1, 0),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 1),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 2),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 3),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 4)
                   )

# Usual method for Wald intervals creates error message:
#  "Error in modelparm.default(model, ...) : 
#    dimensions of coefficients and covariance matrix don't match"
#  because of extra parameter with missing value in coefficients that is not in variance.
# We can remove this parameter using the code below: 

# First, we use is.na(mod.homo.lin1.PI$coefficients) to identify which elements of the 
#  coefficient vector are NA. The result is TRUE if the element is NA and FALSE otherwise. 

# Second, we reverse the TRUE and FALSE responses by using !, so that the positions of 
#  the non-NA elements are now given by TRUE. 

# Finally, we have R return only the non-NA elements and put them back in the original 
#  component of the mod.homo.lin1.PI object. 

# Note that the estimated covariance matrix given by vcov(mod.homo.lin1.PI) does not contain 
#  lin.score1 so we do not need to perform the same steps for it. 

# With the model fit object amended in this way, glht() works However, mcprofile() still 
#  will not work. 

check.na <- is.na(mod.homo.lin1.PI$coefficients)
mod.homo.lin1.PI$coefficients <- mod.homo.lin1.PI$coefficients[!check.na]

library(multcomp)
wald <- glht(mod.homo.lin1.PI, linfct = contr.mat)
wald.ci <- round(exp(confint(wald, calpha = qnorm(0.975))$confint), 2)
row.names(wald.ci) <- c("GP Rep | M:F", "GI VC:SC | M:F", "GI VC:M | M:F", "GI VC:SL | M:F", "GI VC:VL | M:F", "GI SC:M | M:F", "GI SC:SL | M:F", "GI SC:VL | M:F", "GI M:SL | M:F", "GI M:VL | M:F", "GI SL:VL | M:F", "PI REP | 1 Cat Ideol", "PI REP | 2 Cat Ideol", "PI REP | 3 Cat Ideol", "PI REP | 4 Cat Ideol")
colnames(wald.ci) <- c("Estimate", "Lower CI", "Upper CI")
wald.ci

# This fix does not help with making mcprofile work.  See error messages.

library(mcprofile)
LRCI <- mcprofile(mod.homo.lin1.PI, CM = contr.mat)
exp(confint(LRCI, adjust = "none")) # Unadjusted for multiplicity



# Wald CIs by manual calculation

# Get out coefficients and variances
# NOTE THAT THE PARAMETER ESTIMATES HAVE ALREADY HAD THE MISSING VALUE REMOVED.
beta <- matrix(coef(mod.homo.lin1.PI), ncol = 1)
# Note that the variance-covariabce matrix does NOT include the singularity, so no deletions are needed
v.beta <- vcov(mod.homo.lin1.PI)
# Estimate Lin Combos and standard errors as matrix computations
log.contrasts <- contr.mat %*% beta
SElog.contrasts <- matrix(sqrt(diag(contr.mat %*% v.beta %*% t(contr.mat))), ncol = 1)
# Compute confidence intervals in linear predictor scale
alpha = 0.05
lower.log <- log.contrasts + qnorm(alpha/2)*SElog.contrasts
upper.log <- log.contrasts + qnorm(1 - alpha/2)*SElog.contrasts
# Combine Lin Combo coefficients, estimates of contrasts, and confidence intervals in mean scale
wald.ci.1 <- round(data.frame(exp(log.contrasts), exp(lower.log), exp(upper.log)), digits = 2)
# Attach contrast names to rows and columns.                  
rownames <- c("GP Rep | M:F", "GI VC:SC | M:F", "GI VC:M | M:F", "GI VC:SL | M:F", "GI VC:VL | M:F", "GI SC:M | M:F", "GI SC:SL | M:F", "GI SC:VL | M:F", "GI M:SL | M:F", "GI M:VL | M:F", "GI SL:VL | M:F", "PI REP | 1 Cat Ideol", "PI REP | 2 Cat Ideol", "PI REP | 3 Cat Ideol", "PI REP | 4 Cat Ideol")
colnames(wald.ci.1) <- c("Estimate", "Lower CI", "Upper CI")
wald.ci.1
library(xtable)
print(xtable(cbind(rownames, 1/wald.ci.1)), floating = FALSE, include.rownames = FALSE)

# Test fit of scores
# Fit same model with nominal association for PI
mod.homo <- glm(formula = count ~ (gender + party + ideol)^2, family = poisson(link = "log"), data = alldata)
anova(mod.homo.lin1.PI, mod.homo, test = "Chisq")
