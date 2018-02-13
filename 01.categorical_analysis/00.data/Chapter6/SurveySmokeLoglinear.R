#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 11-25-13                                                    #
# PURPOSE: Loglinear model analysis of a 2-way and a 5-way table    #
#  relating to smoking and respitatory symptons using the           #
#  NHANES 1999-2000 data and the survey package                     #
#                                                                   #
# NOTES:                                                            #
#####################################################################
options(width = 60)  # Formatting for book - 60 characters per line
# Read Data
smoke.resp <- read.table(file = "C:\\data\\SmokeRespAge.txt", header = TRUE, sep = " ")
head(smoke.resp)

library(survey)

# Get data and jackknife reps entered into a survey design object
# Even though this is technically a stratified design, there is no information on
#  separate scales for each jackknife replicate, so we use type = "JK1" with a single 
#  scale in scales = 51/52. Ordinarily, type = "JKn" is used for stratified designs.
# Observed responses are in columns 1-10, weights in column 11, and replicate weights in 12-63.

jdesign <- svrepdesign(data = smoke.resp[, c(1:10)], weights = smoke.resp[,11], 
            repweights = smoke.resp[,12:63], type = "JK1", 
            combined.weights = TRUE, scale = 51/52)


##################################################################
# Simple crosstabulation and tests
# 

# Create variables for any respiratory symptoms and any tobacco use
# Add binaries and assign new binary according to whether sum > 0
jdesign <- update(object = jdesign, anytob = (sm_cigs + sm_pipe + sm_cigar + sm_snuff + sm_chew > 0), 
         anyresp = (re_cough + re_phlegm + re_wheez + re_night > 0))

head(jdesign$variables, n = 2)

# Table of weighted counts
wt.table <- svytable(formula = ~ anytob + anyresp, design = jdesign)
wt.table
summary(wt.table)
# Get proportions by dividing by estimated total population size
totpop <- sum(smoke.resp$wtint2yr)
round(wt.table/totpop, digits = 3)
# Get proportions and standard errors for crosstabulation a different way
# Also available in more cryptic form from wt.table$prob.table
props22 <- svymean(x = ~ interaction(anytob, anyresp), design = jdesign, return.replicates = TRUE)
props22
names(props22)

# Odds Ratio; order of terms is available from output.
ORhat <- props22$mean[1]*props22$mean[4]/(props22$mean[2]*props22$mean[3])
ORhat

OR.reps <- props22$replicates[,1] * props22$replicates[,4] /
      (props22$replicates[,2] * props22$replicates[,3])
reps <- length(OR.reps)

var.logOR = (reps - 1)*var(log(OR.reps))*((reps - 1)/reps)

# Normal-based confidence interval
exp(log(ORhat) + qnorm(p = c(0.025, 0.975))*sqrt(var.logOR))
# t-based confidence interval
exp(log(ORhat) + qt(p = c(0.025, 0.975), df = reps-1)*sqrt(var.logOR))

# Tests for independence
# Rao-Scott 2nd-order F approx (also default)
svychisq(formula = ~ anytob + anyresp, design = jdesign, statistic = "F")
# Rao-Scott 1st-order Chi-Square
svychisq(formula = ~ anytob + anyresp, design = jdesign, statistic = "Chisq")
# Saddlepoint approximation to linear comb of chi-squares for Pearson Stat 
svychisq(formula = ~ anytob + anyresp, design = jdesign, statistic = "saddlepoint")

##################################################################
# Loglinear model analysis of 2-way contingency table

#####################################################################
# IMPORTANT NOTE ABOUT PARAMETERIZATION OF MODELS IN svyloglin():
#
## The parameterization used by svyloglin is unfortunately rather different 
#   from that used in other models we've seen. 
## Each factor is parameterized using "sum-to-zero" contrasts, which 
#   force parameter estimates to sum to zero across all values of an index (subscript), 
#   for each value of any other index.
## For example, suppose X has 3 levels. Then the model that is fit is
#    N_i = beta_0 + betaX_i,    where betaX_3 = - (betaX_1 + betaX_2)
#   Thus, betaX_1 + betaX_2 + betaX_3 = 0
## As another example, suppose an additional variable Y has 4 levels. 
#   Then there should be (3-1)(4-1) = 6 separate association parameters. 
#   In this case the model is
#    N_ij = beta_0 + betaX_i + betaY_j + betaXY_ij, 
#   where
#   1. betaX_3 = -(betaX_1 + betaX_2)
#   2. betaY_4 = -(betaY_1 + betaY_2 + betaY_3)
#   3. betaXY_3j = -(betaXY_1j + betaXY_2j) for j = 1, 2, or 3,
#   4. betaXY_i4 = -(betaXY_i1 + betaXY_i2 + betaXY_i3) for i = 1, or 2,
# and 5. betaXY_34 = betaXY_11 + betaXY_ 12+ betaXY_12 + betaXY_21 + betaXY_22 + betaXY_23
#
## This makes finding odds ratios from loglinear models rather tricky! 
#
## A simplification that occurs when a variable has two levels is that any parameters that
#   are formed for its first level simply have their signs reversed for their second level.
## For exammple, in a 2x2 table, a saturated model has parameters
#   betaX_1, betaX_2 = -betaX_1 (so only betaX_1 is estimated)
#   betaY_1, betaY_2 = -betaY_1 (so only betaY_1 is estimated)
#   betaXY_11, betaXY_12 = -betaXY_11, betaXY_21 = -betaXY_11, and betaXY_22 = +betaXY_11 
#    (so only betaXY_11 is estimated).
# An OR for this table is found from 
#  OR_12.12 = betaXY_11 - betaXY_12 - betaXY_21 + betaXY_22 = 4betaXY_11.
# For larger tables, the formulas can be cumbersome to work out, especially if more than 
#  two variables are involved. For example, for a 3-way interaction, betaXYZ_ijk has the 
#  sum-to-zero restrictions placed on 
#  1. i for each jk combination, 
#  2. j for each ik combination, 
#  3. k for each ij combination, 
#  and these restrictions become more complicated when the levels of two or all three 
#  variables are set to their last value. 
#
# In summary, USE CARE when calculating odds ratios from svyloglin fits. WRITE OUT THE MODEL
#  with all of its sum-to-zero 
#####################################################################
totsamp <- nrow(smoke.resp)

# Fit loglinear model to weighted counts and examine summary
ll_mod1 <- svyloglin(formula = ~ anytob*anyresp, design = jdesign)
summary(ll_mod1)

ll_mod0 <- svyloglin(formula = ~ anytob + anyresp, design = jdesign)
# Rao-Scott test
RStest <- anova(ll_mod0, ll_mod1)
RStest  # Default is F-test
# Second-order RS F-test as in book
print(RStest, pval = "F")
# First-order RS test as in book
print(RStest, pval = "chisq")
# Pearson Statistic (Same as first-order RS), 
#  but estimating the true asymptotic dist using saddlepoint approximation
print(RStest, pval = "saddlepoint")

# Estimate and confidence interval fo Odds Ratio 
# Manually from earlier table counts
(wt.table[1]* wt.table[4])/ (wt.table[2]* wt.table[3])

# From model. Note that association parameter is 1/4 * log(OR)!
cll <- coef(ll_mod1, intercept = TRUE) 
cll[3]
exp(4*cll[3])  # Matches manual calculation
exp(4*confint(ll_mod1))[c(3,6)]

# Estimated counts in each cell
# Parameters produced by loglin are on *sample size* scale. 
# Need to be multiplied by Popsize/Sampsize to reflect population counts, 
# or by 1/Sampsize top reflect proportions
# The (1,1) total
exp(cll[1] + cll[2] + cll[3] + cll[4])*totpop/totsamp
# The (1,-1) = (1,2) total
exp(cll[1] + cll[2] - cll[3] - cll[4])*totpop/totsamp
# The (-1,1) = (2,1) total
exp(cll[1] - cll[2] + cll[3] - cll[4])*totpop/totsamp
# The (-1,-1) = (2,2) total
exp(cll[1] - cll[2] - cll[3] + cll[4])*totpop/totsamp


##################################################################
# Loglinear model analysis of 5-way contingency table
#  Table is for all different tobacco use variables
# 
# First print out table of counts
alltob <- svytable(formula = ~ interaction(sm_cigs, sm_pipe, sm_cigar, sm_snuff, sm_chew), design = jdesign)
alltob
cbind(alltob)

# Note that there are two zero counts. These do not bode well for saturated model
#
# Fit series of models from main effects (independence) to saturated model 
#  to find approximate order of model.

ll.tob1 <- svyloglin(formula = ~  sm_cigs + sm_pipe + sm_cigar + sm_snuff + sm_chew, design = jdesign)
# update(..., formula = ~ .) produces the same model as before. 
# Terms can be added with "+ <termname>", or removed with "- <termname>".
# Below we use ^2 to add all terms up to 2-way combinations, ^3 for all terms up to 3-way combinations, and so forth.
ll.tob2 <- update(object = ll.tob1, formula = ~ .^2)
ll.tob3 <- update(object = ll.tob1, formula = ~ .^3)
ll.tob4 <- update(object = ll.tob1, formula = ~ .^4)
ll.tob5 <- update(object = ll.tob1, formula = ~ .^5)
# Compare models in sequence, testing H0: Simpler model suffices
# Where we reject this hypothesis tells us where to start
anova(ll.tob4, ll.tob5)
anova(ll.tob3, ll.tob5)
anova(ll.tob2, ll.tob5)
anova(ll.tob1, ll.tob5)
anova(ll.tob3, ll.tob4)
anova(ll.tob2, ll.tob3)
anova(ll.tob1, ll.tob2)

# Comparison of models 3 and 4 is impossible: same p-value from different test stats
#  Figure out why. Look at parameter estimates and confidence intervals for models
round(cbind(coef(ll.tob3), confint(ll.tob3)), digits = 2)
round(cbind(coef(ll.tob4), confint(ll.tob4)), digits = 2)
# Clearly the sm_cigs:sm_pipe:sm_cigar:sm_chew effect is causing problems
# Refit model 4 without that one 4-way interaction.
ll.tob4a <- update(object = ll.tob4, formula = ~ .-sm_cigs:sm_pipe:sm_cigar:sm_chew)
round(cbind(coef(ll.tob4a), confint(ll.tob4a)), digits = 2)
anova(ll.tob3, ll.tob4a)
# Fit is fine now, and the 4-way interactions are not needed in the model.

# From 3rd order CIs, begin backward elimination of 3rd-order effects
# Compute p-values in compressed form, then update model and repeat.
round(2*(1 - pnorm(abs(coef(ll.tob3)/sqrt(diag(vcov(ll.tob3))))))[-c(1:15)], digits = 3)
ll.tob31 <- update(ll.tob3, formula = ~ .-sm_cigs:sm_pipe:sm_chew)
round(2*(1 - pnorm(abs(coef(ll.tob31)/sqrt(diag(vcov(ll.tob31))))))[-c(1:15)], digits = 3)
ll.tob32 <- update(ll.tob31, formula = ~ .-sm_cigs:sm_pipe:sm_snuff)
round(2*(1 - pnorm(abs(coef(ll.tob32)/sqrt(diag(vcov(ll.tob32))))))[-c(1:15)], digits = 3)
ll.tob33 <- update(ll.tob32, formula = ~ .-sm_cigs:sm_pipe:sm_cigar)
round(2*(1 - pnorm(abs(coef(ll.tob33)/sqrt(diag(vcov(ll.tob33))))))[-c(1:15)], digits = 3)
ll.tob34 <- update(ll.tob33, formula = ~ .-sm_cigar:sm_snuff:sm_chew)
round(2*(1 - pnorm(abs(coef(ll.tob34)/sqrt(diag(vcov(ll.tob34))))))[-c(1:15)], digits = 3)
ll.tob35 <- update(ll.tob34, formula = ~ .-sm_pipe:sm_cigar:sm_chew)
round(2*(1 - pnorm(abs(coef(ll.tob35)/sqrt(diag(vcov(ll.tob35))))))[-c(1:15)], digits = 3)
ll.tob36 <- update(ll.tob35, formula = ~ .-sm_cigs:sm_cigar:sm_chew)
round(2*(1 - pnorm(abs(coef(ll.tob36)/sqrt(diag(vcov(ll.tob36))))))[-c(1:15)], digits = 3)
ll.tob37 <- update(ll.tob36, formula = ~ .-sm_cigs:sm_cigar:sm_snuff)
round(2*(1 - pnorm(abs(coef(ll.tob37)/sqrt(diag(vcov(ll.tob37))))))[-c(1:15)], digits = 3)
# All vars "significant" at 0.10 level. This is where AIC might stop 
# If we use a 0.05 level for no apparent reason, we continue:
ll.tob38 <- update(ll.tob37, formula = ~ .-sm_pipe:sm_cigar:sm_snuff)
round(2*(1 - pnorm(abs(coef(ll.tob38)/sqrt(diag(vcov(ll.tob38))))))[-c(1:15)], digits = 3)
ll.tob39 <- update(ll.tob38, formula = ~ .-sm_cigs:sm_snuff:sm_chew)
round(2*(1 - pnorm(abs(coef(ll.tob39)/sqrt(diag(vcov(ll.tob39))))))[-c(1:15)], digits = 3)
# Conclusion: CLEARLY there is a 3-way interaction among pipe, snuff, chew.
# This model fits well relative to the full third-order model:
anova(ll.tob39, ll.tob3)

# Odds ratios from selected model; ct = cigarette, pi = pipe
#  cr = cigar, sn = snuff, ch = chew
# First 5 coefficients are for main effects; next 10 are 2-way interactions;
#  last is 3-way interaction.
coef(ll.tob39)

logORs <- svycontrast(stat = ll.tob39, contrasts = list( 
  ct.pi = c(0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
  ct.cr = c(0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
  ct.sn = c(0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0), 
  ct.ch = c(0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0), 
  sn.ch.pi0 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4), 
  sn.ch.pi1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4,-4) 
  )) 
logORs
ORs <- as.data.frame(logORs) 
ORs$OR <- exp(ORs$contrast) 
# Calculate degrees of freedom for t distribution
df <- ll.tob39$df.null - length(coef(ll.tob39)) - 1
# Confidence intervals
ORs$lower.CI <- exp((ORs$contrast + qt(.025, df)*ORs$SE)) 
ORs$upper.CI <- exp((ORs$contrast + qt(.975, df)*ORs$SE)) 
round(ORs[,3:5], 2)

####################################################################
# Larger two-way table: how is model parameterized?

# Count number of types of tobacco used and number of respiratory symptoms, 
# and make a table out of these
jdesign2 <- update(object = jdesign, tob = sm_cigs + sm_pipe + sm_cigar + sm_snuff + sm_chew, 
          resp = re_cough + re_phlegm + re_wheez + re_night)


tab54i <- svyloglin(formula = ~ tob + resp, design = jdesign2)
tab54 <- svyloglin(formula = ~ tob * resp, design = jdesign2)
summary(tab54)

svymean(x = ~ as.factor(tob), design = jdesign2)
wt.tab54 <- svytable(formula = ~ tob + resp, design = jdesign2)
wt.tab54
wt.tab54[1,1]*wt.tab54[2,2]/(wt.tab54[2,1]*wt.tab54[1,2])

c54 <- coef(tab54, intercept = TRUE)

# Parameters produced by loglin are on *sample size* scale. 
# Need to be multiplied by Popsize/Sampsize to reflect population counts, 
# or by 1/Sampsize top reflect proportions
# The (1,1) total
exp(c54[1] + c54[2] + c54[7] + c54[11])
# This reproduces the (1,1) total from svytable():
exp(c54[1] + c54[2] + c54[7] + c54[11]) * totpop / totsamp



summary(wt.tab54)
# Get proportions by dividing by estimated total population size
round(wt.tab54/totpop, digits = 3)
# Get proportions and standard errors for crosstabulation a different way
# Also available is n\more cryptic form from 
svymean(x = ~ interaction(tob,resp), design = jdesign2)

tab24 <- svyloglin(formula = ~ tob * anyresp, design = jdesign2)
coef(tab24, intercept = TRUE)



