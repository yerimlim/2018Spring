#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 11-25-13                                                    #
# PURPOSE: Initial analyses of smoking/resp symptoms data using     #
#     survey package                                                #
#                                                                   #
# NOTES:                                                            #
#####################################################################
options(width = 60)  # Formatting for book - 60 characters per line
# Read Data
smoke.resp <- read.table(file = "C:\\data\\SmokeRespAge.txt", header = TRUE, sep = " ")
smoke.resp[1:6, 1:13]
nrow(smoke.resp)  # Sample size

library(survey)

# Get data and jackknife reps entered into a survey design object
# Even though this is technically a stratified design, there is no information on
#  separate scales for each jackknife replicate, so we use type = "JK1" with a single 
#  scale in scales = 51/52. Ordinarily, type = "JKn" is used for stratified designs.
# Observed responses are in columns 1-10, weights in column 11, and replicate weights in 12-63.

jdesign <- svrepdesign(data = smoke.resp[, c(1:10)], weights = smoke.resp[,11], 
            repweights = smoke.resp[,12:63], type = "JK1", 
            combined.weights = TRUE, scale = 51/52)
class(jdesign)
names(jdesign)
# "$variables" contain original data frame 
#  (further accessed with another layer of $...<variablename>)
# "$repweights" contains the replicate weights


##################################################################
# Get weighted estimate of a proportion and a confidence interval
# Showing both certain manual calculations and functions to perform them.

# Total sample size for manual calculations
Nhat <- sum(smoke.resp$wtint2yr)
Nhat

# Manually calculate a weighted count for cigarette smoking
wt_cig <- smoke.resp$sm_cigs * smoke.resp$wtint2yr  
totcigwt <- sum(wt_cig) 
totcigwt
# Calculation below matches the standard error produced later.
#  52 replicate weights start in column 12
Nhat.reps <- numeric(length = 52)
for(r in 1:52){
 Nhat.reps[r] <- sum(smoke.resp$sm_cigs * smoke.resp[,r+11])
}
sum.sq <- var(Nhat.reps)*51
var.tot <- sum.sq*(51/52) 
sqrt(var.tot)  # Estimated standard error

# Automatically calculate the same weighted count (and the standard error!)
svytotal(x = ~ sm_cigs, design = jdesign)

# Get weighted proportion of cigarette smokers manually 
totcigwt/Nhat

# Automatically calculate proportion of cig smokers as mean of a binary
cigprop <- svymean(x = ~ sm_cigs, design = jdesign)
cigprop

# Confidence interval for true proportion
confint(object = cigprop, level = 0.95, df = 51)
# Confidence interval for true proportion (Wald)
confint(object = cigprop, level = 0.95)

# NOTE: Can get proportions for both levels of the variable by treating it as a factor
cigprop2 <- svymean(x = ~ factor(sm_cigs), design = jdesign)
cigprop2

# Wilson Score Interval preliminary calculations
pihat <- coef(cigprop) 
# Effective sample size for Wilson Score interval 
eff_sample <- pihat*(1-pihat)/vcov(cigprop)
round(eff_sample, 3)

tcrit <- qt(p = c(0.025, 0.975), df = 51)
# Wilson Score interval 
j.Wilsonci_wt <- (((2*eff_sample*pihat + tcrit[2]^2) +
          tcrit*sqrt(tcrit[2]^2 + 4*eff_sample*pihat*(1-pihat))) 
          / (2*(eff_sample + tcrit[2]^2)))
round(j.Wilsonci_wt, digits = 3)

# Other confidence intervals are available from svyciprop(); much like binom.confint
#  Works on one variable at a time. 
#  Specific interest: "beta" analogous to Clopper-Pearson 
#  Other methods include "logit" (logit transformation, default), 
#   "likelihood" (LR based on Rao-Scott), "asin" (arcsin-square-root transform),
#   and "mean" (Wald)
svyciprop(formula = ~ sm_cigs, design = jdesign, method = "beta", level = 0.95)
# factor() does not work here.


