#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 1-10-13                                                     #
# PURPOSE: All Subsets selection techniques in Placekick data       #
#                                                                   #
# NOTES:                                                            #
#####################################################################

placekick <- read.table(file = "C:\\data\\Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)

# Alternative specification to perform the search using a previous glm() fit:
#
### full.mod.1 <- glm(formula = good ~., family = binomial(link = logit), data = placekick)
### search.1.aicc <- glmulti(y = full.mod.1, level = 1, method = "h", crit = "aicc", family = binomial(link = "logit"))
#

# Unfortunately, have to deactivate a system variable in order to get rJava to work. 
# See http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
# True for 32 bit as well
if (Sys.getenv("JAVA_HOME") != "")
 Sys.setenv(JAVA_HOME = "")
library(glmulti)
# Using AICc as criterion. Could use crit = "bic" or "aic" instead.
# Using "good ~ ." to include all variables from data (other than "good")
search.1.aicc <- glmulti(y = good ~ ., data = placekick, fitfunction = "glm", 
             level = 1, method = "h", crit = "aicc", family = binomial(link = "logit"))

print(search.1.aicc)
aa <- weightable(search.1.aicc)
cbind(model = aa[1:5,1], round(aa[1:5,2:3], digits = 3))

plot(search.1.aicc, type = "p")

# The following search looks for all pairwise interactions as well. 
# WARNING: It would have taken about 13 years to complete on an Intel Core I7 2600 with 8G RAM.
### search.2marg.aicc <- glmulti(y = full.mod.1, level = 2, marginality = TRUE, method = "h", crit = "aicc")
#
# Instead, glmulti() can use a "genetic algorithm" search procedure to find groups of models 
# that might be good and find the best of those models. 
# 
# We repeat the first search using the genetic algorithm to show that it can get to the same 
# results as the better exhaustive search in a small problem. 

set.seed(267188299)
search.g.aicc <- glmulti(y = good ~ ., data = placekick, fitfunction = "glm", 
             level = 1, method = "g", crit = "aicc", family = binomial(link = "logit"))

# Now try genetic algorithm on bigger problem with pairwise interactions. See glmulti manual 
# for details on tuning parameters for the genetic algorithm.
# NOTE: This took about 7 minutes to run on an Intel Core I7 2600 with 8G RAM.

search.gmarg.aicc <- glmulti(y = good ~ ., data = placekick, fitfunction = "glm", level = 2, 
               marginality = TRUE, method = "g", crit = "aicc", family = binomial(link = "logit"))

print(search.gmarg.aicc)
head(weightable(search.gmarg.aicc))
plot(search.gmarg.aicc, type = "p")

# All subsets using BMA package function bic.glm()

library(BMA)
search.bma <- bic.glm(f = good ~ ., glm.family = "binomial", data = placekick, occam.window = FALSE)
# Be aware that BMA has no function to do AICc, and that it calculates the IC(k) values slightly 
# differently. The ordering of models is the same, and the differences between two models'
# IC(k) values is the same as in glmulti, but the numerical values are different.
summary(search.bma)

# All subsets using BMA package function bic.glm()
library(bestglm)
search.bestglm <- bestglm(Xy = placekick, family = binomial, IC = "BIC")
# Show glm() fit of best model
search.bestglm$BestModel
# List top 5 models.  Can get more models listed using TopModels = parameter.
search.bestglm$BestModels
# List best model of each size
search.bestglm$Subsets 

