######################################################################
# NAME:  Tom Loughin                                                 #
# DATE:  2012-01-23                                                  #
# Purpose: Analyze beetle egg crowding data using Zero Inflated      #
#     Poisson regression                                             #
# NOTES:                                                             #
######################################################################
# Data courtesy of Dr. Jim Nechols, Department of Entomology, Kansas State University.


# Read in beetle egg mass data.  Reduce data to smaller subset for ZIP analysis
eggdata <- read.table("C:\\Data\\BeetleEggCrowding.txt", header = TRUE)
eggdata2 <- eggdata[eggdata$TRT == "I",]

# Plot histograms of egg mass counts for each group  
x11(height = 6, width = 7, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.10BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
stripchart(x = NumEggs ~ Temp, method = "stack", data = eggdata2, ylab = "Temperature", xlab = "Number of egg masses", at = c(1.1,1.8))
# dev.off()  # Create plot for book
 
#Fit ZIP Models: Temp in both probability and mean
library(pscl)
zip.mod.tt <- zeroinfl(NumEggs ~ Temp | Temp, dist = "poisson", data = eggdata2)
summary(zip.mod.tt)
# Fit ZIP Models: Temp in mean only
zip.mod.t0 <- zeroinfl(NumEggs ~ Temp | 1, dist = "poisson", data = eggdata2)
# Fit ZIP Models: Temp in probability only
zip.mod.0t <- zeroinfl(NumEggs ~ 1 | Temp, dist = "poisson", data = eggdata2)
# Fit ZIP Models: no explanatories
zip.mod.00 <- zeroinfl(NumEggs ~ 1 | 1, dist = "poisson", data = eggdata2)

# LRTs of each reduced model against largest model.
library(lmtest)
lrtest(zip.mod.t0, zip.mod.tt)
lrtest(zip.mod.0t, zip.mod.tt)
lrtest(zip.mod.00, zip.mod.tt)

# Summary of chosen model
summary(zip.mod.t0)
# Estimated ratio for means 1C apart (Temp-mean is 2nd parameter)
exp(coef(zip.mod.t0)[2])
#Confidence interval for all parameters
confint(zip.mod.t0) 
# Confidence interval for ratio of means 1C apart
round(exp(confint(zip.mod.t0)[2,]), digits = 2)

#############################################################
# Wald Confidence Intervals using glht() from multcomp.

# Matrix of linear combination coefficients for predicted rates at each combination
coef.mat <- rbind(c(1,21,0), c(1,24,0), c(0,0,1))
library(multcomp)
wald <- glht(zip.mod.t0, linfct = coef.mat)
# Options to get unadjusted (univariate) tests and CIs
# Tests
summary(wald, test = univariate())
# Confidence Intervals for means
wald.ci <- exp(confint(wald, calpha = qnorm(0.975))$confint)
round(wald.ci[c(1,2),], digits = 2)
# Confidence Intervals for P(Immune)
round(wald.ci[3,]/(1 + wald.ci[3,]), digits = 2)

#############################################################
# Computing ratio of means and confidence interval

# Making this flexible to choosing number of degrees for comparison (1C here)
deg <- 1
# Summary of chosen model
summary(zip.mod.t0)
# Estimated ratio for means "deg" degrees apart (Temp-mean is 2nd parameter)
exp(deg*coef(zip.mod.t0)[2])
# Confidence interval for all parameters
confint(zip.mod.t0) 
# Confidence interval for ratio of means "deg" degrees apart
round(deg*exp(confint(zip.mod.t0)[2,]), digits = 2)

#############################################################
# Demonstrate zero handling of all 4 models and regular Poisson model

# Fit regular Poisson model
poi.mod <- glm(NumEggs ~ Temp, family = poisson(link = "log"), data = eggdata2)

# Compute predicted number of zeroes by summing the probability of 0 across all observations
mu.poi <- exp(predict(poi.mod)) 
zero.poi <- sum(exp(-mu.poi))
zero.zip.tt <- sum(predict(object = zip.mod.tt, type = "prob")[,1])
zero.zip.t0 <- sum(predict(object = zip.mod.t0, type = "prob")[,1])
zero.zip.0t <- sum(predict(object = zip.mod.0t, type = "prob")[,1])
zero.zip.00 <- sum(predict(object = zip.mod.00, type = "prob")[,1])

data.frame(observed = sum(eggdata2$NumEggs == 0), Poi = round(zero.poi, digits = 2), ZIP.tt = round(zero.zip.tt, digits = 2), ZIP.t0 = round(zero.zip.t0, digits = 2), ZIP.0t = round(zero.zip.0t, digits = 2), ZIP.00 = round(zero.zip.00, digits = 2))
