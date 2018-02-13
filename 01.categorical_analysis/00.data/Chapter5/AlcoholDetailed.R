#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 08-21-2013                                                  #
# PURPOSE: Detailed analysis of the alcohol consumption example     #
#                                                                   #
# NOTES:                                                            #
#####################################################################
options(width = 60)

#####################################################################
# Read the data

dehart <- read.table("C:\\Data\\DeHartSimplified.csv", header = TRUE, sep = ",", na.strings = " ")
head(dehart)

# Reduce data to what is needed for examples
saturday <- dehart[dehart$dayweek  == 6,]
head(round(x = saturday, digits = 3))
dim(saturday)

# Setting the row labels to be the subject ID values for easier identification.
row.names(saturday) <- saturday$id

# Summarize variables. First three columns are ID and day info, not needed anymore
summary(saturday[,-c(1:3,9)])
tabulate(saturday[,9])

# Code from http://stackoverflow.com/questions/11346880/r-plot-multiple-box-plots-using-columns-from-data-frame
plotdata <- saturday[,-c(1:3,9)]
x11(height = 10, width = 10)
# pdf(file = "c:\\figures\\Figure5.15BW.pdf", width = 10, height = 10, colormodel = "cmyk", pointsize = 20)   # Create plot for book
par(mfrow = c(3,3), mai = c(0.5, 0.5, 0.5, 0.5))
for (i in 1:ncol(plotdata)) {
 boxplot(plotdata[,i], main = names(plotdata[i]), type = "l", cex.axis = 1.5, cex.main = 1.5) 
}
# dev.off()  # Create plot for book

# Look at a scatterplot matrix. 
x11()
pairs(saturday[,-c(1:3,9)])

# Enhanced scatterplot available from car package spm()
library(car)
x11(height = 12, width = 15)
# pdf(file = "c:\\figures\\Figure5.16color.pdf", width = 10, height = 8, colormodel = "cmyk", pointsize = 14)   # Create plot for book
spm(saturday[,-c(1:3,9)], cex.labels = 1.4, cex.axis = 1.4)
# dev.off()  # Create plot for book



#####################################################################
# Model searches using BMA through glmulti() 

############### First a main-effects-only search. Exhaustive search is feasible.

# Unfortinately, have to deactivate a system variable in order to get rJava to work. 
# See http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
# True for 32 bit as well
if (Sys.getenv("JAVA_HOME")! = "")
 Sys.setenv(JAVA_HOME = "")

library(glmulti)

search.1.aicc <- glmulti(y = numall ~ ., data = saturday[,-c(1:3)], fitfunction = "glm", 
            level = 1, method = "h", crit = "aicc", family = poisson(link = "log"))
print(search.1.aicc)
# Look at top 6 models
head(weightable(search.1.aicc))

x11(width = 7, height = 5, pointsize = 14)
# pdf(file = "c:\\figures\\Figure5.17-1color.pdf", width = 7, height = 5, colormodel = "cmyk", pointsize = 16)   # Create plot for book
plot(search.1.aicc, type = "w")  # Plot of model weights
# dev.off()  # Create plot for book
x11(width = 7, height = 5, pointsize = 14)
plot(search.1.aicc, type = "p")  # Plot of IC values
x11(width = 7, height = 5, pointsize = 14)
# pdf(file = "c:\\figures\\Figure5.17-2color.pdf", width = 7, height = 5, colormodel = "cmyk", pointsize = 16)   # Create plot for book
plot(search.1.aicc, type = "s")  # Plot of variable weights
# dev.off()  # Create plot for book

# Note: Best model has 5 variables; same variables stand out as top ones in BMA with >90% weight.
#  All other variables have weight < 30%.



############### Now second order with ALL variables 
# Enumerate the exhaustive search size first
glmulti(y = numall ~ ., data = saturday[,-c(1:3)], fitfunction = "glm", level = 2, 
    marginality = TRUE, method = "d", crit = "aicc", family = poisson(link = "log"))
# "Your candidate set contains more than 1 billion (1e9) models." 
# We will use Genetic Algorithm instead.
# Run multiple times in case one run gets stuck in sub-optimal model.

set.seed(129981872)  # Unfortunately, glmulti() does not heed the seed. This is ineffective in forcing the results to be the same run after run. 
search.2g0.aicc <- glmulti(y = numall ~ ., 
             data = saturday[,-c(1:3)], fitfunction = "glm", level = 2, 
             marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))
search.2g1.aicc <- glmulti(y = numall ~ ., 
             data = saturday[,-c(1:3)], fitfunction = "glm", level = 2, 
             marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))
search.2g2.aicc <- glmulti(y = numall ~ ., 
             data = saturday[,-c(1:3)], fitfunction = "glm", level = 2, 
             marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))
search.2g3.aicc <- glmulti(y = numall ~ ., 
             data = saturday[,-c(1:3)], fitfunction = "glm", level = 2, 
             marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))

# Look at top models from all 4 runs. Not always the same! 

head(weightable(search.2g0.aicc))
head(weightable(search.2g1.aicc))
head(weightable(search.2g2.aicc))
head(weightable(search.2g3.aicc))

# Should combine these to make a "best of" listing
# This is what "consensus()" does.
 
search.2allg.aicc <- consensus(xs = list(search.2g0.aicc, search.2g1.aicc, 
                     search.2g2.aicc, search.2g3.aicc), confsetsize = 100)
head(weightable(search.2allg.aicc))

print(search.2allg.aicc)

x11(width = 9, height = 15, pointsize = 11)
plot(search.2allg.aicc, type = "w")
x11(width = 9, height = 15, pointsize = 11)
plot(search.2allg.aicc, type = "p")
x11(width = 12, height = 15, pointsize = 12)
# par(mai = c(1,1,1,1))
par(mai = c(1,1,.7,.5))
plot(search.2allg.aicc, type = "s")

# Note that top model is also the one with all variables of high importance

# Analysis of parameter estimates
parms <- coef(search.2allg.aicc)
# Renaming columns to fit in book output display
colnames(parms) <- c("Estimate", "Variance", "n.Models", "Probability", "95%CI +/-")
parms.ord <- parms[order(parms[,4], decreasing = TRUE),]
round(parms.ord, digits = 3)
# Reorder parameters in decreasing order of probability
parms.ord <- parms[order(parms[,4], decreasing = TRUE),]
# Confidence intervals for parameters (coef() contains a column with the confidence interval add-ons)
ci.parms <- cbind(lower = parms.ord[,1] - parms.ord[,5], upper = parms.ord[,1] + parms.ord[,5])
round(cbind(parms.ord[,1], ci.parms), digits = 3)

##############
# What would happen if instead I did an exhaustive search on only the 
#  important main-effect variables and their interactions?

search.2e.aicc <- glmulti(y = numall ~ desired + negevent + state + nrel + age, 
             data = saturday[,-c(1:3)], fitfunction = "glm", level = 2, 
             marginality = TRUE, method = "h", crit = "aicc", family = poisson(link = "log"))

print(search.2e.aicc)
# Look at top 6 models
head(weightable(search.2e.aicc))

# Top model is AICC = 421.2, not nearly as good as top model from expanded search using GA.

x11(width = 7, height = 4, pointsize = 12)
plot(search.2e.aicc, type = "w")
x11(width = 7, height = 4, pointsize = 12)
plot(search.2e.aicc, type = "p")
x11(width = 7, height = 4, pointsize = 12)
plot(search.2e.aicc, type = "s")

######################################################################
# Fit best second-order model from GA. Use this as working model.

mod.fit <- glm(formula = numall ~ 1 + prel + negevent + gender + rosn + age + desired + state + 
         rosn:prel + age:rosn + desired:gender + desired:age + state:negevent, 
         family = poisson(link = "log"), data = saturday)

############### Analyze the model fit with various functions
summary(mod.fit)

# Check fit statistic:
deviance(mod.fit)/mod.fit$df.residual
1 + 2*sqrt(2/mod.fit$df.residual)
1 + 3*sqrt(2/mod.fit$df.residual)

# Somewhat of an indication of lack-of-fit somehow. 

############### Is there zero inflation? Create a Pearson Statistic to check

mu.poi <- exp(predict(mod.fit)) 
zero.poi <- sum(exp(-mu.poi))  # Expected number of zeroes for Poisson 
zero.obs <- sum(saturday$numall  == 0)  # Total zeroes in data

# p = 0.97, NO EVIDENCE OF ZERO INFLATION AS EXPECTED.

# Check overall goodness of fit and look at Pearson residuals from it. 
# Useful to check for link issues.
# Assuming that this resides in same folder as current program
source("PostFitGOFTest.R")

GOFtest <- PostFitGOFTest(obs = saturday$numall, pred = mu.poi, g = 0)

############### Examine residuals.
saturday$mu.hat <- predict(mod.fit, type = "response") 
saturday$p.res <- residuals(mod.fit, type = "pearson") 
saturday$s.res <- rstandard(mod.fit, type = "pearson") 
saturday$lin.pred <- mod.fit$linear.predictors
## saturday$cookd <- cooks.distance(mod.fit)
## saturday$hat <- mod.fit$hat

resid.plot <- function(y, x, x.label, color1 = "blue", color2 = "red") {
 ord.x <- order(x)
 plot(x = x, y = y, xlab = x.label, ylab = "Standardized residuals", ylim = c(min (-3, y), max(3,y)))
 abline(h = c(3, 2, 0, -2, -3), lty = 3, col = color1)
 smooth.stand <- loess(formula = y ~ x)
 lines(x = x[ord.x], y = predict(smooth.stand)[ord.x], lty = 2, col = color2)
 invisible()
}

x11(width = 7, height = 6, pointsize = 14)
# pdf(file = "c:\\figures\\Figure5.19color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book
par(mfrow = c(3,3), mar = c(5, 4, 1, 2))  # Removed most of top margin because plots do not have titles

resid.plot(y = saturday$s.res, x = saturday$prel, x.label = "Positive relations")
resid.plot(y = saturday$s.res, x = saturday$negevent, x.label = "Negative Events")
resid.plot(y = saturday$s.res, x = saturday$gender, x.label = "Gender")  # Not sure if plot is worthwhile for a binary variable
resid.plot(y = saturday$s.res, x = saturday$rosn, x.label = "Rosenberg Self Esteem")
resid.plot(y = saturday$s.res, x = saturday$state, x.label = "State Self Esteem")
resid.plot(y = saturday$s.res, x = saturday$desired, x.label = "Desire to Drink")
resid.plot(y = saturday$s.res, x = saturday$age, x.label = "Age")
resid.plot(y = saturday$s.res, x = saturday$mu.hat, x.label = "Estimated mean")
resid.plot(y = saturday$s.res, x = saturday$lin.pred, x.label = "Linear predictor")
# dev.off()  # Create plot for book

# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure5.19BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book
par(mfrow = c(3,3), mar = c(5, 4, 1, 2))  # Removed most of top margin because plots do not have titles
resid.plot(y = saturday$s.res, x = saturday$prel, x.label = "Positive relations", color1 = "black", color2 = "black")
resid.plot(y = saturday$s.res, x = saturday$negevent, x.label = "Negative Events", color1 = "black", color2 = "black")
resid.plot(y = saturday$s.res, x = saturday$gender, x.label = "Gender", color1 = "black", color2 = "black")  # Not sure if plot is worthwhile for a binary variable
resid.plot(y = saturday$s.res, x = saturday$rosn, x.label = "Rosenberg Self Esteem", color1 = "black", color2 = "black")
resid.plot(y = saturday$s.res, x = saturday$state, x.label = "State Self Esteem", color1 = "black", color2 = "black")
resid.plot(y = saturday$s.res, x = saturday$desired, x.label = "Desire to Drink", color1 = "black", color2 = "black")
resid.plot(y = saturday$s.res, x = saturday$age, x.label = "Age", color1 = "black", color2 = "black")
resid.plot(y = saturday$s.res, x = saturday$mu.hat, x.label = "Estimated mean", color1 = "black", color2 = "black")
resid.plot(y = saturday$s.res, x = saturday$lin.pred, x.label = "Linear predictor", color1 = "black", color2 = "black")
# dev.off()  # Create plot for book





########Summary:
# Plots don't show any clear problems with poor choice of link or clear curvature in any predictors. 
# Splines generally wiggle around the center line. Only *possibly* negevent shows some curvature, 
# but it could just be the skewness of negevent creating influential values that attract the spline 
# toward their residual values on the right, There is one very large standardized resudual (> 4), 
# and a small number between 2 and 3 (few enough to believe that are random chance). 
# Should check influence diagnostics. 

############### Default plot produced by plot.glm()
x11()
par(mfrow = c(2,2))
plot(mod.fit)

############### Influence diagnostics via our function. Assuming that this resides in same folder as current program

source("glmDiagnostics.R")
save.info <- glmInflDiag(mod.fit = mod.fit)

###### SUMMARY:
# There are a few large hat values, one pretty large Cook's Distance, and one somewhat outlying value.
#  In addition, there is one observation with both moderate influence and moderate outlying-ness.  
# Looking at those observations in the listing, the outlier (largest Delta.X2 and Delta.dev) 
#  belongs to someone who had 9 drinks but was predicted to have just 2.5. The moderate outlier/influential 
#  point is a person who had 21 drinks and was predicted to have 14. We wonder whether the 21 can be accurate. 
#  This is something that can be explored: does the same model get selected without this possible erroneous value?
#  The large hat values are ID 113, 118, 129. 
summary(mod.fit$data)

# In Which percentile of the data do these three people fall
pcts <- matrix(NA, nrow = 3, ncol = 9)
colnames(pcts) <- colnames(saturday[,5:13])
rownames(pcts) <- list(113, 118, 129)
counter <- 1
for(j in c(113, 118, 129)){
 for(k in c(5:13)){
  pcts[counter, k-4] <- sum((saturday[,k] <= saturday[saturday$id  == j,k]))/length(saturday[,k])
 }
 counter <- counter + 1
}
pcts
###### 113: high nrel, desired, posevent, state; low rosn
###### 118: high prel, posevent; low state, rosn
###### 129: high nrel, low prel

# Assess this visually with Parallel Coordinate Plot
library(MASS)
table(saturday$numall)

# Plot highlighting observations
# May be good to have the printed information from glmInflDiag() to also be printed returned
#  in the object so that one would not need to manually type in the id numbers below
cols <- c("gray50","red")
for(i in c(2, 113, 118, 129, 137, 58)){
 pot.infl <- (saturday$id  == i)
 x11(width = 11, height = 6, pointsize = 14)
 # pdf(file = "c:\\figures\\Figure5.21color.pdf", width = 11, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book
 parcoord(x = saturday[, c(4:8, 10:13, 9)], col = cols[pot.infl + 1], lwd = 1 + 3*pot.infl,
      main = paste("Potentially influential observation ", i, " identified by thick red lines"))
 # dev.off()  # Create plot for book
}

# Black-and-white version of plot
def.par<-par()
par(col.axis = "white", col.lab = "white")
for(i in 58){
 #pdf(file = "c:\\figures\\Figure5.21BW.pdf", width = 11, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book

 #Re-order observations so that the highlighted observation gets plotted last
 n<-nrow(saturday)
 set1<-saturday[(saturday$id  != i), c(4:8, 10:13, 9)]
 set2<-saturday[(saturday$id  == i), c(4:8, 10:13, 9)]
 set3<-rbind(set1, set2)
 print(table(set3$numall))

 parcoord(x = set3, col = c(rep("gray50", times = n-1), "black"), lwd = c(rep(1, times = n-1), 1 + 3))
 #lty = c(rep(1, times = n-1), 2)
 #dev.off()  # Create plot for book
}
par(def.par)


parcoord2<-function (x, col = 1, lty = 1, var.label = FALSE, ...)
{
    rx <- apply(x, 2L, range, na.rm = TRUE)
    x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x,
        na.rm = TRUE) - min(x, na.rm = TRUE)))
    matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty,
        xlab = "", ylab = "", axes = FALSE, ...)
   # matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty,
   #     xlab = "", ylab = "", axes = FALSE)
   axis(1, at = 1L:ncol(x), labels = colnames(x), col = "white")
    for (i in 1L:ncol(x)) {
        lines(c(i, i), c(0, 1), col = "grey70")
        if (var.label)
            text(c(i, i), c(0, 1), labels = format(rx[, i], digits = 3),
                xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
    }
    invisible()
}
x11(width = 11, height = 6, pointsize = 14)

par(col.axis = "white", col.lab = "white")
for(i in 58){
 #pdf(file = "c:\\figures\\Figure5.21BW.pdf", width = 11, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book

 #Re-order observations so that the highlighted observation gets plotted last
 n<-nrow(saturday)
 set1<-saturday[(saturday$id  != i), c(4:8, 10:13, 9)]
 set2<-saturday[(saturday$id  == i), c(4:8, 10:13, 9)]
 set3<-rbind(set1, set2)
 print(table(set3$numall))

 parcoord2(x = set3, col = c(rep("lightgray", times = n-1), "darkblue"), lwd = c(rep(1, times = n-1), 1 + 3))
 #lty = c(rep(1, times = n-1), 2)
 #dev.off()  # Create plot for book
}
par(def.par)

























############### Is the influential outlier with 21 drinks the main cause of lack of fit?
# TO check, rerun selected model without it.

mod.fit.1 <- glm(formula = numall ~ 1 + prel + negevent + gender + rosn + age + desired + state + 
         rosn:prel + age:rosn + desired:gender + desired:age + state:negevent, 
        family = poisson(link = "log"), data = saturday[saturday$numall<20,])

summary(mod.fit.1)

# Check fit statistic:
deviance(mod.fit.1)/mod.fit.1$df.residual
1 + 2*sqrt(2/mod.fit.1$df.residual)
1 + 3*sqrt(2/mod.fit.1$df.residual)
######## Little change in results.

### Might consider rerunning variable selection without this point. 

search.3g0.aicc <- glmulti(y = numall ~ ., 
              data = saturday[saturday$numall<20,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))
search.3g1.aicc <- glmulti(y = numall ~ ., 
              data = saturday[saturday$numall<20,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))
search.3g2.aicc <- glmulti(y = numall ~ ., 
              data = saturday[saturday$numall<20,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))
search.3g3.aicc <- glmulti(y = numall ~ ., 
              data = saturday[saturday$numall<20,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "aicc", family = poisson(link = "log"))

search.3allg.aicc <- consensus(xs = list(search.3g0.aicc, search.3g1.aicc, search.3g2.aicc, search.3g3.aicc), confsetsize = 100)
head(weightable(search.3allg.aicc))

print(search.3allg.aicc)

x11(width = 7, height = 6, pointsize = 12)
plot(search.3allg.aicc, type = "w")
x11(width = 7, height = 6, pointsize = 12)
plot(search.3allg.aicc, type = "p")
x11(width = 7, height = 6, pointsize = 12)
plot(search.3allg.aicc, type = "s")
x11(width = 15, height = 8, pointsize = 12)
plot(search.3allg.aicc, type = "r")

###
# Briefly check fit of the new model without extreme observation
mod.fit.2 <- glm(formula = numall ~ 1 + prel + negevent + rosn + age + desired + state + 
          rosn:prel + age:rosn + desired:age + state:negevent, 
         family = poisson(link = "log"), data = saturday[saturday$numall<20,])

summary(mod.fit.2)

# Check fit statistic:
deviance(mod.fit.2)/mod.fit.2$df.residual
1 + 2*sqrt(2/mod.fit.2$df.residual)
1 + 3*sqrt(2/mod.fit.2$df.residual)
######## Little change in results. Still evidence of a poor fit.



##### Summary: The gender and desired:gender interactions are no longer very important
#  Moving from nearly 100% to <40% evidence weight. 
#  All other variables remain in the model and no new ones are added. 

# We will stick with original model, as we have no reason to remove this observation. 
# Examination of larger data file reveals one other observation at 18 drinks, and one at 15, 14, 13. 
#    hist(x = dehart$numall,breaks = 25)
# Perhaps it is a real value? Question for the researcher.

############### Investigate overdispersion in original model
# No reason found for poor deviance/df statistic; as it is not entirely caused by the one outlier. 
#  Conclude that there is overdispersion.

# Which is better: quasipoisson or negative binomial?

res.sq <- residuals(object = mod.fit, type = "response")^2 
set1 <- data.frame(res.sq, mu.hat = saturday$mu.hat) 

fit.quad <- lm(formula = res.sq ~ mu.hat + I(mu.hat^2), data = set1) 
anova(fit.quad)
# Quadratic term is not at all significant. 

fit.lin <- lm(formula = res.sq ~ mu.hat, data = set1) 
x11(height = 7, width = 6) 
plot(x = set1$mu.hat, y = set1$res.sq, xlab = "Predicted count", ylab = "Squared Residual") 
curve(expr = predict(object = fit.lin, newdata = data.frame(mu.hat = x), type = "response"), 
   col = "blue", add = TRUE, lty = "solid") 
curve(expr = predict(object = fit.quad, newdata = data.frame(mu.hat = x), type = "response"), 
   col = "red", add = TRUE, lty = "dashed") 
legend(x = 50, y = 1000, legend = c("Linear", "Quadratic"), col = c("red", "blue"), 
    lty = c("solid", "dashed"), bty = "n")

# No curvature apparent. Conclusion: Use quasi-Poisson.

##################################################################
# Since we are changing models, should start over with variable selection

# In order to use quasipoisson in glmulti, need to provide dispersion 
#  parameter estimate as "glmultiqaiccvalue". This comes from a previous 
#  quasipoisson fit.
#  Would normally use a saturated model for this, so that we can be sure that 
#  no important variables have been omitted. However, the previous working 
#  model seemed to clearly indicate which variables were important (at least 
#  in the Poisson model) and the saturated model with all pairwise interactions 
#  among all variables gives a much larger dispersion parameter estimate than 
#  the reduced model, so we will use the variables from the previous working 
#  model in a new quasipoisson fit.
#  

mq <- glm(formula = numall ~ prel + negevent + gender + rosn + age + desired + state + 
      rosn:prel + age:rosn + desired:gender + desired:age + state:negevent, 
     family = quasipoisson(link = "log"), data = saturday)
names(summary(mq))

# We will use Genetic Algorithm on model with interactions.
# Run multiple times in case one run gets stuck in sub-optimal model.
# Code below is needed in order to use quasipoisson model in glmulti()
library(R.utils)
setOption("glmulti-cvalue", summary(mq)$dispersion)
getOption("glmulti-cvalue")


search.4g1.qaicc <- glmulti(y = numall ~ ., 
              data = saturday[,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "qaicc", family = poisson(link = "log"))
search.4g2.qaicc <- glmulti(y = numall ~ ., 
              data = saturday[,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "qaicc", family = poisson(link = "log"))
search.4g3.qaicc <- glmulti(y = numall ~ ., 
              data = saturday[,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "qaicc", family = poisson(link = "log"))
search.4g4.qaicc <- glmulti(y = numall ~ ., 
              data = saturday[,c(4:13)], fitfunction = "glm", level = 2, 
              marginality = TRUE, method = "g", crit = "qaicc", family = poisson(link = "log"))

search.4allg.qaicc <- consensus(xs = list(search.4g4.qaicc, search.4g1.qaicc, search.4g2.qaicc, search.4g3.qaicc), confsetsize = 100)
head(weightable(search.4allg.qaicc))

print(search.4allg.qaicc)

x11(width = 7, height = 6, pointsize = 12)
plot(search.4allg.qaicc, type = "w")
x11(width = 7, height = 6, pointsize = 12)
plot(search.4allg.qaicc, type = "p")
x11(width = 7, height = 6, pointsize = 12)
plot(search.4allg.qaicc, type = "s")

#####RESULTS: Same variables are clearly more important than others. 
# One variable slides slightly to lower than 80% weight, but this one is still clearly more important than reamining ones. 
# Choosing to leave model as is.

# Analysis of parameter estimates
parms <- coef(search.4allg.qaicc)
# Renaming columns to fit in book output display
colnames(parms) <- c("Estimate", "Variance", "n.Models", "Probability", "95%CI +/-")
round(parms, digits = 3)
# Reorder parameters in decreasing order of probability
parms.ord <- parms[order(parms[,4], decreasing = TRUE),]
# Confidence intervals for parameters (coef() contains a column with the confidence interval add-ons)
ci.parms <- cbind(lower = parms.ord[,1] - parms.ord[,5], upper = parms.ord[,1] + parms.ord[,5])
round(cbind(parms.ord[,1], ci.parms), digits = 3)
# Expressed as odds ratios
round(exp(cbind(OR = parms.ord[,1], ci.parms))[-1,], digits = 2)


##################################################################
# Analysis of parameter estimates 
# Final model is object mq: quasipoisson on original variable set from first genetic algorithm set.

summary(mq)
round(summary(mq)$coefficients, 3)
############### Could look at LR tests for each term, but these are not meaningful following a variable selection.
# (All terms have already been determined important by the AICc criterion.)

# library(car)
# Anova(mq)



# Analyze interactions using contour plots. Every variable is involved in at least one interaction.
#  Showing example for prel: rosn. Others function the same way

# Start by making grid of points. Then predict values onto the grid. Finally plot the surface. 
# Tell us where to make grid: approximately at ranges of explanatories
summary(saturday[,c(4:13)]) 
# Set the values for the grid for each variable
p1 <- seq(from = 0, to = 9, by = .05)
n1 <- seq(from = 0, to = 2.5, by = .01)
g1 <- seq(from = 1, to = 2, by = 1)  # Only two possible levels of gender
r1 <- seq(from = 2, to = 4, by = .01)
a1 <- seq(from = 20, to = 45, by = .1)
d1 <- seq(from = 0, to = 8, by = .05)
s1 <- seq(from = 2.5, to = 5, by = .01)


# Combine grids for two variables into a data frame, as required for predict(). 
# Set other explanatory variables to average values. 
# (Note gender = 1.5! The advantage of having the binary explanatory treated as numerical 
#  is that we can conceive of an average value!) 
pr1 <- data.frame(expand.grid(prel = p1, rosn = r1), negevent = mean(saturday$negevent), 
         gender = 1.5, age = mean(saturday$age), desired = mean(saturday$desired), 
         state = mean(saturday$state))
# Predict values for the results and store into a matrix following dimensions of the two variables' grids.
surface.pr = matrix(predict(object = mq, newdata = pr1, type = "response"), nrow = length(p1))

# 3D plot to show surface more clearly.
library(rgl)
open3d()
persp3d(x = p1, y = r1, z  =  surface.pr, col = "red", xlab = "Positive Relationships", ylab = "Rosn", zlab = "Predicted Drinks", )
# Could add points if desired:
# points3d(x = saturday$prel, y = saturday$rosn, z = saturday$numall, col = "blue")
aspect3d(x = 1, y = 1, z = 1)


### Could repeat for other interaction pairs...

# Numerical approach: Get estimates of the effect of each variable on the mean count.
#  Complicated by the fact that there are so many interactions, which means that 
#   the effect of one variable changes depending on level of another variable.
#   One estimate does not help. Need to compute estimate for one variable at different levels of the other.
#  Using method suuggested by Milliken and Johnson: 
#   Estimate effects of one variable separately at three quartiles of other variable.
#  Will do this for each variable: estimate ratio of meand for a 1-unit change in target variable,
#   separately at quartiles of interacting variable, holding other variables constant

# For book, displaying the prel results first

# Save the estimated coefficients into bhat 
bhat <- mq$coefficients
round(bhat, digits = 3)
# First compute and store the quartiles for each variable. 
#  Note: the three quartiles are stored in the 2nd, 3rd, and 5th positions in the summary().
rosn.quart <- summary(saturday$rosn)[c(2,3,5)]
rosn.quart

# Compute the linear combinations of parameters to estimate mean ratios, and exponentiate
#  These are the estimated mean ratios of the first variable at three quartiles of interacting variable
# Note: For variables that are involved in more than one interaction, need to consider values for
#  ALL interacting variables at once. Otherwise, those values are assumed to be 0
#  which may not make sense (e.g., setting age = 0...). This applies to rosn, age, and desired.
#  Others need consider only the one variable that they interact with.

# Single-interaction variables
mprel.rosn <- exp(bhat["prel"] + rosn.quart*bhat["prel:rosn"])
mprel.rosn
100*(mprel.rosn - 1)

# Remaining variables
prel.quart <- summary(saturday$prel)[c(2,3,5)]
negev.quart <- summary(saturday$negevent)[c(2,3,5)]
state.quart <- summary(saturday$state)[c(2,3,5)]
age.quart <- summary(saturday$age)[c(2,3,5)]
des.quart <- summary(saturday$desired)[c(2,3,5)]
rosn.quart <- summary(saturday$rosn)[c(2,3,5)]
#"quartiles" for gender are senseless. Only two values to consider.
gen.quart <- c(1,2)

# Compute the linear combinations of parameters to estimate mean ratios, and exponentiate
#  These are the estimated mean ratios of the first variable at three quartiles of interacting variable
# Note: For variables that are involved in more than one interaction, need to consider values for
#  ALL interacting variables at once. Otherwise, those values are assumed to be 0
#  which may not make sense (e.g., setting age = 0...). This applies to rosn, age, and desired.
#  Others need consider only the one variable that they interact with.

# Single-interaction variables
mnegev.state <- exp(bhat["negevent"] + state.quart *bhat["negevent:state"])
mstate.negev <- exp(bhat["state"]  + negev.quart *bhat["negevent:state"])
mgen.des   <- exp(bhat["gender"]  + des.quart  *bhat["gender:desired"])

# Interpret as percent by which mean changes for a 1-unit increase in first variable at quartiles of second variable
mean.ratio1 <- 100*t(cbind(mprel.rosn, mnegev.state, mgen.des, mstate.negev) - 1)
round(mean.ratio1, digits = 1)

# For variables involved in two interactions, need to create grid of combined quartiles (3x3 = 9 rows)
# For rosn, involves prel and age
p.a.grid <- as.matrix(expand.grid(prel = prel.quart, age = age.quart))
p.a.grid
# For age, involves rosn and desired
r.d.grid <- as.matrix(expand.grid(rosn = rosn.quart, desired = des.quart))
r.d.grid
# For desired, involves age and gender. Recall only two values for gender!
a.g.grid <- as.matrix(expand.grid(age = age.quart, gender = gen.quart))
a.g.grid

# Then multiply by the two interaction coefficients (get the order right!) and add to main effect estimate
p.a.coef <- c(bhat["prel:rosn"], bhat["rosn:age"])
p.a.coef
r.d.coef <- c(bhat["rosn:age"], bhat["age:desired"])
r.d.coef
a.g.coef <- c(bhat["age:desired"], bhat["gender:desired"])
a.g.coef

mrosn.prel.age <- exp(bhat["rosn"]  + p.a.grid %*% p.a.coef)
mage.rosn.des <- exp(bhat["age"]   + r.d.grid %*% r.d.coef)
mdes.age.gen  <- exp(bhat["desired"] + a.g.grid %*% a.g.coef)
# Express as percentage change
data.frame(p.a.grid, mratio.rosn.pa = round(100*(mrosn.prel.age-1), digits = 2))
data.frame(r.d.grid, mratio.age.rd = round(100*(mage.rosn.des-1), digits = 2))
data.frame(a.g.grid, mratio.desired.ag = round(100*(mdes.age.gen-1), digits = 2))





#Profile LR interval using mcprofile
library(mcprofile)


# Create coefficient matrices as 1*target coefficient + quartile*interaction(s)
# Look at bhat for ordering of coefficients

# CI for prel, controlling for rosn
K.prel <- matrix(data = c(0, 1, 0, 0, 0, 0, 0, 0, rosn.quart[1], 0, 0, 0, 0,
             0, 1, 0, 0, 0, 0, 0, 0, rosn.quart[2], 0, 0, 0, 0,
             0, 1, 0, 0, 0, 0, 0, 0, rosn.quart[3], 0, 0, 0, 0),
         nrow = 3, byrow = TRUE)
# Does work with quasi-models. Produces wider intervals than otherwise.
profile.prel <- mcprofile(object = mq, CM = K.prel)  # Calculate -2log(Lambda)
ci.prel <- confint(object = profile.prel, level = 0.95)
# ci.beta$confint
100*(exp(ci.prel$estimate) - 1)  # Verifies got same answer as above
100*(exp(ci.prel$confint) - 1)

# CI for state, controlling for negevent
K.state <- matrix(data = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, negev.quart[1],
              0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, negev.quart[2],
              0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, negev.quart[3]),
         nrow = 3, byrow = TRUE)
# Does work with quasi-models. Produces wider intervals than otherwise.
profile.state <- mcprofile(object = mq, CM = K.state)  # Calculate -2log(Lambda)
ci.state <- confint(object = profile.state, level = 0.95)
# ci.beta$confint
100*(exp(ci.state$estimate) - 1)  # Verifies got same answer as above
100*(exp(ci.state$confint) - 1)

# CI for negevent, controlling for state
K.negev <- matrix(data = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, state.quart[1],
              0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, state.quart[2],
              0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, state.quart[3]),
nrow = 3, byrow = TRUE)
# Does work with quasi-models. Produces wider intervals than otherwise.
profile.negev <- mcprofile(object = mq, CM = K.negev)  # Calculate -2log(Lambda)
ci.negev <- confint(object = profile.negev, level = 0.95)
# ci.beta$confint
round(data.frame(100*(exp(ci.negev$estimate) - 1),100*(exp(ci.negev$confint) - 1)), digits = 2)
100*(exp(ci.negev$estimate) - 1)  # Verifies got same answer as above
100*(exp(ci.negev$confint) - 1)

# CI for gender, controlling for desired
K.gen <- matrix(data = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, des.quart[1], 0, 0,
             0, 0, 0, 1, 0, 0, 0, 0, 0, 0, des.quart[2], 0, 0,
             0, 0, 0, 1, 0, 0, 0, 0, 0, 0, des.quart[3], 0, 0),
        nrow = 3, byrow = TRUE)
# Does work with quasi-models. Produces wider intervals than otherwise.
profile.gen <- mcprofile(object = mq, CM = K.gen)  # Calculate -2log(Lambda)
ci.gen <- confint(object = profile.gen, level = 0.95)
# ci.beta$confint
100*(exp(ci.gen$estimate) - 1)  # Verifies got same answer as above
100*(exp(ci.gen$confint) - 1)

# Now to those variables involved in two interactions

# CI for rosn, controlling for prel and age
K.rosn <- matrix(data = c(0, 0, 0, 0, 1, 0, 0, 0, prel.quart[1], age.quart[1], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[1], age.quart[2], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[1], age.quart[3], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[2], age.quart[1], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[2], age.quart[2], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[2], age.quart[3], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[3], age.quart[1], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[3], age.quart[2], 0, 0, 0,
             0, 0, 0, 0, 1, 0, 0, 0, prel.quart[3], age.quart[3], 0, 0, 0),
       nrow = 9, byrow = TRUE)
# Does work with quasi-models. Produces wider intervals than otherwise.
profile.rosn <- mcprofile(object = mq, CM = K.rosn)  # Calculate -2log(Lambda)
ci.rosn <- confint(object = profile.rosn, level = 0.95)
# ci.beta$confint
100*(exp(ci.rosn$estimate) - 1)  # Verifies got same answer as above
100*(exp(ci.rosn$confint) - 1)

# CI for age, controlling for rosn and desired
K.age <- matrix(data = c(0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[1], 0, des.quart[1], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[1], 0, des.quart[2], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[1], 0, des.quart[3], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[2], 0, des.quart[1], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[2], 0, des.quart[2], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[2], 0, des.quart[3], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[3], 0, des.quart[1], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[3], 0, des.quart[2], 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, rosn.quart[3], 0, des.quart[3], 0),
       nrow = 9, byrow = TRUE)
# Does work with quasi-models. Produces wider intervals than otherwise.
profile.age <- mcprofile(object = mq, CM = K.age)  # Calculate -2log(Lambda)
ci.age <- confint(object = profile.age, level = 0.95)
# ci.beta$confint
100*(exp(ci.age$estimate) - 1)  # Verifies got same answer as above
100*(exp(ci.age$confint) - 1)

# CI for desired, controlling for age and gender
K.des <- matrix(data = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, age.quart[1], 0,
             0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, age.quart[2], 0,
             0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, age.quart[3], 0,
             0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, age.quart[1], 0,
             0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, age.quart[2], 0,
             0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, age.quart[3], 0),
       nrow = 6, byrow = TRUE)
# Does work with quasi-models. Produces wider intervals than otherwise.
profile.des <- mcprofile(object = mq, CM = K.des)  # Calculate -2log(Lambda)
ci.des <- confint(object = profile.des, level = 0.95)
# ci.beta$confint
100*(exp(ci.des$estimate) - 1)  # Verifies got same answer as above
100*(exp(ci.des$confint) - 1)





