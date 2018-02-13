#####################################################################
# NAME: Chris Bilder                                                #
# DATE: 7-8-13                                                      #
# PURPOSE: Find best model for the placekicking data set. Note that #
#  the version of the data set used here includes 13 additional     #
#  observations and a few additional variables                      #
#                                                                   # 
# NOTES:                                                            #
#####################################################################


placekick.mb <- read.table("C:\\data\\Placekick.mb.csv", header = TRUE, sep = ",")
head(placekick.mb)
tail(placekick.mb)
# sum(is.na(placekick.mb))


######################################################################
# Model selection - try main effects models only

 # The 32- or 64-bit version of R being used needs to have the corresponding 32- or 64-bit version of
 #  Java installed on the computer in order for glmulti to work
 # Sys.setenv("JAVA_HOME" = "") # If R has problems loading glmulti, may need to use this code
 library(glmulti)
 
 mod.fit.full <- glm(formula = good ~ week + distance + altitude + home + type + precip + wind +
  change + elap30 + PAT + field, family = binomial(link = logit), data = placekick.mb)
 summary(mod.fit.full)

 # All possible using AICc
 search.1.aicc <- glmulti(y = mod.fit.full, level = 1, method = "h", crit = "aicc",
  family = binomial(link = "logit"))
 slotNames(search.1.aicc)
 search.1.aicc@formulas[[1]]
 weightable(search.1.aicc)[1:20,]
 print(search.1.aicc)


######################################################################
# Model selection - try main effects and particular two-way interactions

 # Can only "exclude", not "include", interactions with glmulti()
 excluded.interactions = c("week:distance", "week:altitude", "week:home", "week:type",
   "week:precip", "week:wind", "week:change", "week:elap30", "week:PAT", "week:field",
   "week:temp72",
  "distance:home", "distance:type",
  "altitude:home", "altitude:type", "altitude:precip", "altitude:wind", "altitude:change",
   "altitude:elap30", "altitude:PAT", "altitude:field", "altitude:temp72",
  "home:type", "home:precip", "home:change", "home:elap30", "home:PAT", "home:field", "home:temp72",
  "type:wind", "type:change", "type:elap30", "type:PAT", "type:field", "type:temp72",
  "precip:wind", "precip:change", "precip:elap30", "precip:PAT",
  "wind:change", "wind:elap30", "wind:PAT", "wind:field", "wind:temp72",
  "change:elap30", "change:PAT", "change:field", "change:temp72",
  "elap30:PAT", "elap30:field", "elap30:temp72",
  "PAT:field", "PAT:temp72",
  "field:temp72")

 # Number of interactions to exclude
 # Week = 11, distance = 2, altitude = 9, home = 7, type = 6, precip = 4,
 #  wind = 5, change = 4, elap30 = 3, PAT = 2, field = 1
 sum(c(11, 2, 9, 7, 6, 4, 5, 4, 3, 2, 1))  # 66 total interactions where 54 are excluded

 # Try to use glmulti() but can not get to work
 # Need to use different syntax than previously when using exclude
 main.effects = c("week", "distance", "altitude", "home", "type", "precip", "wind",
  "change", "elap30", "PAT", "field", "temp72")
 search1 <- glmulti(y = "good", xr = main.effects, data = placekick.mb, level = 2, fitfunction = "glm",
  method = "d", crit = "aicc", family = binomial(link = "logit"), exclude = excluded.interactions[-1],
  marginality = TRUE)
 # https://stat.ethz.ch/pipermail/r-help/2012-August/321324.html - more on similar error

 # methods = "d" only shows what variables will be investigated, and this does not work
 search1 <- glmulti(y = "good", xr = main.effects, data = placekick.mb, level = 2, fitfunction = "glm",
  method = "d", crit = "aicc", family = binomial(link = "logit"), exclude = 
   c("week:distance", "week:altitude", "week:home", "week:type",
   "week:precip", "week:wind", "week:change", "week:elap30", "week:PAT", "week:field",
   "week:temp72",
   "PAT:field"),
  marginality = TRUE)

 search1 <- glmulti(y = "good", xr = main.effects, data = placekick.mb, level = 2,
  method = "d", crit = "aicc", family = binomial(link = "logit"), exclude = 
   c("PAT:field", "elap30:PAT", "elap30:field"),
  marginality = TRUE)

 # Simpler tests
 search1 <- glmulti(y = mod.fit.full, level = 2, method = "d", crit = "aicc",
  family = binomial(link = "logit"), exclude = "elap30", marginality = TRUE)
 search1 <- glmulti(y = good ~ distance + wind + PAT + change, data = placekick.mb, fitfunction = "glm",
  level = 2, method = "g", crit = "aicc", family = binomial(link = "logit"), marginality = TRUE,
  exclude = c("wind:PAT"))
 search1 <- glmulti(y = "good", xr = c("distance", "wind", "PAT", "change"), data = placekick.mb, fitfunction = "glm",
  level = 2, method = "d", crit = "aicc", family = binomial(link = "logit"), marginality = TRUE,
  exclude = "PAT:wind")
 search1 <- glmulti(y = "good", xr = c("distance", "wind", "PAT"), data = placekick.mb, fitfunction = "glm",
  level = 2, method = "d", crit = "aicc", family = binomial(link = "logit"), marginality = TRUE,
  exclude = c("distance:wind", "distance:PAT"))  # 10 models is correct



################################################################################
# Due to the problems with glmulti(), I start with the main effects model found previously
#  and perform a forward search with the interactions of interest. Main effects are put back into
#  a model as needed corresponding to an interaction.

 mod.fit.full <- glm(formula = good ~ week + distance + altitude + home + type + precip + wind +
  change + elap30 + PAT + field, family = binomial(link = logit), data = placekick.mb)
 summary(mod.fit.full)

 AICc <- function(object) {
  n <- length(object$y)
  r <- length(object$coefficients)
  AICc <- AIC(object) + 2*r*(r + 1)/(n-r-1)
  list(AICc = AICc, BIC = BIC(object))
 }

 # Main effects model
 mod.fit <- glm(formula = good ~ distance + wind + change + PAT, family = binomial(link = logit), data = placekick.mb)
 AICc(object = mod.fit)$AICc

 # Examine models with one interaction included
 mod.fit1 <- glm(formula = good ~ distance + wind + change + PAT + altitude + distance:altitude,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit2 <- glm(formula = good ~ distance + wind + change + PAT + precip + distance:precip,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit3 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind, family = binomial(link = logit), data = placekick.mb)

 mod.fit4 <- glm(formula = good ~ distance + wind + change + PAT + distance:change, family = binomial(link = logit), data = placekick.mb)

 mod.fit5 <- glm(formula = good ~ distance + wind + change + PAT + elap30 + distance:elap30, family = binomial(link = logit), data = placekick.mb)

 mod.fit6 <- glm(formula = good ~ distance + wind + change + PAT + distance:PAT, family = binomial(link = logit), data = placekick.mb)

 mod.fit7 <- glm(formula = good ~ distance + wind + change + PAT + field + distance:field,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit8 <- glm(formula = good ~ distance + wind + change + PAT + temp72 + distance:temp72,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit9 <- glm(formula = good ~ distance + wind + change + PAT + home + home:wind, family = binomial(link = logit), data = placekick.mb)

 mod.fit10 <- glm(formula = good ~ distance + wind + change + PAT + type + precip + type:precip,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit11 <- glm(formula = good ~ distance + wind + change + PAT + precip + field + precip:field,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit12 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + precip:temp72,
  family = binomial(link = logit), data = placekick.mb)

 # mod.fit13 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + change:PAT,
 #  family = binomial(link = logit), data = placekick.mb)

 # mod.fit14 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + wind:PAT,
 #  family = binomial(link = logit), data = placekick.mb)

 inter <- c("distance:altitude", "distance:precip", "distance:wind", "distance:change",
      "distance:elap30",  "distance:PAT",  "distance:field", "distance:temp72",
      "home:wind",     "type:precip",   "precip:field", "precip:temp72")
 AICc.vec <- c(AICc(mod.fit1)$AICc, AICc(mod.fit2)$AICc, AICc(mod.fit3)$AICc, AICc(mod.fit4)$AICc,
       AICc(mod.fit5)$AICc, AICc(mod.fit6)$AICc, AICc(mod.fit7)$AICc, AICc(mod.fit8)$AICc,
       AICc(mod.fit9)$AICc, AICc(mod.fit10)$AICc, AICc(mod.fit11)$AICc, AICc(mod.fit12)$AICc)
 all.AICc1 <- data.frame(inter = inter, AICc.vec)
 order(all.AICc1[,2])
 all.AICc1[order(all.AICc1[,2]), ]


 # Examine models with distance:wind already included.
 mod.fit1.2 <- glm(formula = good ~ distance + wind + change + PAT + altitude + distance:wind + distance:altitude,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit2.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + distance:wind + distance:precip,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit4.2 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind + distance:change, family = binomial(link = logit), data = placekick.mb)

 mod.fit5.2 <- glm(formula = good ~ distance + wind + change + PAT + elap30 + distance:wind + distance:elap30, family = binomial(link = logit), data = placekick.mb)

 mod.fit6.2 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind + distance:PAT, family = binomial(link = logit), data = placekick.mb)

 mod.fit7.2 <- glm(formula = good ~ distance + wind + change + PAT + field + distance:wind + distance:field,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit8.2 <- glm(formula = good ~ distance + wind + change + PAT + temp72 + distance:wind + distance:temp72,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit9.2 <- glm(formula = good ~ distance + wind + change + PAT + home + distance:wind + home:wind, family = binomial(link = logit), data = placekick.mb)

 mod.fit10.2 <- glm(formula = good ~ distance + wind + change + PAT + type + precip + distance:wind + type:precip,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit11.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + field + distance:wind + precip:field,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit12.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + distance:wind + precip:temp72,
  family = binomial(link = logit), data = placekick.mb)

 # mod.fit13.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + distance:wind + change:PAT,
 #  family = binomial(link = logit), data = placekick.mb)

 # mod.fit14.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + distance:wind + wind:PAT,
 #  family = binomial(link = logit), data = placekick.mb)

 AICc.vec2 <- c(AICc(mod.fit1.2)$AICc, AICc(mod.fit2.2)$AICc,           AICc(mod.fit4.2)$AICc,
       AICc(mod.fit5.2)$AICc, AICc(mod.fit6.2)$AICc, AICc(mod.fit7.2)$AICc, AICc(mod.fit8.2)$AICc,
       AICc(mod.fit9.2)$AICc, AICc(mod.fit10.2)$AICc, AICc(mod.fit11.2)$AICc, AICc(mod.fit12.2)$AICc)
 all.AICc2 <- data.frame(inter = inter[-3], AICc.vec2)
 order(all.AICc2[,2])
 all.AICc2[order(all.AICc2[,2]), ]


 # Examine models with distance:wind and distance:PAT already included.
 mod.fit1.3 <- glm(formula = good ~ distance + wind + change + PAT + altitude + distance:wind + distance:PAT + distance:altitude,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit2.3 <- glm(formula = good ~ distance + wind + change + PAT + precip + distance:wind + distance:PAT + distance:precip,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit4.3 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind + distance:PAT + distance:change, family = binomial(link = logit), data = placekick.mb)

 mod.fit5.3 <- glm(formula = good ~ distance + wind + change + PAT + elap30 + distance:wind + distance:PAT + distance:elap30, family = binomial(link = logit), data = placekick.mb)

 mod.fit7.3 <- glm(formula = good ~ distance + wind + change + PAT + field + distance:wind + distance:PAT + distance:field,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit8.3 <- glm(formula = good ~ distance + wind + change + PAT + temp72 + distance:wind + distance:PAT + distance:temp72,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit9.3 <- glm(formula = good ~ distance + wind + change + PAT + home + distance:wind + distance:PAT + home:wind, family = binomial(link = logit), data = placekick.mb)

 mod.fit10.3 <- glm(formula = good ~ distance + wind + change + PAT + type + precip + distance:wind + distance:PAT + type:precip,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit11.3 <- glm(formula = good ~ distance + wind + change + PAT + precip + field + distance:wind + distance:PAT + precip:field,
  family = binomial(link = logit), data = placekick.mb)

 mod.fit12.3 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + distance:wind + distance:PAT + precip:temp72,
  family = binomial(link = logit), data = placekick.mb)

 # mod.fit13.3 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + distance:wind + distance:PAT + change:PAT,
 #  family = binomial(link = logit), data = placekick.mb)

 # mod.fit14.3 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + distance:wind + distance:PAT + wind:PAT,
 #  family = binomial(link = logit), data = placekick.mb)

 AICc.vec3 <- c(AICc(mod.fit1.3)$AICc, AICc(mod.fit2.3)$AICc,           AICc(mod.fit4.3)$AICc,
       AICc(mod.fit5.3)$AICc,             AICc(mod.fit7.3)$AICc, AICc(mod.fit8.3)$AICc,
       AICc(mod.fit9.3)$AICc, AICc(mod.fit10.3)$AICc, AICc(mod.fit11.3)$AICc, AICc(mod.fit12.3)$AICc)
 all.AICc3 <- data.frame(inter = inter[-c(3,6)], AICc.vec3)
 order(all.AICc2[,2])
 # No other interactions decrease AICc
 all.AICc3[order(all.AICc3[,2]), ]

 # Conclusion: Only distance:wind and distance:PAT lower AICc


 # Alternative (not as good) - Using glmulti() with only main effects and all variables of interest.
 #  Then with the variables remaining, examine the interactions only among these variables.
 search1 <- glmulti(y = good ~ distance + wind + change + PAT, marginality = TRUE,
  data = placekick.mb, level = 2, method = "h", crit = "aicc", family = binomial(link = "logit"))
 head(weightable(search1))
 coef(search1)


################################################################################
# Examine distance^2

 mod.fit.dist2 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind + distance:PAT +
  I(distance^2), family = binomial(link = logit), data = placekick.mb)
 AICc(mod.fit.dist2)

 # Convert to EVP form
 w <- aggregate(formula = good ~ distance + wind + change + PAT, data = placekick.mb, FUN = sum)
 n <- aggregate(formula = good ~ distance + wind + change + PAT, data = placekick.mb, FUN = length)
 w.n <- data.frame(w, trials = n$good, prop = round(w$good/n$good,2))
 head(w.n)
 nrow(w.n)  # Number of EVPs
 sum(w.n$trials)  # Number of observations

 # Verify model fit to EVP data matches the model fit to the binary response data format
 mod.prelim1 <- glm(formula = good/trials ~ distance + wind + change + PAT + distance:wind + distance:PAT,
  family = binomial(link = logit), data = w.n, weights = trials)
 round(summary(mod.prelim1)$coefficients, digits = 4)
 round(summary(mod.fit6.2)$coefficients, digits = 4)

 # Standardized residuals vs. distance
 x11(width = 7, height = 6, pointsize = 12)
 # pdf(file = "c:\\figures\\Figure5.11color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 stand.resid <- rstandard(model = mod.prelim1, type = "pearson")
 plot(x = w.n$distance, y = stand.resid, ylim = c(min(-3, stand.resid),
  max(3, stand.resid)), ylab = "Standardized Pearson residuals", xlab = "Distance")
 abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
 ord.dist <- order(w.n$distance)
 smooth.stand <- loess(formula = stand.resid ~ distance, data = w.n, weights = trials)
 lines(x = w.n$distance[ord.dist], y = predict(smooth.stand)[ord.dist], lty = "solid", col = "red")
 # dev.off()  # Create plot for book

 # Black-and-white version of plot
 # pdf(file = "c:\\figures\\Figure5.11BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 stand.resid <- rstandard(model = mod.prelim1, type = "pearson")
 plot(x = w.n$distance, y = stand.resid, ylim = c(min(-3, stand.resid),
  max(3, stand.resid)), ylab = "Standardized Pearson residuals", xlab = "Distance")
 abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "black")
 ord.dist <- order(w.n$distance)
 smooth.stand <- loess(formula = stand.resid ~ distance, data = w.n, weights = trials)
 lines(x = w.n$distance[ord.dist], y = predict(smooth.stand)[ord.dist], lty = "solid", col = "black")
 # dev.off()  # Create plot for book


 # Very similar way to get the loess model plotted
 # smooth.stand <- loess(formula = stand.resid ~ distance, data = w.n, weights = trials)
 # distances <- seq(from = min(w.n$distance), to = max(w.n$distance),
 #  by = (max(w.n$distance) - min(w.n$distance))/100)
 # pred.data <- predict(object = smooth.stand, newdata = data.frame(distance = distances))
 # lines(x = distances, y = pred.data, lty = "solid", col = "red")



##############################################################################
# Diagnostics - preliminary model

 # AICc and other information criteria measures are different than before due to using the EVP
 #  form of the data (there are a smaller number of rows to the data set)
 AICc(object = mod.prelim1)
 AIC(object = mod.prelim1)
 
 # Read in file containing examine.logistic.reg() and run function
 source(file = "C:\\Rprograms\\Examine.logistic.reg.R")

 # Used with examine.logistic.reg() for rescaling numerical values
 one.fourth.root <- function(x) {
  x^0.25
 }
 one.fourth.root(16)  # Example
 
 save.info1 <- examine.logistic.reg(mod.fit.obj = mod.prelim1, identify.points = TRUE, scale.n = one.fourth.root,
  scale.cookd = sqrt)
 # save.info1 <- examine.logistic.reg(mod.fit.obj = mod.prelim1, identify.points = FALSE, scale.n = one.fourth.root,
 #  scale.cookd = sqrt) # No identification of points
 # Deviance residual version of plots
 # save.info1 <- examine.logistic.reg(mod.fit.obj = mod.prelim1, identify.points = TRUE,
 #  scale.n = one.fourth.root, scale.cookd = sqrt, pearson.dev = "deviance")
 names(save.info1)


 source("C:\\Rprograms\\HLtest.R")
 HL <- HLTest(obj = mod.prelim1, g = 10)
 cbind(HL$observed, round(HL$expect, digits = 1))
 HL
 source("C:\\Rprograms\\OsiusRojek.R")
 o.r.test(obj = mod.prelim1)
 source("C:\\Rprograms\\StukelTest.R")
 stukel.test(obj = mod.prelim1)  # Surprisingly, significant p-value - p-value is larger for chosen model later

 # Examine individual EVPs more closely
 w.n.diag1 <- data.frame(w.n, pi.hat = round(save.info1$pi.hat, 2),
  std.res = round(save.info1$stand.resid, 2),
  cookd = round(save.info1$cookd, 2), h = round(save.info1$h,2))
 # w.n.diag1  # Excluded to save space in the book
 
 # Potential EVPs to examine further
 p <- length(mod.prelim1$coefficients)
 ck.out <- abs(w.n.diag1$std.res) > 2 | w.n.diag1$cookd > 4/nrow(w.n) | w.n.diag1$h > 3*p/nrow(w.n)
 extract.EVPs <- w.n.diag1[ck.out,]  # Extract EVPs
 extract.EVPs[order(extract.EVPs$distance),]  # Order by distance

 # Investigate non-20 yard placekicks further
 mod.prelim1.wo119.120 <- glm(formula = good/trials ~ distance + wind + change + PAT + distance:wind + distance:PAT,
  family = binomial(link = logit), data = w.n[-c(119, 120),], weights = trials)
 round(summary(mod.prelim1.wo119.120)$coefficients, digits = 4)
 # All non-20 yard PATs
 w.n[w.n$distance != 20 & w.n$PAT == 1,]
 
 
######################################################################
# Model selection with reduced data set

 # Remove non-20 yard PATs; "!" negates and "&" means "and"
 placekick.mb2 <- placekick.mb[!(placekick.mb$distance != 20 & placekick.mb$PAT == 1),]
 head(placekick.mb2)
 tail(placekick.mb2)
 nrow(placekick.mb2)  # Number of observations after 13 were removed


 mod.fit.full2 <- glm(formula = good ~ week + distance + altitude + home + type + precip + wind +
  change + elap30 + PAT + field, family = binomial(link = logit), data = placekick.mb2)
 summary(mod.fit.full2)

 # All possible using AICc
 search.2.aicc <- glmulti(y = mod.fit.full2, level = 1, method = "h", crit = "aicc",
  family = binomial(link = "logit"))
 slotNames(search.2.aicc)
 search.2.aicc@formulas[[1]]

 # Main effects model
 mod.fit1 <- glm(formula = good ~ distance + wind + change + PAT, family = binomial(link = logit),
  data = placekick.mb2)
 AICc(object = mod.fit1)$AICc

 # Examine models with one interaction included
 mod.fit2 <- glm(formula = good ~ distance + wind + change + PAT + altitude + distance:altitude,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit3 <- glm(formula = good ~ distance + wind + change + PAT + precip + distance:precip,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit4 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind, family = binomial(link = logit), data = placekick.mb2)

 mod.fit5 <- glm(formula = good ~ distance + wind + change + PAT + distance:change, family = binomial(link = logit), data = placekick.mb2)

 mod.fit6 <- glm(formula = good ~ distance + wind + change + PAT + elap30 + distance:elap30, family = binomial(link = logit), data = placekick.mb2)

 mod.fit7 <- glm(formula = good ~ distance + wind + change + PAT + distance:PAT, family = binomial(link = logit), data = placekick.mb2)

 mod.fit8 <- glm(formula = good ~ distance + wind + change + PAT + field + distance:field,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit9 <- glm(formula = good ~ distance + wind + change + PAT + temp72 + distance:temp72,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit10 <- glm(formula = good ~ distance + wind + change + PAT + home + home:wind, family = binomial(link = logit), data = placekick.mb2)

 mod.fit11 <- glm(formula = good ~ distance + wind + change + PAT + type + precip + type:precip,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit12 <- glm(formula = good ~ distance + wind + change + PAT + precip + field + precip:field,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit13 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + precip:temp72,
  family = binomial(link = logit), data = placekick.mb2)

 inter <- c("distance:altitude", "distance:precip", "distance:wind", "distance:change",
      "distance:elap30",  "distance:PAT",  "distance:field", "distance:temp72",
      "home:wind",     "type:precip",   "precip:field", "precip:temp72")
 AICc.vec <- c(AICc(mod.fit2)$AICc, AICc(mod.fit3)$AICc, AICc(mod.fit4)$AICc, AICc(mod.fit5)$AICc,
       AICc(mod.fit6)$AICc, AICc(mod.fit7)$AICc, AICc(mod.fit8)$AICc, AICc(mod.fit9)$AICc,
       AICc(mod.fit10)$AICc, AICc(mod.fit11)$AICc, AICc(mod.fit12)$AICc, AICc(mod.fit13)$AICc)
 all.AICc1 <- data.frame(inter = inter, AICc.vec)
 order(all.AICc1[,2])
 all.AICc1[order(all.AICc1[,2]), ]


 # Examine models with distance:wind already included.
 mod.fit2.2 <- glm(formula = good ~ distance + wind + change + PAT + altitude + distance:wind + distance:altitude,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit3.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + distance:wind + distance:precip,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit5.2 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind + distance:change, family = binomial(link = logit), data = placekick.mb2)

 mod.fit6.2 <- glm(formula = good ~ distance + wind + change + PAT + elap30 + distance:wind + distance:elap30, family = binomial(link = logit), data = placekick.mb2)

 mod.fit7.2 <- glm(formula = good ~ distance + wind + change + PAT + distance:wind + distance:PAT, family = binomial(link = logit), data = placekick.mb2)

 mod.fit8.2 <- glm(formula = good ~ distance + wind + change + PAT + field + distance:wind + distance:field,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit9.2 <- glm(formula = good ~ distance + wind + change + PAT + temp72 + distance:wind + distance:temp72,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit10.2 <- glm(formula = good ~ distance + wind + change + PAT + home + distance:wind + home:wind, family = binomial(link = logit), data = placekick.mb2)

 mod.fit11.2 <- glm(formula = good ~ distance + wind + change + PAT + type + precip + distance:wind + type:precip,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit12.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + field + distance:wind + precip:field,
  family = binomial(link = logit), data = placekick.mb2)

 mod.fit13.2 <- glm(formula = good ~ distance + wind + change + PAT + precip + temp72 + distance:wind + precip:temp72,
  family = binomial(link = logit), data = placekick.mb2)

 AICc.vec2 <- c(AICc(mod.fit2.2)$AICc, AICc(mod.fit3.2)$AICc,           AICc(mod.fit5.2)$AICc,
       AICc(mod.fit6.2)$AICc, AICc(mod.fit7.2)$AICc, AICc(mod.fit8.2)$AICc, AICc(mod.fit9.2)$AICc,
       AICc(mod.fit10.2)$AICc, AICc(mod.fit11.2)$AICc, AICc(mod.fit12.2)$AICc, AICc(mod.fit13.2)$AICc)
 all.AICc2 <- data.frame(inter = inter[-3], AICc.vec2)
 order(all.AICc2[,2])
 all.AICc2[order(all.AICc2[,2]), ]


 # EVP form
 w2 <- aggregate(formula = good ~ distance + wind + change + PAT, data = placekick.mb2, FUN = sum)
 n2 <- aggregate(formula = good ~ distance + wind + change + PAT, data = placekick.mb2, FUN = length)
 w.n2 <- data.frame(w2, trials = n2$good, prop = round(w2$good/n2$good, 2))
 head(w.n2)
 nrow(w.n2)  # Number of EVPs
 sum(w.n2$trials)  # Number of observations

 # Verify model fit to EVP data matches the model fit to the binary response data format
 mod.prelim2 <- glm(formula = good/trials ~ distance + wind + change + PAT + distance:wind,
  family = binomial(link = logit), data = w.n2, weights = trials)
 summary(mod.prelim2)

 # Standardized residuals vs. distance
 x11(width = 7, height = 6, pointsize = 12)
 stand.resid2 <- rstandard(model = mod.prelim2, type = "pearson")
 # ord.dist <- order(w.n2$distance)
 plot(x = w.n2$distance, y = stand.resid2, ylim = c(min(-3, stand.resid2),
  max(3, stand.resid2)), ylab = "Standardized Pearson residuals", xlab = "Distance")
 abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
 ord.dist2 <- order(w.n2$distance)
 smooth.stand2 <- loess(formula = stand.resid2 ~ distance, data = w.n2, weights = trials)
 lines(x = w.n2$distance[ord.dist2], y = predict(smooth.stand2)[ord.dist2], lty = "solid", col = "red")


 HL <- HLTest(obj = mod.prelim2, g = 10)
 cbind(HL$observed, round(HL$expect, digits = 1))
 HL
 o.r.test(obj = mod.prelim2)
 stukel.test(obj = mod.prelim2)


################################################################################
# Diagnostics using data without the non-20 yard placekicks

 save.info2 <- examine.logistic.reg(mod.fit.obj = mod.prelim2, identify.points = TRUE, scale.n = one.fourth.root,
  scale.cookd = sqrt)

 # Examine individual EVPs more closely
 w.n.diag2 <- data.frame(w.n2, pi.hat = round(save.info2$pi.hat, 2),
  std.res = round(save.info2$stand.resid, 2),
  cookd = round(save.info2$cookd, 2), h = round(save.info2$h, 2))
 # w.n.diag2  # Excluded to save space in the book

 # Potential EVPs to examine further
 p <- length(mod.prelim2$coefficients)
 ck.out <- abs(w.n.diag2$std.res) > 2 | w.n.diag2$cookd > 4/nrow(w.n2) | w.n.diag2$h > 3*p/nrow(w.n2)
 extract.EVPs2 <- w.n.diag2[ck.out,]  # Extract EVPs
 extract.EVPs2[order(extract.EVPs2$distance),]  # Order by distance

 w.n2[100:102,]
 w.n[100:102,]
 mod.prelim2.wo101 <- glm(formula = good/trials ~ distance + wind + change + PAT + distance:wind,
  family = binomial(link = logit), data = w.n2[-101,], weights = trials)
 summary(mod.prelim2.wo101)

 w.n2[14:16,]
 w.n[14:16,]
 mod.prelim2.wo15 <- glm(formula = good/trials ~ distance + wind + change + PAT + distance:wind,
  family = binomial(link = logit), data = w.n2[-15,], weights = trials)
 summary(mod.prelim2.wo15)

 # Compare
 # beta^s
 round(data.frame(orig = summary(mod.prelim2)$coefficients[,1],
  wo101 = summary(mod.prelim2.wo101)$coefficients[,1],
  wo15 = summary(mod.prelim2.wo15)$coefficients[,1]), digits = 4)
 # SEs
 round(data.frame(orig = summary(mod.prelim2)$coefficients[,2],
  wo101 = summary(mod.prelim2.wo101)$coefficients[,2],
  wo15 = summary(mod.prelim2.wo15)$coefficients[,2]), digits = 4)
 # Wald test p-values
 round(data.frame(orig = summary(mod.prelim2)$coefficients[,4],
  wo101 = summary(mod.prelim2.wo101)$coefficients[,4],
  wo15 = summary(mod.prelim2.wo15)$coefficients[,4]), digits = 4)



################################################################################
# Model interpretation

 # OR estimates
 library(package = mcprofile)

 OR.name <- c("Change", "PAT", "Distance, 10-yard decrease, windy", "Distance, 10-yard decrease, not windy",
  "Wind, distance = 20", "Wind, distance = 30", "Wind, distance = 40", "Wind, distance = 50",
  "Wind, distance = 60")
 var.name <- c("int", "distance", "wind", "change", "PAT", "distance:wind")

 K <- matrix(data = c(0,  0, 0, 1, 0,  0,
            0,  0, 0, 0, 1,  0,
            0, -10, 0, 0, 0, -10,
            0, -10, 0, 0, 0,  0,
            0,  0, 1, 0, 0, 20,
            0,  0, 1, 0, 0, 30,
            0,  0, 1, 0, 0, 40,
            0,  0, 1, 0, 0, 50,
            0,  0, 1, 0, 0, 60),
  nrow = 9, ncol = 6, byrow = TRUE, dimnames = list(OR.name, var.name))
 # K # Check matrix - excluded to save space
 linear.combo <- mcprofile(object = mod.prelim2, CM = K)
 ci.log.OR <- confint(object = linear.combo, level = 0.90, adjust = "none")
 # ci.log.OR
 exp(ci.log.OR)

 # Wald CIs (if desired)
 save.wald <- wald(linear.combo)
 save.wald
 ci.log.OR.wald <- confint(object = save.wald, level = 0.90, adjust = "none")
 exp(ci.log.OR.wald)
 

 # Examine probability of success for PATs vs. field goals
 predict(object = mod.prelim2, newdata = data.frame(distance = c(20, 20), wind = c(0, 0),
  change = c(0, 0), PAT = c(1, 0)), type = "response")

 # Using mcprofile to obtain profile LR
 K <- matrix(data = c(1, 20, 0, 0, 1, 0,
            1, 20, 0, 0, 0, 0),
  nrow = 2, ncol = 6, byrow = TRUE, dimnames = list(c("PAT", "FG"), var.name))
 # K # Check matrix - excluded to save space
 linear.combo <- mcprofile(object = mod.prelim2, CM = K)
 ci.lin.pred <- confint(object = linear.combo, level = 0.90, adjust = "none")
 ci.lin.pred
 # exp(ci.lin.pred$estimate)/(1 + exp(ci.lin.pred$estimate))
 # plogis(q = c(4.14, 2.88))
 # as.matrix() is needed to get the proper class for plogis()
 round(plogis(q = as.matrix(ci.lin.pred$estimate)), digits = 3)  # as.numeric() and as.vector() do not work
 round(plogis(q = as.matrix(ci.lin.pred$confint)), digits = 3)

 #Elliott's kick discussed in Bilder and Loughin (1998)
 K <- matrix(data = c(1, 42, 0, 1, 0, 0), nrow = 1, ncol = 6, byrow = TRUE)
 K
 linear.combo <- mcprofile(object = mod.prelim2, CM = K)
 ci.lin.pred <- confint(object = linear.combo, level = 0.90, adjust = "none")
 round(plogis(q = as.matrix(ci.lin.pred$estimate)), digits = 3)
 round(plogis(q = as.matrix(ci.lin.pred$confint)), digits = 3)

 # Plot - Probability of success for four combinations of explanatory variables
 beta.hat <- mod.prelim2$coefficients
 # Change = 0, wind = 0
 x11(width = 7, height = 6, pointsize = 12)
 # pdf(file = "c:\\figures\\Figure5.13color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x), lty = "solid", xlim = c(18, 66), ylim = c(0, 1), lwd = 2,
  col = "red", panel.first = grid(col = "gray", lty = "dotted"), ylab = "Estimated probability of success",
  xlab = "Distance")
 # change = 1, wind = 0
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x + beta.hat[4]), lty = "dashed",
  lwd = 2 , col = "darkgreen", add = TRUE)
 # change = 0, wind = 1
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x + beta.hat[3] + beta.hat[6]*x), lty = "dotted",
  lwd = 2, col = "blue", add = TRUE)
 # change = 1, wind = 1
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x + beta.hat[3] + beta.hat[4] + beta.hat[6]*x), lty = "dotdash",
  lwd = 2, col = "purple", add = TRUE)
 names1 <- c("Change = 0, Wind = 0", "Change = 1, Wind = 0", "Change = 0, Wind = 1", "Change = 1, Wind = 1")
 legend(x = 20, y = 0.39, legend = names1, lty = c("solid", "dashed", "dotted", "dotdash"),
  col = c("red","darkgreen","blue","purple"), bty = "n", cex = 1, lwd = 2)
 # dev.off()  # Create plot for book

 # Black-and-white version of plot
 # pdf(file = "c:\\figures\\Figure5.13BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x), lty = "solid", xlim = c(18, 66), ylim = c(0, 1), lwd = 2,
  col = "black", ylab = "Estimated probability of success", xlab = "Distance")
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x + beta.hat[4]), lty = "dashed",
  lwd = 2 , col = "black", add = TRUE)
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x + beta.hat[3] + beta.hat[6]*x), lty = "dotted",
  lwd = 2, col = "black", add = TRUE)
 curve(expr = plogis(beta.hat[1] + beta.hat[2]*x + beta.hat[3] + beta.hat[4] + beta.hat[6]*x), lty = "dotdash",
  lwd = 2, col = "black", add = TRUE)
 names1 <- c("Change = 0, Wind = 0", "Change = 1, Wind = 0", "Change = 0, Wind = 1", "Change = 1, Wind = 1")
 legend(x = 20, y = 0.39, legend = names1, lty = c("solid", "dashed", "dotted", "dotdash"),
  col = c("black","black","black","black"), bty = "n", cex = 1, lwd = 2)
 # dev.off()  # Create plot for book


 # Plot - Probability of success for two combinations of explanatory variables with CIs
 # Most of this function is from Chapter 2
 ci.pi <- function(newdata, mod.fit.obj, alpha){
  linear.pred <- predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
  CI.lin.pred.lower <- linear.pred$fit - qnorm(p = 1-alpha/2)*linear.pred$se
  CI.lin.pred.upper <- linear.pred$fit + qnorm(p = 1-alpha/2)*linear.pred$se
  CI.pi.lower <- exp(CI.lin.pred.lower) / (1 + exp(CI.lin.pred.lower))
  CI.pi.upper <- exp(CI.lin.pred.upper) / (1 + exp(CI.lin.pred.upper))
  list(pi.hat = plogis(linear.pred$fit), lower = CI.pi.lower, upper = CI.pi.upper)
 }
 # Test
 ci.pi(newdata = data.frame(distance = c(20, 20), wind = c(0, 0),
  change = c(0, 0), PAT = c(1, 0)), mod.fit.obj = mod.prelim2, alpha = 0.10)

 # Change = 0, wind = 0
 x11(width = 7, height = 6, pointsize = 12)
 # pdf(file = "c:\\figures\\Figure5.14color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 0, change = 0, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$pi.hat,
  xlim = c(18, 66), lty = "solid", lwd = 2, col = "red", xlab = "Distance", ylab = "Estimated probability of success",
  ylim = c(0, 1), panel.first = grid(col = "gray", lty = "dotted"))
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 0, change = 0, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$lower,
  lty = "dotted", lwd = 2, col = "red", add = TRUE)
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 0, change = 0, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$upper,
  lty = "dotted", lwd = 2, col = "red", add = TRUE)

 # Change = 1 and wind = 1
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 1, change = 1, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$pi.hat,
  lty = "dotdash", lwd = 2, col = "purple", add = TRUE)
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 1, change = 1, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$lower,
  lty = "dotted", lwd = 2, col = "purple", add = TRUE)
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 1, change = 1, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$upper,
  lty = "dotted", lwd = 2, col = "purple", add = TRUE)

 names1 <- c("Estimated Probability", "90% Confidence Interval")
 text(x = 22, y = 0.38, "Least risky")
 legend(x = 17, y = 0.38, legend = names1, lty = c("solid", "dotted", "dotted"), col = c("red","red"), bty = "n", lwd = 2)
 text(x = 21.5, y = 0.18, "Most risky")
 legend(x = 17, y = 0.18, legend = names1, lty = c("dotdash", "dotted", "dotted"), col = c("purple","purple"), bty = "n", lwd = 2)
 # dev.off()  # Create plot for book


 # Black-and-white version of plot
 # pdf(file = "c:\\figures\\Figure5.14BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 0, change = 0, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$pi.hat,
  xlim = c(18, 66), lty = "solid", lwd = 2, col = "black", xlab = "Distance", ylab = "Estimated probability of success",
  ylim = c(0, 1))
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 0, change = 0, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$lower,
  lty = "dotted", lwd = 2, col = "black", add = TRUE)
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 0, change = 0, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$upper,
  lty = "dotted", lwd = 2, col = "black", add = TRUE)

 # Change = 1 and wind = 1
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 1, change = 1, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$pi.hat,
  lty = "dotdash", lwd = 2, col = "black", add = TRUE)
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 1, change = 1, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$lower,
  lty = "dotted", lwd = 2, col = "black", add = TRUE)
 curve(expr = ci.pi(newdata = data.frame(distance = x, wind = 1, change = 1, PAT = 0), mod.fit.obj = mod.prelim2, alpha = 0.10)$upper,
  lty = "dotted", lwd = 2, col = "black", add = TRUE)

 names1 <- c("Estimated Probability", "90% Confidence Interval")
 text(x = 22, y = 0.38, "Least risky")
 legend(x = 17, y = 0.38, legend = names1, lty = c("solid", "dotted", "dotted"), col = c("black","black"), bty = "n", lwd = 2)
 text(x = 21.5, y = 0.18, "Most risky")
 legend(x = 17, y = 0.18, legend = names1, lty = c("dotdash", "dotted", "dotted"), col = c("black","black"), bty = "n", lwd = 2)
 # dev.off()  # Create plot for book


#