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
# Logistic regression analysis of the binary variable for any respiratory symptoms
#  against age and different forms of tobacco use
# Note: Documentation says:
#  "For binomial and Poisson families use family = quasibinomial() 
#  and family = quasipoisson() to avoid a warning about non-integer 
#  numbers of successes. The 'quasi' versions of the family objects 
#  give the same point estimates and standard errors and do not give the warning."
# 

# Create variable for any respiratory symptoms 
# Add binaries and assign new binary according to whether sum > 0
jdesign <- update(object = jdesign, anyresp = (re_cough + re_phlegm + re_wheez + re_night > 0))

# Model with age alone
m1 <- svyglm(formula = anyresp ~ age, design = jdesign, family = quasibinomial(link = "logit"))
summary(m1)
# Model with age and all tobacco product main effects
m2 <- svyglm(formula = anyresp ~ age + sm_cigs + sm_pipe + sm_cigar + sm_snuff + sm_chew, design = jdesign, 
       family = quasibinomial(link = "logit"))
summary(m2)

# Comparison of these models: anova() does LRT F-test by default.
anova(m1, m2)

# Adding age interactions with tobacco products
m3 <- svyglm(formula = anyresp ~ age * (sm_cigs + sm_pipe + sm_cigar + sm_snuff + sm_chew), design = jdesign, 
       family = quasibinomial(link = "logit"))
summary(m3)

# Comparison of m2 and m3 two ways. They have opposite default test statistics, even though they perform the same test.
anova(m2, m3)
regTermTest(model = m3, test.terms = ~ age : (sm_cigs + sm_pipe + sm_cigar + sm_snuff + sm_chew))
regTermTest(model = m3, test.terms = ~ age : (sm_cigs + sm_pipe + sm_cigar + sm_snuff + sm_chew), method = "LRT")
# Backward elimination of interactions and main effects: Remove age:sm_cigs
m <- update(m3, formula = ~ . - age:sm_cigs) 
# svyglm(anyresp ~ age + sm_cigs + age * (sm_pipe + sm_cigar + sm_snuff + sm_chew) , design = jdesign.age,family = quasibinomial())
summary(m)
# Backward elimination of interactions: Remove age:sm_snuff
m <- update(m, formula = ~ . - age:sm_snuff)
summary(m)
# Backward elimination of interactions: Remove sm_snuff
m <- update(m, formula = ~ . - sm_snuff)
summary(m)
# Backward elimination of interactions: Remove age:sm_cigar
m <- update(m, formula = ~ . - age:sm_cigar)
summary(m)
# Backward elimination of interactions: Remove sm_cigar
m <- update(m, formula = ~ . - sm_cigar)
summary(m)
# all terms in model above are significant using alpha = 0.15 call this the final model
m.final <- svyglm(formula = anyresp ~ age + sm_cigs + age * (sm_pipe + sm_chew), design = jdesign, family = quasibinomial(link = "logit"))
summary(m.final)


##################################################################
# CIs from reduced (final) model As exercise: repeat from full model (m3)

# Odds Ratios for symptoms with 10-year age change
# First for non-pipe users, then for pipe users, then chew users
logOR.age <- svycontrast(m.final, list(
tenyr = c(0, 10, 0, 0, 0, 0, 0),
tenyr_pipe = c(0, 10, 0, 0, 0, 10, 0),
tenyr_chew = c(0, 10, 0, 0, 0, 0, 10)))
# Convert to data frame for computations
ORs.age <- as.data.frame(logOR.age)
ORs.age$OR <- exp(ORs.age$contrast)
ORs.age$lower.CI <- exp((ORs.age$contrast + qt(0.025, m.final$df.residual) * ORs.age$SE))
ORs.age$upper.CI <- exp((ORs.age$contrast + qt(0.975, m.final$df.residual) * ORs.age$SE))
round(ORs.age[,3:5],2)

# Next ORs for each tobacco use. For pipe and chew, do separately at 20, 50, 80 years old
# Coefficients are found from differrence in logits between 
logOR.tob <- svycontrast(m.final, list(
 cigs = c(0, 0, 1, 0, 0, 0, 0),
 pipe.20 = c(0, 0, 1, 0, 0, 0, 20),
 pipe.50 = c(0, 0, 1, 0, 0, 0, 50),
 pipe.80 = c(0, 0, 1, 0, 0, 0, 80),
 chew.20 = c(0, 0, 0, 1, 0, 0, 20),
 chew.50 = c(0, 0, 0, 1, 0, 0, 50),
 chew.80 = c(0, 0, 0, 1, 0, 0, 80)))

# Convert object into data frame for further computations
ORs.tob <- rbind(as.data.frame(logOR.tob))
ORs.tob$OR <- exp(ORs.tob$contrast)
ORs.tob$lower.CI <- exp((ORs.tob$contrast + qt(0.025, m.final$df.residual) * ORs.tob$SE))
ORs.tob$upper.CI <- exp((ORs.tob$contrast + qt(0.975, m.final$df.residual) * ORs.tob$SE))
round(ORs.tob[,3:5],2)

# Predicted P(symptoms) for a few cases
logits.reduced <- svycontrast(m.final, list(
 twenty_none = c(1, 20, 0, 0, 0, 0, 0),
 twenty_cig = c(1, 20, 1, 0, 0, 0, 0),
 fifty_none = c(1, 50, 0, 0, 0, 0, 0),
 fifty_cig = c(1, 50, 1, 0, 0, 0, 0),
 fifty_all = c(1, 50, 1, 1, 1, 50, 50)))

# Convert object into data frame for further computations
preds.reduced <- as.data.frame(logits.reduced)
preds.reduced$prob <- plogis(preds.reduced$contrast)
preds.reduced$lower.CI <- plogis(preds.reduced$contrast + qt(p = 0.025, df = m.final$df.residual) * preds.reduced$SE)
preds.reduced$upper.CI <- plogis(preds.reduced$contrast + qt(p = 0.975, df = m.final$df.residual) * preds.reduced$SE)
round(preds.reduced[,3:5], digits = 3)

##################################################################
# Plot model fit

# First make plots of the fits. Will plot pi-hat against age for 
# No tobacco, Cigarettes only, Pipe only, and Chew only.
# Since cigarette:age and chew:age are not in the final model, the odds ratios for 
#  a given change in age should be the same as no-use in these plots.
# However, both pipe:age is in the model, so age-ORs will
#  change compared to no tobacco use.

# Start by computing Nhats for saturated model, including Y binary
# Use "interaction({all terms in model},Y)"
tab <- svytotal(x = ~ interaction(age, sm_cigs, sm_pipe, sm_chew, anyresp), 
        design = jdesign)
N1 <- coef(tab)
lenN <- length(N1)
# Calculate conditional proportions of success, failure
# Phat contains proportion of failure for each level of covariates, followed by prop of success 
Nhat0 <- N1[1:(lenN/2)]
Nhat1 <- N1[(lenN/2+1):lenN]
Nplus <- Nhat0 + Nhat1
# Nplus <- ((Nhat0 + Nhat1), (Nhat0 + Nhat1))
# NPlus is total of successes and failures. Eliminate combinations with none of either and compute proportions
Phat <- Nhat1[which(Nplus>0)]/Nplus[which(Nplus>0)]
Nplus <- Nplus[which(Nplus>0)]
len <- length(Phat)
head(Phat)
len

# Gather explanatories, predicted values, and CI into one data frame
X0 <- as.data.frame(model.matrix(m.final))
# Aggregate into exp var pattern form
X0 <- unique(X0)
ord <- order(X0$sm_chew, X0$sm_pipe, X0$sm_cigs, X0$age)
X0 <- X0[ord,]
# pred.dat <- aggregate(x = X0, by = list(model.dat$age, model.dat$sm_cigs, model.dat$sm_pipe, 
#                     model.dat$sm_cigar, model.dat$sm_chew, model.dat$sm_snuff),
#            FUN = mean)[,-c(1:4)]
# Add observed proportions to data set
pred.dat <- cbind(X0, Phat, Nplus) 

# Function for computing predicted values and CIs
ci.pi <- function(newdata, mod.fit.obj, alpha){
 lin.pred <- coef(predict(object = mod.fit.obj, newdata = newdata, type = "link"))
 SE.lin.pred <- sqrt(diag(vcov(predict(object = mod.fit.obj, newdata = newdata, type = "link", vcov = TRUE))))
 CI.lp.lower <- lin.pred + qt(alpha/2, mod.fit.obj$df.residual) * SE.lin.pred
 CI.lp.upper <- lin.pred + qt(1-alpha/2, mod.fit.obj$df.residual) * SE.lin.pred
 pred <- plogis(lin.pred)
 CI.pi.lower <- exp(CI.lp.lower)/(1+exp(CI.lp.lower))
 CI.pi.upper <- exp(CI.lp.upper)/(1+exp(CI.lp.upper))
 list(pred = pred, lower = CI.pi.lower, upper = CI.pi.upper)
}

x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure6.3color.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book
# Split window into 4 frames
par(mfrow = c(2,2))
# Plot of observed and predicted prob(resp symp) for non-users

notob <- which(pred.dat$sm_cigs + pred.dat$sm_pipe + pred.dat$sm_chew == 0)

symbols(x = pred.dat$age[notob], 
    y = pred.dat$Phat[notob], 
    xlab = "Age", ylab = "Estimated probability", 
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "No tobacco use",
    circles = sqrt(pred.dat$Nplus[notob]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, 
                       sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black", 
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "red", 
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "red", 
   lty = "dashed", add = TRUE)
# Plot of observed and predicted prob(resp symp) for CIG-users

cigonly <- which((1 - pred.dat$sm_cigs) + pred.dat$sm_pipe + pred.dat$sm_chew == 0)

symbols(x = pred.dat$age[cigonly], 
    y = pred.dat$Phat[cigonly], 
    xlab = "Age", ylab = "Estimated probability", 
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "Cigarettes only",
    circles = sqrt(pred.dat$Nplus[cigonly]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 1, sm_pipe = 0, 
                    sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black", 
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 1, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "red", 
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 1, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "red", 
   lty = "dashed", add = TRUE)
# Plot of observed and predicted prob(resp symp) for PIPE-users
pipeonly <- which(pred.dat$sm_cigs + (1 - pred.dat$sm_pipe) + pred.dat$sm_chew == 0)

symbols(x = pred.dat$age[pipeonly], 
    y = pred.dat$Phat[pipeonly], 
    xlab = "Age", ylab = "Estimated probability", 
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "Pipe only",
    circles = sqrt(pred.dat$Nplus[pipeonly]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 1, 
                    sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black", 
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 1, sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "red", 
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 1, sm_cigar = 0, sm_snuff = 0, sm_chew = 0), 
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "red", 
   lty = "dashed", add = TRUE)
# Plot of observed and predicted prob(resp symp) for CHEW-users
chewonly <- which(pred.dat$sm_cigs + pred.dat$sm_pipe + (1 - pred.dat$sm_chew) == 0)

symbols(x = pred.dat$age[chewonly], 
    y = pred.dat$Phat[chewonly], 
    xlab = "Age", ylab = "Estimated probability", 
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "Chew only",
    circles = sqrt(pred.dat$Nplus[chewonly]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, 
                    sm_cigar = 0, sm_snuff = 0, sm_chew = 1), 
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black", 
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 1), 
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "red", 
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 1), 
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "red", 
   lty = "dashed", add = TRUE)
# dev.off()  # Create plot for book





# Black-and-white version of plot
x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure6.3BW.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book
# Split window into 4 frames
par(mfrow = c(2,2))
# Plot of observed and predicted prob(resp symp) for non-users

symbols(x = pred.dat$age[notob],
    y = pred.dat$Phat[notob],
    xlab = "Age", ylab = "Estimated probability",
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "No tobacco use",
    circles = sqrt(pred.dat$Nplus[notob]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0,
                       sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black",
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "black",
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "black",
   lty = "dashed", add = TRUE)
# Plot of observed and predicted prob(resp symp) for CIG-users

symbols(x = pred.dat$age[cigonly],
    y = pred.dat$Phat[cigonly],
    xlab = "Age", ylab = "Estimated probability",
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "Cigarettes only",
    circles = sqrt(pred.dat$Nplus[cigonly]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 1, sm_pipe = 0,
                    sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black",
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 1, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "black",
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 1, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "black",
   lty = "dashed", add = TRUE)
# Plot of observed and predicted prob(resp symp) for PIPE-users

symbols(x = pred.dat$age[pipeonly],
    y = pred.dat$Phat[pipeonly],
    xlab = "Age", ylab = "Estimated probability",
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "Pipe only",
    circles = sqrt(pred.dat$Nplus[pipeonly]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 1,
                    sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black",
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 1, sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "black",
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 1, sm_cigar = 0, sm_snuff = 0, sm_chew = 0),
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "black",
   lty = "dashed", add = TRUE)
# Plot of observed and predicted prob(resp symp) for CHEW-users

symbols(x = pred.dat$age[chewonly],
    y = pred.dat$Phat[chewonly],
    xlab = "Age", ylab = "Estimated probability",
    panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0,1), xlim = c(10,90), main = "Chew only",
    circles = sqrt(pred.dat$Nplus[chewonly]),
    inches = 0.07)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0,
                    sm_cigar = 0, sm_snuff = 0, sm_chew = 1),
          mod.fit.obj = m.final, alpha = 0.05)$pred, col = "black",
   lty = "solid", add = TRUE, lwd = 2)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 1),
          mod.fit.obj = m.final, alpha = 0.05)$lower, col = "black",
   lty = "dashed", add = TRUE)
curve(expr = ci.pi(newdata = data.frame(age = x, sm_cigs = 0, sm_pipe = 0, sm_cigar = 0, sm_snuff = 0, sm_chew = 1),
          mod.fit.obj = m.final, alpha = 0.05)$upper, col = "black",
   lty = "dashed", add = TRUE)
# dev.off()  # Create plot for book
