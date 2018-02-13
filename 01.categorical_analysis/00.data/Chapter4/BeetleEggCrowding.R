######################################################################
# NAME:  Tom Loughin                                                 #
# DATE:  2012-01-23                                                  #
# Purpose: Analyze beetle egg crowding data using Poisson Rate       #
#     regression.                                                    #
# NOTES:                                                             #
######################################################################

# Read in beetle egg mass data
eggdata <- read.table(file = "C:\\Data\\BeetleEggCrowding.txt", header = TRUE)
eggdata$females <- ifelse(test = eggdata$TRT == "I", yes = 1, no = 5)
head(eggdata)

# Getting mean rate per female for each combination of TRT and Temp
aggregate(formula = NumEggs ~ TRT + Temp, data = eggdata, FUN = mean, subset = TRT == "I")
aggregate(formula = NumEggs ~ TRT + Temp, data = eggdata, FUN = mean, subset = TRT == "G")$NumEggs/5

# Fit model using temperature, crowding level, and interaction
eggmod1 <- glm(NumEggs ~ Temp*TRT, family = poisson(link = "log"), offset = log(females), data = eggdata)
round(summary(eggmod1)$coefficients, digits = 3)
# Get the true egg-laying rate per female:
# Predicted means using observed t. 
newdata <- data.frame(TRT = c("G", "I", "G", "I"), Temp = c(21, 21, 24, 24), females = c(5, 1, 5, 1))
mu.hat <- round(predict(object = eggmod1, newdata = newdata, type = "response"), 2)
# Convert means to rates
data.frame(newdata, mu.hat, rate = mu.hat/newdata$females)

# Type II ANOVA Tests
library(car)
Anova(eggmod1)

# Matrix of linear combination coefficients for predicted rates at each combination
coef.mat <- as.matrix(rbind(c(1,21,1,21), c(1,24,1,24), c(1,21,0,0), c(1,24,0,0)))

# Wald inferences using multcomp package
library(multcomp)
wald <- glht(eggmod1, linfct = coef.mat)
# Defaults use multiplicity adjustment for simultaneous confidence level
summary(wald)
round(exp(confint(wald)$confint), digits = 2)
# Options to get unadjusted (univariate) tests and CIs
summary(wald, test = univariate())
wald.ci <- exp(confint(wald, calpha = qnorm(0.975))$confint)
round(wald.ci, digits = 2)

# Use mcprofile to get LR confidence intervals.
library(mcprofile)
LRCI <- mcprofile(eggmod1, CM = coef.mat)
exp(confint(LRCI, level = 0.95))
exp(confint(LRCI, level = 0.95, adjust = "none"))
wald.calc <- wald(LRCI)
exp(confint(wald.calc, level = 0.95, adjust = "none"))


# Plot mean rates and confidence intervals
x11(height = 6, width = 7, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.8BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
plot(x = c(21,24,21,24), y = wald.ci[,1], xlim = c(20,25.5), ylim = c(0,5), xlab = "Temperature", ylab = "Mean Egg Masses per Female")
segments(x0 = 21, x1 = 24, y0 = wald.ci[1,1], y1 = wald.ci[2,1], lty = "solid", lwd = 2)
segments(x0 = 21, x1 = 24, y0 = wald.ci[3,1], y1 = wald.ci[4,1], lty = "dotted", lwd = 2)
segments(x0 = 21, x1 = 21, y0 = wald.ci[1,2], y1 = wald.ci[1,3], lwd = 2)
segments(x0 = 21, x1 = 21, y0 = wald.ci[3,2], y1 = wald.ci[3,3], lwd = 2)
segments(x0 = 24, x1 = 24, y0 = wald.ci[2,2], y1 = wald.ci[2,3], lwd = 2)
segments(x0 = 24, x1 = 24, y0 = wald.ci[4,2], y1 = wald.ci[4,3], lwd = 2)
legend(x = 24, y = 1, legend = c("Individual", "Crowded"), cex = 0.9, lty = c("solid", "dotted"), lwd = 2, title = "TRT Level", bty = "n")
# dev.off()  # Create plot for book


# Plot mean rates and confidence intervals: Color
x11(height = 6, width = 7, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.8color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
plot(x = c(21,24,21,24), y = wald.ci[,1], xlim = c(20,25.5), ylim = c(0,5), xlab = "Temperature", ylab = "Mean Egg Masses per Female")
segments(x0 = 21, x1 = 24, y0 = wald.ci[1,1], y1 = wald.ci[2,1], col = "blue", lwd = 2)
segments(x0 = 21, x1 = 24, y0 = wald.ci[3,1], y1 = wald.ci[4,1], col = "red", lwd = 2)
segments(x0 = 21, x1 = 21, y0 = wald.ci[1,2], y1 = wald.ci[1,3], col = "blue", lwd = 2)
segments(x0 = 21, x1 = 21, y0 = wald.ci[3,2], y1 = wald.ci[3,3], col = "red", lwd = 2)
segments(x0 = 24, x1 = 24, y0 = wald.ci[2,2], y1 = wald.ci[2,3], col = "blue", lwd = 2)
segments(x0 = 24, x1 = 24, y0 = wald.ci[4,2], y1 = wald.ci[4,3], col = "red", lwd = 2)
legend(x = 24, y = 1, legend = c("Individual", "Crowded"), cex = 0.9, col = c("blue", "red"), lwd = 2, title = "TRT Level", bty = "n")
# dev.off()  # Create plot for book

