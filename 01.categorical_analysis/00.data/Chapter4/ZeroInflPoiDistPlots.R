#####################################################################
# NAME:  Tom Loughin and Chris Bilder                               #
# DATE:  01-23-12                                                   #
# PURPOSE: Show PMF of Zeri-Inflated Poisson distribution with      #
#          various pi, mu                                           #
#                                                                   #
# NOTES: Makes plot only,  Program not discussed in text            #
#####################################################################

# Zero Inflated Poisson with probability of immune=pi, mean for Poisson=mu

# Create grid of pi, mu values, add pmf calculation
all <- expand.grid(y = c(0:20), pi = c(0.5,0.2,0), mu = c(10,2))
all <- data.frame(all, p = (1 - all$pi) * dpois(x = all$y, lambda = all$mu) + ifelse(all$y == 0, yes = all$pi, no = 0))

head(all)

# Make plots
x11(width = 10, height = 7, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.9BW.pdf", width = 10, height = 7, colormodel = "cmyk", pointsize = 20)   # Create plot for book
par(mfrow = c(2,3))

plot(x = all$y[1:21], y = all$p[1:21], type = "h", xlim = c(0,20), xlab = "y", ylab = "P(Y=y)", lty = "solid", lwd = 3, main = expression(paste(pi, "=0.5, ", mu, "=10")))
plot(x = all$y[22:42], y = all$p[22:42], type = "h", xlim = c(0,20), xlab = "y", ylab = "P(Y=y)", lty = "solid", lwd = 3, main = expression(paste(pi, "=0.2, ", mu, "=10")))
plot(x = all$y[43:63], y = all$p[43:63], type = "h", xlim = c(0,20), xlab = "y", ylab = "P(Y=y)", lty = "solid", lwd = 3, main = expression(paste(pi, "=0, ", mu, "=10")))
plot(x = all$y[64:84], y = all$p[64:84], type = "h", xlim = c(0,20), xlab = "y", ylab = "P(Y=y)", lty = "solid", lwd = 3, main = expression(paste(pi, "=0.5, ", mu, "=2")))
plot(x = all$y[85:105], y = all$p[85:105], type = "h", xlim = c(0,20), xlab = "y", ylab = "P(Y=y)", lty = "solid", lwd = 3, main = expression(paste(pi, "=0.2, ", mu, "=2")))
plot(x = all$y[106:126], y = all$p[106:126], type = "h", xlim  = c(0,20), xlab = "y", ylab = "P(Y=y)", lty = "solid", lwd = 3, main = expression(paste(pi, "=0, ", mu, "=2")))
# dev.off()  # Create plot for book



