###########################################################################
# NAME:  Chris Bilder  / Tom Loughin                                      #
# DATE:  2012-01-24                                                       #
# PURPOSE: True confidence level for Wald, Score, Exact, and t interval   #
#   for the parameter of a Poisson distribution                           #
# NOTES:                                                                  #
###########################################################################

# Initial settings and calculations
alpha <- 0.05
n <- 5
sum.y <- c(0:(40*n))
y.bar <- sum.y/n

# Wald
lower.wald <- y.bar - qnorm(p = 1 - alpha/2)*sqrt(y.bar/n)
upper.wald <- y.bar + qnorm(p = 1 - alpha/2)*sqrt(y.bar/n)
length.wald <- upper.wald - lower.wald

# Score
lower.score <- (y.bar + qnorm(p = 1 - alpha/2)^2/(2*n)) - qnorm(p = 1 - alpha/2) * sqrt((y.bar + qnorm(p = 1 - alpha/2)^2/(4*n))/n)
upper.score <- (y.bar + qnorm(p = 1 - alpha/2)^2/(2*n)) + qnorm(p = 1 - alpha/2) * sqrt((y.bar + qnorm(p = 1 - alpha/2)^2/(4*n))/n)
length.score <- upper.score - lower.score

# Exact
lower.exact <- qchisq(p = alpha/2, df = 2*sum.y)/(2*n)
upper.exact <- qchisq(p = 1 - alpha/2, df = 2*(sum.y + 1))/(2*n)
length.exact <- upper.exact - lower.exact

# All mu's
mu.seq <- seq(from = .01, to = 20, by = .01)

# Save true confidence levels in a matrix
save.true.conf <- matrix(data = NA, nrow = length(mu.seq), ncol = 9)

# Create counter for the loop
counter <- 1

# Function to simulate Poisson data and calculate mean and variance
mv.func <- function(mu, R, n) {
  y <- matrix(data = rpois(R*n, lambda = mu), nrow = n, ncol = R) 

  mean.y <- apply(X = y, MARGIN = 2, FUN = mean)
  var.y <- apply(X = y, MARGIN = 2, FUN = var)

  cbind(mean.y, var.y)
}

# Loop over each pi that the true confidence level is calculated on each
for(mu in mu.seq) {

  pmf <- dpois(x = sum.y, lambda = n*mu)

  # Wald
  save.wald <- ifelse(test = mu > lower.wald, yes = ifelse(test = mu < upper.wald, yes = 1, no = 0), no = 0)
  wald <- sum(save.wald * pmf)
  wald.l <- sum(length.wald * pmf)

  # Score
  save.score <- ifelse(test = mu > lower.score, yes = ifelse(test = mu < upper.score, yes = 1, no = 0), no = 0)
  score <- sum(save.score * pmf)
  score.l <- sum(length.score * pmf)
  
  # exact
  save.exact <- ifelse(test = mu > lower.exact, yes = ifelse(test = mu < upper.exact, yes = 1, no = 0), no = 0)
  exact <- sum(save.exact * pmf)
  exact.l <- sum(length.exact * pmf)
  
  # t: Generate data, compute mean and variance with mv.func
  R = 1825
  mean.var <- mv.func(mu = mu, R = R, n = n)

  # t intervals
  lowert <- mean.var[,1] - qt(p = 1 - alpha/2, df = n - 1) * sqrt(mean.var[,2]/n)
  uppert <- mean.var[,1] + qt(p = 1 - alpha/2, df = n - 1) * sqrt(mean.var[,2]/n)
  length.t <- uppert - lowert

  save.t <- ifelse(test = mu > lowert, yes = ifelse(test = mu < uppert, yes = 1, no = 0), no = 0)
  tt <- mean(save.t)
  t.l <- mean(length.t)

# Save all coverage estimates and lengths for this mu
  save.true.conf[counter,] <- c(mu, wald, score, exact, tt, wald.l, score.l, exact.l, t.l)
  counter <- counter + 1
  
}
  
###################
# CHRIS EDITED HERE
# Plots: Confidence level B/W
x11(width = 12, height = 5, pointsize = 20)
# pdf(file = "c:\\figures\\Figure4.2-1BW.pdf", width = 12, height = 5, colormodel = "cmyk", pointsize = 20)   # Create plot for book
par(mfrow = c(1,4)) #1x4 plotting grid
par(mar =  c(5, 4, 4, 0) + 0.1) #Extends right margin to edge of plot

plot(x = save.true.conf[,1], y = save.true.conf[,2], main = paste("Wald,n=", n, sep=""),
  xlab = expression(mu), ylab = "True confidence level", type = "l", ylim = c(0.8,1))
abline(h = 1 - alpha, lty = "dotted") #Can add , col="red"
axis(side = 4, at = seq(from = 0.8, to = 1, by = 0.05), labels = FALSE)
plot(x = save.true.conf[,1], y = save.true.conf[,3], main = paste("Score,n=", n, sep=""),
  xlab = expression(mu), ylab = "", type = "l", ylim = c(0.8,1),
  yaxt = "n")
axis(side = 2, at = seq(from = 0.8, to = 1, by = 0.05), labels = FALSE)
axis(side = 4, at = seq(from = 0.8, to = 1, by = 0.05), labels = FALSE)
abline(h = 1 - alpha, lty = "dotted") #Can add , col="red"
plot(x = save.true.conf[,1], y = save.true.conf[,4], main = paste("Exact,n=", n, sep=""),
  xlab = expression(mu), ylab = "", type = "l", ylim = c(0.8,1), yaxt = "n")
axis(side = 2, at = seq(from = 0.8, to = 1, by = 0.05), labels = FALSE)
axis(side = 4, at = seq(from = 0.8, to = 1, by = 0.05), labels = FALSE)
abline(h = 1 - alpha, lty = "dotted") #Can add , col="red"
plot(x = save.true.conf[,1], y = save.true.conf[,5], main = paste("t,n=", n, sep=""),
 xlab = expression(mu), ylab = NA, type = "l", ylim = c(0.8,1), yaxt = "n")
axis(side = 2, at = seq(from = 0.8, to = 1, by = 0.05), labels = FALSE)
axis(side = 4, at = seq(from = 0.8, to = 1, by = 0.05), labels = FALSE)
abline(h = 1 - alpha + 1.96*sqrt(alpha*(1 - alpha)/R), lty = "dotted")
abline(h = 1 - alpha - 1.96*sqrt(alpha*(1 - alpha)/R), lty = "dotted")
abline(h = 1 - alpha, lty = "dotted") #Can add , col="red"
par(mar =  c(5, 4, 4, 2) + 0.1) #default
# dev.off()  # Create plot for book

# Could also  reduce the second element of mar, but the "True confidence level"
#  y-axis label on the first plot would be left off. It may stil be clear from the figure caption what is plotted.
###################

# Plots: average length: B/W
x11(width = 7, height = 6, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.3-1BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
# pdf(file = "c:\\figures\\Figure4.3-2BW.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
plot(x = save.true.conf[,1], y = save.true.conf[,6], main = paste("Mean length of Intervals, n=", n, sep = ""),
  xlab = expression(mu), ylim = c(0, floor(30/sqrt(n))), ylab = "Mean Length", type = "l", lty = 3, lwd = 2)
lines(x = save.true.conf[,1], y = save.true.conf[,7],  type = "l", lty = 1, lwd = 2)
lines(x = save.true.conf[,1], y = save.true.conf[,8],  type = "l", lty = 4, lwd = 2)
lines(x = save.true.conf[,1], y = save.true.conf[,9],  type = "l", lty = 2, lwd = 2)
legend(x = 0, y = floor(30/sqrt(n)), legend = c("Wald", "Score", "Exact", "t"), lty = c("solid", "dotted", "dotdash", "dashed"), lwd = 2, bty = "n")
# dev.off()  # Create plot for book


# Plots: Average length at small mu: B/W
x11(width = 7, height = 6, pointsize = 15)
plot(x = save.true.conf[,1], y = save.true.conf[,6], main = paste("Mean length of Intervals, n=", n, sep=""),
  xlab = expression(mu), ylab = "Mean Length", type = "l", lty = 3, lwd = 2, xlim = c(0,1),
  ylim= c(0,ceiling(4/sqrt(n))))
lines(x = save.true.conf[,1], y = save.true.conf[,7],  type = "l", lty = 1, lwd = 2)
lines(x = save.true.conf[,1], y = save.true.conf[,8],  type = "l", lty = 4, lwd = 2)
lines(x = save.true.conf[,1], y = save.true.conf[,9],  type = "l", lty = 2, lwd = 2)
legend(x = 0, y = floor(30/sqrt(n)), legend = c("Wald", "Score", "Exact", "t"), lty = c("solid", "dotted", "dotdash", "dashed"), lwd = 2, bty = "n")

# Plots: average length: Color
x11(width = 7, height = 6, pointsize = 15)
# pdf(file = "c:\\figures\\Figure4.3-1color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
# pdf(file = "c:\\figures\\Figure4.3-2color.pdf", width = 7, height = 6, colormodel = "cmyk", pointsize = 15)   # Create plot for book
plot(x = save.true.conf[,1], y = save.true.conf[,6], main = paste("Mean length of Intervals, n=", n, sep = ""),
  xlab = expression(mu), ylim = c(0,floor(30/sqrt(n))), ylab = "Mean Length", type = "l", lty = 3, lwd = 2)
lines(x = save.true.conf[,1], y = save.true.conf[,7],  type = "l", lty = 1, lwd = 2, col="blue")
lines(x = save.true.conf[,1], y = save.true.conf[,9],  type = "l", lty = 2, lwd = 2, col="orange")
lines(x = save.true.conf[,1], y = save.true.conf[,8],  type = "l", lty = 4, lwd = 2, col="green")
legend(x = 0, y = floor(30/sqrt(n)), legend=c("Wald", "Score", "Exact", "t"), lty = c(3,1,4,2), col = c("black", "blue", "green", "orange"), lwd = 2, bty = "n")
# dev.off()  # Create plot for book

# Plots: Average length at small mu: Color
x11(width = 7, height = 6, pointsize = 15)
plot(x = save.true.conf[,1], y = save.true.conf[,6], main = paste("Mean length of Intervals, n=", n, sep = ""),
  xlab = expression(mu), ylab = "Mean Length", type = "l", lty = 3, lwd = 2, xlim = c(0,1), ylim = c(0,ceiling(4/sqrt(n))))
lines(x = save.true.conf[,1], y = save.true.conf[,7],  type = "l", lty = 1, lwd = 2, col="blue")
lines(x = save.true.conf[,1], y = save.true.conf[,8],  type = "l", lty = 4, lwd = 2, col="green")
lines(x = save.true.conf[,1], y = save.true.conf[,9],  type = "l", lty = 2, lwd = 2, col="orange")
legend(x = 0, y = ceiling(4/sqrt(n)), legend = c("Wald", "Score", "Exact", "t"), lty = c(3,1,4,2), col = c("black", "blue", "green", "orange"), lwd = 2, bty = "n")


