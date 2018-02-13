########################################################################
# NAME: Tom Loughin                                                    #
# DATE: 08-26-2013                                                     #
# PURPOSE: Simulate random effects for logistic regression and compare # 
#          curve with average random effect to average curve           #
#                                                                      #
# NOTES:                                                               #
########################################################################
set.seed(3789022)

beta1 <- 0.2
beta0 <- -100*beta1
b <- rnorm(n = 50, mean = 0, sd = 1.5)
x <- seq(from = 70, to = 130, by = .5)
probs <- matrix(data = 0, ncol = 50, nrow = length(x))

x11(height = 7, width = 12)
# pdf(file = "c:\\figures\\Figure6.8color.pdf", width = 12, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book
curve(expr = plogis(beta0 + beta1*x), from = 70, to = 130, col = "red", lwd = 2, ylab = "P(Y = 1)")

for(i in c(1:50)){
 curve(expr = plogis((beta0 + b[i]) + beta1*x), from = 70, to = 130, 
    col = "black", lwd = 1, lty = "dotted", add = TRUE)
 probs[,i] <- plogis((beta0 + b[i]) + beta1*x)
 
}
curve(expr = plogis(beta0 + beta1*x), from = 70, to = 130, col = "red", lwd = 3, add = TRUE)

marg <- apply(X = probs, MARGIN = 1, FUN = mean)
lines(x = x, y = marg, lty = "dashed", lwd = 3, col = "blue")
legend(x = 115, y = 0.3, legend = c("Subject-specific", "Marginal", "Individual"), bty = "n",
    lty = c("solid", "dashed", "dotted"), lwd = c(3,3,1), col = c("red", "blue", "black"))
# dev.off()  # Create plot for book


# Black-and-white version of plot
# pdf(file = "c:\\figures\\Figure6.8BW.pdf", width = 12, height = 6, colormodel = "cmyk", pointsize = 14)   # Create plot for book
curve(expr = plogis(beta0 + beta1*x), from = 70, to = 130, col = "red", lwd = 2, ylab = "P(Y = 1)")

for(i in c(1:50)){
 curve(expr = plogis((beta0 + b[i]) + beta1*x), from = 70, to = 130,
    col = "black", lwd = 1, lty = "dotted", add = TRUE)
 probs[,i] <- plogis((beta0 + b[i]) + beta1*x)

}
curve(expr = plogis(beta0 + beta1*x), from = 70, to = 130, col = "black", lwd = 3, add = TRUE)

marg <- apply(X = probs, MARGIN = 1, FUN = mean)
lines(x = x, y = marg, lty = "dashed", lwd = 3, col = "black")
legend(x = 115, y = 0.3, legend = c("Subject-specific", "Marginal", "Individual"), bty = "n",
    lty = c("solid", "dashed", "dotted"), lwd = c(3,3,1), col = c("black", "black", "black"))
# dev.off()  # Create plot for book
