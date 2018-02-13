#####################################################################
# NAME: Chris Bilder                                                #
# DATE: 7-1-11                                                      #
# PURPOSE: Test for independence for Fisher's experiment            #
#                                                                   #
# NOTES:                                                            #
#####################################################################

# All possible probabilities 
M <- 0:4
# Syntax for dhyper(m, a, b, k)
data.frame(M, prob = round(dhyper(M, 4, 4, 4), 4))

c.table <- array(data = c(4, 0, 0, 4), dim = c(2,2), dimnames = list(Actual = c("Milk", "Tea"),
          Response = c("Milk", "Tea")))
c.table

fisher.test(x = c.table)
fisher.test(x = c.table, alternative = "greater")


###############################################################################
# Exact distribution for X^2

 X.sq <- c(chisq.test(x = matrix(data = c(0, 4, 4, 0), nrow = 2, ncol = 2), correct = FALSE)$statistic,
     chisq.test(x = matrix(data = c(1, 3, 3, 1), nrow = 2, ncol = 2), correct = FALSE)$statistic,
     chisq.test(x = matrix(data = c(2, 2, 2, 2), nrow = 2, ncol = 2), correct = FALSE)$statistic,
     chisq.test(x = matrix(data = c(3, 1, 1, 3), nrow = 2, ncol = 2), correct = FALSE)$statistic,
     chisq.test(x = matrix(data = c(4, 0, 0, 4), nrow = 2, ncol = 2), correct = FALSE)$statistic)
 pmf1 <- data.frame(X.sq, prob = round(dhyper(0:4, 4, 4, 4),4))
 pmf1

 # Find PMF over unique values of X^2 (See Chapter 2's placekick example for another use of this function)
 pmf2 <- aggregate(formula = prob ~ X.sq, data = pmf1, FUN = sum)
 cdf <- cumsum(pmf2$prob)
 pmf3 <- data.frame(pmf2, cdf)
 pmf3  # >1 in last element due to rounding error
 
 # Plot of CDFs
 x11(width = 7, height = 6, pointsize = 12)
 # pdf(file = "c:\\figures\\Figure6.1BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 plot(x = c(0,0,9,9), y = c(0,1,0,1), type = "n", ylab = "Cumulative probability", xlab = expression(X^2))
 abline(h = c(0,1), col = "black", lty = "dotted")
 curve(expr = pchisq(q = x, df = 1), xlim = c(0,9), col = "black", ylab = "Cumulative probability", xlab = expression(X^2), 
   add = TRUE, lwd = 2, n = 1000)
 curve(expr = pchisq(q = x, df = 1), xlim = c(9,12), col = "black", add = TRUE, lwd = 2)
 segments(x0 = -1, y0 = 0, x1 = 0, y1 = 0, lwd = 1, col = "black", lty = "dashed")
 segments(x0 = 0, y0 = 0, x1 = 0, y1 = 0.5143, lwd = 1, col = "black", lty = "dashed")
 segments(x0 = 0, y0 = 0.5143, x1 = 2, y1 = 0.5143, lwd = 1, col = "black", lty = "dashed")
 segments(x0 = 2, y0 = 0.5143, x1 = 2, y1 = 0.9714, lwd = 1, col = "black", lty = "dashed")
 segments(x0 = 2, y0 = 0.9714, x1 = 8, y1 = 0.9714, lwd = 1, col = "black", lty = "dashed")
 segments(x0 = 8, y0 = 0.9714, x1 = 8, y1 = 1, lwd = 1, col = "black", lty = "dashed")
 segments(x0 = 8, y0 = 1, x1 = 10, y1 = 1, lwd = 1, col = "black", lty = "dashed")
 legend(x = 6, y = 0.8, legend = c(expression(chi[1]^2),"Exact"), lty = c("solid","dashed"), lwd = c(2,1), col = c("black", "black"), bty = "n")
 # dev.off()  # Create plot for book



#