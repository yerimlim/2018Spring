set.seed(2895612)
x = c(1:10)
set1 <- data.frame(x, y = x + rnorm(n = 10))
lin <- lm(formula = y~x, data = set1)
p.lin <- predict(lin, newdata = set1)

x11(height = 6, width = 7, pointsize = 12)
# pdf(file = "c:\\figures\\FigureB.5BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = set1$x, y = set1$y, pch = 16, lty = "dotted", type = "b", xlab = "X", ylab = "Y", lwd = 2)
abline(a = lin$coefficients[1], b = lin$coefficients[2], lty = "solid", lwd = 2)
legend(x = 1, y = 12, legend = c("Linear Regression", "Saturated Model"), lty = c("solid", "dotted"), bty = "n",
 lwd = 2)
# dev.off()  # Create plot for book
