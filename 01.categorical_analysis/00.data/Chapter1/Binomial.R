###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  9-16-10                                                          #
# PURPOSE: Binomial example                                               #
# NOTES:                                                                  #
###########################################################################

#P (W = 1) with n = 5 and pi = 0.6
dbinom(x = 1, size = 5, prob = 0.6) 

# The entire pmf
pmf<-dbinom(x = 0:5, size = 5, prob = 0.6) 
pmf

# Make the printing look a little nicer
pmf<-dbinom(x = 0:5, size = 5, prob = 0.6) 
save<-data.frame(w = 0:5, prob = round(x = pmf, digits = 4))
save 

# Plot PMF
x11(width = 6, height = 6, pointsize = 12)  # Use dev.new() for non-windows computers, x11() works too
# pdf(file = "c:\\figures\\Figure1.1.pdf", width = 6, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = save$w, y = save$prob, type = "h", xlab = "w", ylab = "P(W=w)",
     main = "Plot of a binomial PMF for n=5, pi=0.6", 
     panel.first = grid(col = "gray", lty = "dotted"), lwd = 3)
abline(h = 0)
# dev.off()  # Create plot for book

# Alternative plot using the expression() function to use better notation
plot(x = save$w, y = save$prob, type = "h", xlab = "w", ylab = "P(W=w)",
     main = expression(paste("Plot of a binomial PMF for ", italic(n) == 5, " and ", italic(pi) == 0.6)), 
     panel.first = grid(col="gray", lty="dotted"), lwd = 3)
abline(h = 0)

# Simulate observations from a Binomial distribution
set.seed(4848)
bin5<-rbinom(n = 1000, size = 5, prob = 0.6)
bin5[1:10]

mean(bin5)
var(bin5)

# Frequency distribution
table(x = bin5) 
x11(width = 6, height = 6, pointsize = 12)
# Relative frequency histogram
hist(x = bin5, main = "Binomial with n=5, pi=0.6, 1000 observations", probability = TRUE,
  ylab = "Relative frequency")  # Far left bar is not drawn correctly using the default breaks
# pdf(file = "c:\\figures\\Figure1.2.pdf", width = 6, height = 6, colormodel = "cmyk")   # Create plot for book
hist(x = bin5, main = "Binomial with n=5, pi=0.6, 1000 observations", probability = TRUE,
  breaks = c(-0.5:5.5), ylab = "Relative frequency")
# dev.off()  # Create plot for book


# Better plot
save.count<-table(bin5)
save.count
barplot(height = save.count, names = c("0", "1", "2", "3", "4", "5"), main = "Binomial with n=5, pi=0.6, 1000 bin. observations", xlab = "x")
