######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  9-23-10                                                     #
# UPDATE:                                                            #
# Purpose: Compute and plot the likelihood function                  #
#                                                                    #
# NOTES:                                                             #
#                                                                    #
######################################################################

sum.y<-4
n<-10
pi<-c(0.2, 0.3, 0.35, 0.39, 0.4, 0.41, 0.5)
Lik<-pi^sum.y*(1-pi)^(n-sum.y)
data.frame(pi, Lik)

x11(width = 7, height = 6, pointsize = 12)
# Likelihood function plot
# pdf(file = "c:\\figures\\FigureB.1BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
curve(expr = x^sum.y*(1-x)^(n-sum.y), from = 0, to = 1,
      xlab = expression(pi), ylab = "Likelihood function")
abline(h = 0)
# dev.off()  # Create plot for book

# Log-likelihood function plot
curve(expr = sum.y*log(x) + (n-sum.y)*log(1-x), from = 0, to = 1,
      xlab = expression(pi), ylab = "Log-likelihood function")

# Similar plot but using plot() function
pi<-seq(from = 0, to = 1, by = 0.01)
Lik<-pi^sum.y*(1-pi)^(n-sum.y)
save<-data.frame(pi, Lik)
head(save)
plot(x = pi, y = Lik, xlab = expression(pi), xlim = c(0,1), ylab = "Likelihood function", type = "l")  
  
# Y-axis and title uses expression() as well
curve(expr = x^sum.y*(1-x)^(n-sum.y), from = 0, to = 1, col = "red",
      main = expression(paste("Likelihood function for ", pi)),
      xlab = expression(pi), ylab = expression(paste(L, "(", pi, "|", y[1],...,y[n],")" ) ))
abline(h=0)      


#
