#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-8-11                                                     #
# Purpose: Examine the logistic distribution                        #
#                                                                   #
# NOTES:                                                            #
#####################################################################

mu<--2
sigma<-2

# Examples for f(-2) and F(-2)
dlogis(x = -2, location = mu, scale = sigma)
plogis(q = -2, location = mu, scale = sigma)

x11(width = 10, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure2.12BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
par(mfrow = c(1,2))
# par(pty="s")  # Makes plot square

curve(expr = dlogis(x = x, location = mu, scale = sigma), ylab = "f(x)", xlab = "x", xlim = c(-15, 15),
      main = "PDF", col = "black", n = 1000)
abline(h = 0)

curve(expr = plogis(q = x, location = mu, scale = sigma), ylab = "F(x)", xlab = "x", xlim = c(-15, 15),
      main = "CDF", col = "black", n = 1000)
# abline(h = c(0,1))
# dev.off()  # Create plot for book

