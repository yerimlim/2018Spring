#####################################################################
# NAME:  Tom Loughin and Chris Bilder                               #
# DATE:  01-23-12                                                   #
# PURPOSE: Compare Poisson and Normal distributions with same mean  #
#          and variance                                             #
#                                                                   #
# NOTES:                                                            #
#####################################################################

mu <- 100
# setting limits for computing probabilities
lower <- round(x = mu - 4*sqrt(mu) + 0.5, digits = 0) 
upper <- round(x = mu + 4*sqrt(mu) + 0.5, digits = 0)

# Plot Po(mu) with corresponding N(mu,mu)
save.po1 <- data.frame(y = c(lower:upper), prob = round(x = dpois(x = c(lower:upper), lambda = mu), digits = 4))
save.po1

# Plot probability mass function as histogram with discrete lines
x11(width = 7, height = 6, pointsize = 12)
plot(x = save.po1$y, y = save.po1$prob, type = "h", xlab = "y", ylab = "P(Y=y)")
# Add normal distribution with mean mu and variance mu
curve(expr = ((2*pi*mu)^(-1/2)) * exp(-(x-mu)^2/(2*mu)), from = lower, to = upper, add = TRUE, lty = "dotted")
abline(h = 0)

# Make QQ plot: Better than histogram for comparing distributions
p <- c(1:999)/1000
poiq <- qpois(p, lambda = mu)
norq <- qnorm(p, mean = mu, sd = sqrt(mu))

x11(height = 6, width = 6)
plot(x = norq, y = poiq, main = expression(paste(mu, "=100",)))
abline(a = 0, b = 1)
