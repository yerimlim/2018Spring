# This program is not referenced in the book, but it is referenced in
#   workshops that we give corresponding to the book.

w <- 48
n <- 64
alpha <- 0.05

pi.hat <- w/n
pi.hat

pi.tilde <- (w + qnorm(p = 1-alpha/2)^2 /2) / (n + qnorm(p = 1-alpha/2)^2)
pi.tilde

wilson <- pi.tilde + qnorm(p = c(alpha/2, 1-alpha/2))*sqrt(n)/(n+qnorm(p = 1-       alpha/2)^2) * sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n))
round(wilson, digits = 4)

library(package = binom)
binom.confint(x = w, n = n, conf.level = 1-alpha, methods = "wilson")