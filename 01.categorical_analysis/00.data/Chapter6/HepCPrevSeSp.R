###########################################################################
# NAME: Chris Bilder                                                      #
# DATE: 4-16-13                                                           #
# PURPOSE: Estimate prevalence under testing error                        #
# NOTES:                                                                  #
###########################################################################

################################################################################
# Wald interval

 # Basic computations
 w <- 42
 n <- 1875
 alpha <- 0.05
 pi.hat <- w/n
 Se <- 0.96
 Sp <- 0.99

 # Point estimate
 pi.hat <- w/n
 pi.tilde.hat <- (pi.hat + Sp - 1)/(Se + Sp - 1)
 data.frame(pi.hat, pi.tilde.hat)

 # Variance
 var.pi.hat <- pi.hat*(1 - pi.hat)/n
 var.pi.tilde.hat <- pi.hat*(1 - pi.hat)/(n*(Se + Sp - 1)^2)
 data.frame(var.pi.hat, var.pi.tilde.hat)

 # CIs
 # Wald for pi
 pi.hat + qnorm(p = c(alpha/2, 1 - alpha/2))*sqrt(var.pi.hat)
 # Wald for pi.tilde
 pi.tilde.hat + qnorm(p = c(alpha/2, 1 - alpha/2))*sqrt(var.pi.tilde.hat)


################################################################################
# Compare with different values of Se and Sp

 # Function for caclulations
 calc.it <- function(w, n, Se, Sp, alpha = 0.05) {
  pi.hat <- w/n
  pi.tilde.hat <- (pi.hat + Sp - 1)/(Se + Sp - 1)
  var.pi.tilde.hat <- pi.hat*(1 - pi.hat)/(n*(Se + Sp - 1)^2)
  wald <- pi.tilde.hat + qnorm(p = c(alpha/2, 1 - alpha/2))*sqrt(var.pi.tilde.hat)
  c(round(pi.tilde.hat,5), round(var.pi.tilde.hat,8), round(wald,8))
 }

 # Using values from Table 3 of Wilkins et al. (2010)
 calc.it(w = w, n = n, Se = 0.79, Sp = 0.80)  # Recombinant immunoblot assay
 calc.it(w = w, n = n, Se = 0.87, Sp = 0.99)  # Saliva-based anti-HCV
 calc.it(w = w, n = n, Se = 0.94, Sp = 0.97)  # anit-HCV (ELISA) Se = 0.94 to 1.00 and Sp = 0.97 to 0.98
 calc.it(w = w, n = n, Se = 1.00, Sp = 0.98)  # anit-HCV (ELISA)
 calc.it(w = w, n = n, Se = 0.96, Sp = 0.99)  # PCR





#