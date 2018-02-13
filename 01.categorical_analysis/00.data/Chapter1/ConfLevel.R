###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  10-1-10                                                          #
# PURPOSE: True confidence level for Wald interval                        #
# NOTES:                                                                  #
###########################################################################

# Initial settings
alpha<-0.05
pi<-0.157
# pi<-0.156
n<-40


###########################################################################
# True confidence level

  w<-0:n
  pi.hat<-w/n
  pmf<-dbinom(x = w, size = n, prob = pi)
  var.wald<-pi.hat*(1-pi.hat)/n
  lower<-pi.hat - qnorm(p = 1-alpha/2) * sqrt(var.wald)
  upper<-pi.hat + qnorm(p = 1-alpha/2) * sqrt(var.wald)
  save<-ifelse(test = pi>lower, yes = ifelse(test = pi<upper, yes = 1, no = 0), no = 0)
  sum(save*pmf)
  data.frame(w, pi.hat, round(data.frame(pmf, lower, upper),4), save)[1:13,]

  # For pi = 0.157
  sum(dbinom(x = 4:11, size = n, prob = pi))
  # For pi = 0.156
  # sum(dbinom(x = 3:11, size = n, prob = pi))


###########################################################################
# Estimated true confidence level

  numb.bin.samples<-1000  # Number of binomial samples of size n

  set.seed(4516)
  w<-rbinom(n = numb.bin.samples, size = n, prob = pi)
  counts<-table(x = w)
  counts
  sum(counts[4:11])/numb.bin.samples

  pi.hat<-w/n
  pi.hat[1:10]
  var.wald<-pi.hat*(1-pi.hat)/n
  lower<-pi.hat - qnorm(p = 1-alpha/2) * sqrt(var.wald)
  upper<-pi.hat + qnorm(p = 1-alpha/2) * sqrt(var.wald)
  data.frame(w, pi.hat, lower, upper)[1:10,]

  save<-ifelse(test = pi>lower, yes = ifelse(test = pi<upper, yes = 1, no = 0), no = 0)
  save[1:10]
  mean(save)
  true.conf<-mean(save)
  cat("An estimate of the true confidence level is:", round(true.conf,4), "\n")


  library(package = binom)
  binom.confint(x = sum(save), n = numb.bin.samples, conf.level = 1-alpha, methods = "wilson")

 
###########################################################################
# Compare the two ways

  # Simulate same samples again
  set.seed(4516)
  w<-rbinom(n = numb.bin.samples, size = n, prob = pi)

  table(w)  # Frequency for each w observed
  prop.w<-table(w)/numb.bin.samples  # Proportion for each w
  obs.w<-as.integer(names(table(w)))  # Obtain w number
  binom.prob<-round(dbinom(x = obs.w, size = n, prob = pi),4)
  data.frame(w = obs.w, obs.prop = prop.w, binom.prob = binom.prob)

  sum(prop.w)
  sum(binom.prob)  # Note: not equal to 1 because some possible values of w were not observed


#
