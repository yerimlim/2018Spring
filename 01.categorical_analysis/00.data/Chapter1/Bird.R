#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  11-16-10                                                   #
# PURPOSE: Larry Bird example                                       #
#                                                                   #
# NOTES:                                                            #
#####################################################################


# Create contingency table - notice the data is entered by columns
c.table<-array(data = c(251, 48, 34, 5), dim = c(2,2), dimnames = list(First = c("made", "missed"),
             Second = c("made", "missed")))
list(First = c("made", "missed"), Second = c("made", "missed"))  # See dimnames
c.table  # Whole table
c.table[1,1]  # w1
c.table[1,]  # w1 and n1-w1
sum(c.table[1,])  # n1
rowSums(c.table)  # n1 and n2

# Find the estimated pi^j
pi.hat.table<-c.table/rowSums(c.table)
pi.hat.table
sum(pi.hat.table[1,])

# Another way to create a contingency table
c.table2<-array(data = c(251, 48, 34, 5), dim = c(2,2), dimnames = list(c("first made", "first missed"),
             c("second made", "second missed")))
c.table2



#####################################################################
# What if the data did not already come in a contingency table format?

  # Create "raw" data
  miss.miss<-matrix(rep(c("missed", "missed"), 5), 5,2, byrow=T)
  miss.make<-matrix(rep(c("missed", "made"), 48), 48,2, byrow=T)
  make.miss<-matrix(rep(c("made", "missed"), 34), 34,2, byrow=T)
  make.make<-matrix(rep(c("made", "made"), 251), 251,2, byrow=T)

  # Put "raw" data into one data.frame
  all.data<-rbind(miss.miss, miss.make, make.miss, make.make)
  all.data2<-data.frame(all.data)

  # Gives new names to columns
  names(all.data2)<-c("first", "second")
  # Rearrange rows to "simulate" how the data may have been observed as
  set.seed(9212)
  all.data2<-all.data2[sample(x = 1:nrow(all.data2), replace = FALSE),]
  row.names(all.data2)<-NULL  # Remove original row numbers
  head(all.data2)
 
  # Find contingency table two different ways
  bird.table1<-table(all.data2$first, all.data2$second)
  bird.table1
  bird.table1[1,1]  # w1

  bird.table2<-xtabs(formula = ~ first + second, data = all.data2) 
  bird.table2
  bird.table2[1,1]  # w1
  bird.table2/rowSums(bird.table2)
  # summary(bird.table2)  # Provides Pearson chi-square test for independence




#####################################################################
# Confidence interval for difference of two probabilities

  alpha<-0.05
  pi.hat1<-pi.hat.table[1,1]
  pi.hat2<-pi.hat.table[2,1]

  # Wald
  var.wald<-pi.hat1*(1-pi.hat1) / sum(c.table[1,]) + pi.hat2*(1-pi.hat2) / sum(c.table[2,])
  pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.wald)

  # Agresti-Caffo
  pi.tilde1<-(c.table[1,1]+1)/(sum(c.table[1,])+2)
  pi.tilde2<-(c.table[2,1]+1)/(sum(c.table[2,])+2)
  var.AC<-pi.tilde1*(1-pi.tilde1) / (sum(c.table[1,])+2) + pi.tilde2*(1-pi.tilde2) / (sum(c.table[2,])+2)
  pi.tilde1 - pi.tilde2 + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.AC)

  # Note: Each interval limit could be calculated one at a time as well. For example,
  #      the Wald interval is
  lower<-pi.hat1 - pi.hat2 - qnorm(p = 1-alpha/2) *
    sqrt(pi.hat1*(1-pi.hat1) / sum(c.table[1,]) +
    pi.hat2*(1-pi.hat2) / sum(c.table[2,]))
  upper<-pi.hat1 - pi.hat2 + qnorm(p = 1-alpha/2) *
    sqrt(pi.hat1*(1-pi.hat1) / sum(c.table[1,]) +
    pi.hat2*(1-pi.hat2) / sum(c.table[2,]))
  data.frame(lower, upper)



  ####################################################
  # Other ways to get the C.I.s

  # We could also avoid using tables and obtain the Wald interval
  w1<-251
  n1<-285
  w2<-48
  n2<-53
  alpha<-0.05
  pi.hat1<-w1/n1
  pi.hat2<-w2/n2
  var.wald<-pi.hat1*(1-pi.hat1) / n1 +  pi.hat2*(1-pi.hat2) / n2
  pi.hat1 - pi.hat2 + qnorm(p = c(alpha/2, 1-alpha/2)) *  sqrt(var.wald)

  # C.I. and also hypothesis test for Ho: pi_1|1 - pi_1|2
  prop.test(x = c.table[,1], n = rowSums(c.table), conf.level = 0.95, correct = FALSE)
  prop.test(x = c.table, conf.level = 0.95, correct = FALSE)

    # Wald statistic
    Z.0<-(pi.hat1 - pi.hat2)/sqrt( pi.hat1*(1-pi.hat1)/sum(c.table[1,]) + pi.hat2*(1-pi.hat2)/sum(c.table[2,]) )
    Z.0^2
  
    # Incorporate null hypothesis into variance in the denominator
    pi.hat.Ho<-sum(c.table[,1])/sum(c.table)
    Z.0<-(pi.hat1 - pi.hat2)/sqrt( pi.hat.Ho*(1-pi.hat.Ho)*(1/sum(c.table[1,]) + 1/sum(c.table[2,])) )
    Z.0^2
  

  # Calculations using the PropCIs package
  library(package = PropCIs)
  
  # Wald
  wald2ci(x1 = c.table[1,1], n1 = sum(c.table[1,]), x2 = c.table[2,1], n2 = sum(c.table[2,]),
    conf.level = 0.95, adjust = "Wald")
  
  # Agresti-Caffo
  wald2ci(x1 = c.table[1,1], n1 = sum(c.table[1,]), x2 = c.table[2,1], n2 = sum(c.table[2,]),
    conf.level = 0.95, adjust = "AC")


#####################################################################
# Hypothesis test for difference of two probabilities

  prop.test(x = c.table, conf.level = 0.95, correct = FALSE)

  # LRT
  pi.bar<-colSums(c.table)[1]/sum(c.table)
  log.Lambda<-c.table[1,1]*log(pi.bar/pi.hat.table[1,1]) + c.table[1,2]*log((1-pi.bar)/(1-pi.hat.table[1,1])) +
       c.table[2,1]*log(pi.bar/pi.hat.table[2,1]) + c.table[2,2]*log((1-pi.bar)/(1-pi.hat.table[2,1]))
  test.stat<--2*log.Lambda
  crit.val<-qchisq(p = 0.95, df = 1)
  p.val<-1-pchisq(q = test.stat, df = 1)
  round(data.frame(pi.bar, test.stat, crit.val, p.val, row.names = NULL), 4)
  
  library(package = vcd)
  assocstats(x = c.table)


  # Other ways to do the hyptothes test
  chisq.test(x = c.table, correct = FALSE)
  summary(bird.table2)
  summary(as.table(c.table))

  class(bird.table2)
  class(as.table(c.table))
  summary.table(bird.table2)


####################################################
# Relative risk

  cat("The sample relative risk is", round(pi.hat1/pi.hat2, 4), "\n \n")

  alpha<-0.05
  n1<-sum(c.table[1,])
  n2<-sum(c.table[2,])

  # Wald confidence interval
  ci<-exp(log(pi.hat1/pi.hat2) + qnorm(p = c(alpha/2, 1-alpha/2)) *
     sqrt((1-pi.hat1)/(n1*pi.hat1) + (1-pi.hat2)/(n2*pi.hat2)))
  round(ci, 4)
  rev(round(1/ci, 4))  # inverted

  # Change contingency table so that we are looking at the "missed" column
  (1-pi.hat1)/(1-pi.hat2)
   exp(log((1-pi.hat1)/(1-pi.hat2)) + qnorm(p = c(alpha/2, 1-alpha/2)) *
     sqrt((pi.hat1)/(n1*(1-pi.hat1)) + (pi.hat2)/(n2*(1-pi.hat2))))





####################################################
# OR

  OR.hat<-c.table[1,1]*c.table[2,2] / (c.table[2,1]*c.table[1,2])
  round(OR.hat, 2)
  round(1/OR.hat, 2)

  alpha<-0.05
  var.log.or<-1/c.table[1,1] + 1/c.table[1,2] + 1/c.table[2,1] + 1/c.table[2,2]
  OR.CI<-exp(log(OR.hat) + qnorm(p = c(alpha/2, 1-alpha/2)) *
        sqrt(var.log.or))
  round(OR.CI, 2)
  rev(round(1/OR.CI, 2))


  # Another way to get the OR
  #  Note that the function below automatically adds 0.5 to each cell for variance 
  #  but not for the odds ratio itself unless there are 0 counts (see code in function).
  library(vcd)  # Visualizing categorical data package
  save.OR<-oddsratio(x = c.table, log = TRUE)
  attributes(save.OR)  # names( ) does not work
  summary(save.OR) 
  confint(save.OR, level = 0.95)
  OR.tilde<-(c.table[1,1]+0.5)*(c.table[2,2]+0.5)/((c.table[1,2]+0.5)*(c.table[2,1]+0.5))
  log(OR.tilde)  # Does not match vcd's log(OR^)
  sqrt(1/(c.table[1,1]+0.5) + 1/(c.table[2,2]+0.5) + 1/(c.table[1,2]+0.5) + 1/(c.table[2,1]+0.5))  # Matches vcd's standard error (sqrt(var^(log(OR^)))










  #

