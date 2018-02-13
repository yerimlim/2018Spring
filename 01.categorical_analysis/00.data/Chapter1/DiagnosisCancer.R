###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  3-24-10                                                          #
# PURPOSE: C.I.s for dependent proportions                                #
# NOTES:                                                                  #
###########################################################################


# Create contingency table
c.table<-array(data = c(4, 3, 6, 3), dim = c(2,2), dimnames = list(MRI = c("Localized", "Advanced"),
             Ultrasound = c("Localized", "Advanced")))
c.table  # Whole table
n<-sum(c.table)
pi.hat.table<-c.table/sum(c.table)
 
# Point estimate
pi.hat.plus1<-sum(c.table[,1])/n
pi.hat.1plus<-sum(c.table[1,])/n
data.frame(pi.hat.plus1, pi.hat.1plus, diff = pi.hat.plus1 - pi.hat.1plus)


###########################################################################
# C.I. for p__+1 - p_1+
 
  # Wald using function from PropCIs package
  library(package = PropCIs)
  diffpropci.Wald.mp(b = c.table[1,2], c = c.table[2,1], n = sum(c.table), conf.level = 0.95)

  # Wald interval using formulas
  pi.hat.table<-c.table/n
  alpha<-0.05
  var.hat<-1/n * ( pi.hat.plus1*(1-pi.hat.plus1) +  pi.hat.1plus*(1-pi.hat.1plus) - 2*(pi.hat.table[1,1]*pi.hat.table[2,2] - pi.hat.table[1,2]*pi.hat.table[2,1]) )
  round(pi.hat.plus1 - pi.hat.1plus + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.hat), 4)


  # Agresti and Min interval
  diffpropci.mp(b = c.table[1,2], c = c.table[2,1], n = sum(c.table), conf.level = 0.95)
  
  ###############################################
  # Show how to do calculation w/o diffpropci.mp()
  
    # Save actual data
    c.table.actual<-c.table
    n.actual<-n
    pi.hat.plus1.actual<-pi.hat.plus1
    pi.hat.1plus.actual<-pi.hat.1plus
    pi.table.actual<-pi.hat.table
    
    # Make adjustment
    c.table<-c.table+0.5
    
    # New calculations
    n<-sum(c.table)
    pi.hat.plus1<-sum(c.table[,1])/n
    pi.hat.1plus<-sum(c.table[1,])/n
    pi.hat.table<-c.table/sum(c.table)
    var.hat<-1/sum(c.table) * ( pi.hat.plus1*(1-pi.hat.plus1) +  pi.hat.1plus*(1-pi.hat.1plus) - 2*(pi.hat.table[1,1]*pi.hat.table[2,2] - pi.hat.table[1,2]*pi.hat.table[2,1]) )
    round(sum(c.table[,1])/n  - sum(c.table[1,])/n + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.hat), 4)
  
    # Put actual data back in correct objects
    n<-n.actual 
    c.table<-c.table.actual
    pi.table<-pi.table.actual
    pi.hat.plus1<-pi.hat.plus1.actual
    pi.hat.1plus<-pi.hat.1plus.actual
 

  # Tango
  scoreci.mp(b = c.table[1,2], c = c.table[2,1], n = sum(c.table), conf.level = 0.95)



###########################################################################
# McNemar's test

  mcnemar.test(x = c.table, correct = FALSE)

  # Test statistic
  M<-(c.table[2,1] - c.table[1,2])^2 / (c.table[2,1] + c.table[1,2])
  M 
  # p-value
  1 - pchisq(q = M, df = 1)

  # Statistic with continuity correction
  mcnemar.test(x = c.table, correct = TRUE)
  (abs(c.table[2,1] - c.table[1,2]) - 1)^2 / (c.table[2,1] + c.table[1,2])







#