######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  8-20-10                                                     #
# UPDATE:                                                            #
# Purpose: R Appendix examples before the regression example         #
#                                                                    #
# NOTES:                                                             #
#                                                                    #
######################################################################


######################################################################
# Section 1

  2+2
  pnorm(1.96)
  (2-3)/6
  2^2
  sin(pi/2)
  log(1)

  save<-2+2
  save
  
  ls()
  objects()
  

######################################################################
# Section 2

  x<-c(1,2,3,4,5)
  sd2<-function(numbers) {
    sqrt(var(numbers))
  }
  sd2(x)
  
  sd2<-function(numbers) {
    cat("Print the data \n", numbers, "\n")
    sqrt(
    var(numbers)) 
  } 
  save<-sd2(x) 
  save 

  # Another example of calculating the standard deviation without the var() function
  sd3<-function(numbers) {
    sqrt(sum((numbers-mean(numbers))^2)/(length(numbers)-1))
  }
  sd3(x)

  # Another exmaple of calculating the standrd deviation where all of the function's code is on one line.
  #  The semicolon is used to separate cat() and sqrt() function calls. This symbol is used
  #  to signal the end of a complete line of code (rarely is there a need for it).
  sd4<-function(numbers) { cat("Print the data \n", numbers, "\n");  sqrt(var(numbers))  } 
  sd4(x)


######################################################################
# Section 3

  pnorm(1.96) 
  pnorm(q = 1.96) 
  pnorm(1.96, 0, 1) 
  pnorm(q = 1.96, mean = 0, sd = 1)  


######################################################################
# Section 4

  pnorm(q = c(-1.96,1.96)) 
  qt(p = c(0.025, 0.975), df = 9) 

  x<-c(3.68, -3.63, 0.80, 3.03, -9.86, -8.66, -2.38, 8.94, 0.52, 1.25) 
  x 
  var.xbar<-var(x)/length(x)
  mean(x) + qt(p = c(0.025, 0.975), df = length(x)-1) * sqrt(var.xbar)
  t.test(x = x, mu = 2, conf.level = 0.95)































#
