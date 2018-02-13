#############################################################################
# NAME: Tom Loughin                                                         #
# DATE: 1-10-13                                                             #
# PURPOSE: Accessory functions to fit plor() models in glmulti()            #
#                                                                           #
# NOTES: Based on similar programs from Vincent Calcagno                    #
#############################################################################

library(MASS)
# Unfortunately, have to deactivate a system variable in order to get rJava to work. 
# See http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
# True for 32 bit as well
if (Sys.getenv("JAVA_HOME")!= "")
 Sys.setenv(JAVA_HOME = "")
library(glmulti)

# First need to create a polr()-based function that can be called as the 
#  fitfunction =  in glmulti()

polr.glmulti <- function(formula, data){
 #Need Hess = TRUE to get hessian stored for use later in estimating 
 # standard errors for parameter estimates
 polr(as.formula(paste(deparse(formula))), data = data, Hess = TRUE)
}

# Next need to create a version of the "getfit" internal function for glmulti() that can
#  read polr-class objects and return the parameter estimates, standard errors, and residual DF
#
# Because glmulti() is an S4 function and polr() is S3, need to "register" polr 
#  for use in S4 computing 
setOldClass("polr")

# Now creating the getfit internal function for polr.
# Started with the generic getfir, obtained using getMethod("getfit")
# Needed to add "polr" to signature = argument, and tell it where to find the DF
setMethod("getfit",signature = "polr", 
     function (object, ...) 
     {
      summ = summary(object)
      summ1 = summ$coefficients
      didi = dimnames(summ1)
      if (is.null(didi[[1]])) {
       summ1 = matrix(rep(0, 2), nrow = 1, nc = 2, dimnames = list(c("NULLOS"), 
                                     list("Estimate", "Std. Error")))
       return(cbind(summ1, data.frame(df = c(0))))
      }
      summ1 = summ1[, 1:2]
      if (length(dim(summ1)) == 0) {
       didi = dimnames(summ$coefficients)
       summ1 = matrix(summ1, nrow = 1, nc = 2, dimnames = list(didi[[1]], 
                                   didi[[2]][1:2]))
      }
      # Need to fix location of residual df: 
      return(cbind(summ1, data.frame(df = rep(summ$df.residual, length(summ$coefficients[,1])))))
     }
)
