#############################################################################
# NAME: Tom Loughin                                                         #
# DATE: 1-10-13                                                             #
# PURPOSE: Accessory functions to fit zeroinfl() models in glmulti()        #
#                                                                           #
# NOTES: Based on similar programs from Vincent Calcagno                    #
#############################################################################

# Unfortunately, have to deactivate a system variable in order to get rJava to work. 
# See http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
# True for 32 bit as well
if (Sys.getenv("JAVA_HOME")!="")
 Sys.setenv(JAVA_HOME = "")

library(glmulti)

library(pscl)


# Need to call zeroinfl from glmulti.
# Formula for zeroinfl has two parts: one for mu and one for pi.
# Have to be able to recreate the formula from two separate parts.
# This function defaults to assuming that that variable selection is to take place 
#  both in the mean model and in the probability model. The same variables are 
#  either in both models or excluded from both models. Unfortunately,
#  the way glmulti() is structured, it appears that it would be very difficult
#  to do separate variable selection on the mean and the probability models.
#
# Usage:
# If variable selection is desired in both mean and probability, then use the 
#  formula = ... specificaiton to list the variables to be considered in both models. 
#  Do not include an inflate = ... argument.
# If the probability model is to be held fixed, then list the variables for the mean 
#  model in the formula = ... and list the variables for the probability model in inflate = "...".
#  For the inflate = parameter, use format "x1 + x2" and enclose the terms in quotes. 
#  If no variables are to be used, list "1".
# 
# Needed to add "width.cutoff = 500" or the formula may be put together wrong and cause errors.
# Also recemmended to use short variable names when there are many variables.
zeroinfl.glmulti = function(formula, data, inflate = NULL, ...) {
 if (is.null(inflate)) zeroinfl(as.formula(paste(deparse(formula, width.cutoff = 500))),data = data,...)
 else zeroinfl(as.formula(paste(deparse(formula, width.cutoff = 500), "|" ,inflate)),data = data,...)
} 

# There is no nobs() method for zeroinfl, so one is created here.
nobs.zeroinfl <- function(obj){obj$n}

# Next need to create a version of the "getfit" internal function for glmulti() that can
#  read zeroinfl-class objects and return the parameter estimates, standard errors, and residual DF
#
# Because glmulti() is an S4 function and zeroinfl() is S3, need to "register" zeroinfl 
#  for use in S4 computing 

setOldClass("zeroinfl")

# Now creating the getfit internal function for zeroinfl.
# Started with the generic getfir, obtained using getMethod("getfit")
# Needed to add "zeroinfl" to signature = argument, and separately process Mean and Probability model parameters.

setMethod("getfit",signature = "zeroinfl", 
     function (object, ...) 
     {
      summ = summary(object)
      summ1.c = summ$coefficients$count
      didi.c = dimnames(summ1.c)
      if (is.null(didi.c[[1]])) {
       summ1.c = matrix(rep(0, 2), nrow = 1, nc = 2, dimnames = list(c("NULLOS"), 
                                      list("Estimate", "Std. Error")))
       return(cbind(summ1.c, data.frame(df.c = c(0))))
      }
      summ1.z = summ$coefficients$zero
      didi.z = dimnames(summ1.z)
      if (is.null(didi.z[[1]])) {
       summ1.z = matrix(rep(0, 2), nrow = 1, nc = 2, dimnames = list(c("NULLOS"), 
                                      list("Estimate", "Std. Error")))
       return(cbind(summ1.z, data.frame(df.z = c(0))))
      }
      
      summ1 = rbind(summ1.c[, 1:2],summ1.z[,1:2])
      didi = dimnames(rbind(summ1.c, summ1.z))
      rdimn <- rbind(cbind(paste("mu-",didi.c[[1]],sep = "")),cbind(paste("pi-",didi.z[[1]],sep = "")))
      dimnames(summ1) = list(rdimn, didi[[2]][1:2])
      rr <- cbind(summ1, data.frame(df = rep(summ$df.residual, nrow(summ1))))
      return(rr)
     }
)
