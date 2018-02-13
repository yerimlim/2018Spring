#############################################################################
# NAME: Tom Loughin                                                         #
# DATE: 1-10-13                                                             #
# PURPOSE: Accessory functions to fit multinom() models in glmulti()        #
#                                                                           #
# NOTES: Based on similar programs from Vincent Calcagno                    #
#############################################################################

library(nnet)
# Unfortunately, have to deactivate a system variable in order to get rJava to work. 
# See http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
# True for 32 bit as well
if (Sys.getenv("JAVA_HOME")!= "")
 Sys.setenv(JAVA_HOME = "")
library(glmulti)

# First need to create a multinom()-based function that can be called as the 
#  fitfunction =  in glmulti()

multinom.glmulti <- function(formula, data){
 multinom(as.formula(paste(deparse(formula))), data = data, Hess = TRUE)
}

# Need a method function for computing sample size; multinom() does not have one built in.
nobs.multinom <- function(obj){nobs(logLik(obj))}

# Next need to create a version of the "getfit" internal function for glmulti() that can
#  read multinom-class objects and return the parameter estimates, standard errors, 
#  and residual DF
#
# Because glmulti() is an S4 function and multinom() is S3, need to "register" multinom 
#  for use in S4 computing 
setOldClass("multinom")

# Now creating the getfit internal function for multinom.
# Started with the generic getfir, obtained using getMethod("getfit")
# Needed to add "multinom" to signature = argument, and tell it where to find the DF
setMethod("getfit",signature = "multinom", 
     function (object, ...) 
     {
      summ = summary(object)
      sumc = as.vector(summ$coefficients)
      sums = as.vector(summ$standard.errors)
      namen = c()
      q = 1
      for(i in c(1:length(object$coefnames))){
       for (j in c(1:(length(object$lab)-1))){
        namen[q] <- paste(object$lab[j+1], "-", object$coefnames[i])
        q = q+1
       }
      }
      summ1 = as.data.frame(cbind(Estimate = sumc,Std.Error = sums))
      row.names(summ1) <- namen
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
      return(cbind(summ1, data.frame(df = rep(nobs(object) - summ$edf, length(summ$coefficients[,1])))))
     }
)
