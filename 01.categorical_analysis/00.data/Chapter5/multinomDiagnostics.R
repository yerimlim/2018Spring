#######################################################################
# NAME: Tom Loughin                                                   #
# DATE: 1-10-13                                                       #
# PURPOSE: Diagnostics for nominal multinomial model using multinom() #
#                                                                     #
# NOTES: ASSUMES THAT multinom() HAS BEEN FIT TO BINARY-FORM DATA     #
#     Assumes that arguments Hess = TRUE and model = TRUE have been   #
#      added to multinom()                                            #
#     (Need binaries and model = TRUE to extract response binaries    #
#      for calculations)                                              #
#######################################################################

#
# First compute the variance-covariance matrix for beta-hat manually. 
# This can help to identify when there are problems in the data that are 
# not immediately apparent from the fit of the model (e.g. zero cell counts). 
# Also identifies when scaling the explanatories might be useful. 
# In particular, if the matrix produced by vcov() on the multinom-class 
#  object does not match the one we compute, then there is a problem and 
#  the analysis should be rerun with either scaled explanatories or 
#  adjusted binaries. 

multinomDiag <- function(mod.fit){ 
 # Going to compute X'VX
 # Use generics to find residuals and estimated probabilities
 res <- as.data.frame(residuals(mod.fit))
 pi.hat <- as.data.frame(predict(mod.fit, type = "prob"))
 # Rearrange categorical response into J binary columns.
 levs <- colnames(res)
 counts <- mod.fit$weights
 # Rearrange X in the right order to match vcov from multinom()
 J <- length(levs)
 n <- nrow(res)
 indices <- c(t(matrix(data = 1:(n*(J-1)), nrow = n, ncol = J-1, byrow = FALSE)))
 X <- diag(J-1) %x% model.matrix(mod.fit)  # %x% is Kronecker Product
 X <- X[indices,] 
 # Now get V = Var-hat(Y)
 # Remember that first column of pi.hat is reference category
 V <- matrix(data = 0, nrow = n*(J-1), ncol = n*(J-1))
 # Need a different form of Pearson Residual for later
 pear.res1 <- matrix(data = NA, nrow = n, ncol = J-1)
 for(ii in c(1:n)) {
  p.i <- as.numeric(pi.hat[ii,-1])
  V.i <- (diag(p.i) - p.i%*%t(p.i))*counts[ii]
  index <- (J - 1)*(ii - 1) + 1
  V[c(index:(index+(J-2))),c(index:(index+(J-2)))] <- V.i
  vinv <- solve(V.i)
  e <- eigen(vinv)
  ev <- e$vectors
  B <- ev %*% diag(sqrt(e$values)) %*% t(ev)  # = V^{-1/2}
  
  pear.res1[ii,] <- counts[ii]*as.numeric(res[ii,-1]) %*% B
 }
 # Get Var(beta-hat) = (X' V X)^{-1}
 sigma <- solve(t(X) %*% V %*% X)  # Could just take vcov(mod.fit)

 # Form hat matrix: V^{1/2} X (X' V X)^{-1} X' V^{1/2}
 # Take square root of V using eigen-decomposition 
 e <- eigen(V)
 ev <- e$vectors
 B <- ev %*% diag(sqrt(e$values)) %*% t(ev)  # = V^{1/2}: B %*% B = V
 
 H <- B %*% X %*% sigma %*% t(X) %*% B  # Hat Matrix
 
 ##############################################################################
 # Standard diagnostics:
 # 1. Deviance/DF
 # 2. Pearson residuals and Pearson values for each observation

 # Deviance/DF
 cat("Deviance = ", mod.fit$deviance, "df = ", (n - mod.fit$edf),"\n")
 cat("Deviance/df = ", mod.fit$deviance/(n - mod.fit$edf),"\n")
 cat("Threshold = 1 + 3*sqrt(2/(n- mod.fit$edf))", 1 + 3*sqrt(2/(n- mod.fit$edf)),"\n") 

 # Create Pearson residuals and Pearson goodness-of-fit value for observation
 
 response <- mod.fit$model[,1]
 if(class(response) == "matrix") binary = response else{ 
  binary <- matrix(data = NA, nrow = length(response), ncol = J)
  colnames(binary) <- levs
  for(jj in c(1:J)){
   binary[,jj] = as.numeric(response == levs[jj])
  }
 }
 pear.res <- (binary - counts*pi.hat)/sqrt(counts*pi.hat)
 pear.obs <- apply(X = pear.res^2, MARGIN = 1, FUN = sum)
 X2 <- sum(pear.obs)

 # Plot results of Pearson statistics
 x11()
 plot(x = c(1:length(pear.obs)), y = pear.obs, xlab = "Observation number", ylab = "Pearson Statistic",
    main = "Index Plot of Pearson Values")
 abline(h = qchisq(p = 0.95, df = J-1), lty = "dotted")
 abline(h = qchisq(p = 0.99, df = J-1), lty = "dotted")

 dev.res2 <- -2*apply(X = binary*log(pi.hat), MARGIN = 1, FUN = sum)

 # Plot results of Deviance statistics
 x11()
 plot(x = c(1:length(dev.res2)), y = dev.res2, xlab = "Observation number", ylab = "Deviance Statistic",
    main = "Index Plot of Deviance Values")
 abline(h = qchisq(p = 0.95, df = J-1), lty = "dotted")
 abline(h = qchisq(p = 0.99, df = J-1), lty = "dotted")

 Xmat <- as.matrix(mod.fit$model[,-1])
 colnames(Xmat) <- colnames(mod.fit$model[,-1]) 

 # Combine results into data frame so that we can interpret large Pearson values
 resid.stats <- data.frame(Xmat, binary, pi.hat = round(pi.hat,3), p.res = round(pear.res,2), 
            pear.val = round(pear.obs,2), dev.val = round(dev.res2, 2))

 # Print observations with large Pearson or Deviance values
 print("Observations with Pearson values above the Chi-Square(.95) threshold")
 print(resid.stats[(resid.stats$pear.val > qchisq(p = 0.95,df = J-1)),])
 print("Observations with Deviance values above the Chi-Square(.95) threshold")
 print(resid.stats[(resid.stats$dev.val > qchisq(p = 0.95,df = J-1)),])

 ############################################################################
 # Influence Statistics
 # See Lesaffre and Albert (1989) for details.
 # 
 # Hat matrix is H 
 # Evidence that Hat matrix diagonal sums to (p+1)(J-1) 
 # sum(diag(H))

 # Prepare to calculate some building blocks for influence diagnostics
 # Use Hii as (J-1)x(J-1) diagonal block elements of H for obs i
 detM <- vector(length = n)  # Will hold determinant of Mii = 1 - Hii
 cookD <- vector(length = n)  # Will hold approximate Cook's D values.
 hii <- vector(length = n)  # Will hold leverage values for each obs
 deltaDev <- vector(length = n)  # Will hold leverage values for each obs
 deltaX2 <- vector(length = n)  # Will hold leverage values for each obs
 for(ii in (1:n)){
  start <- (J-1)*(ii-1) + 1  # Index to guide extraction of block from diagonal of H
  Hii <- matrix(data = H[c(start:(start+J-2)),c(start:(start+J-2))], nrow = J-1)
  # leverage value is sum of all diagonal elements of H for that observation
  hii[ii] <- sum(diag(Hii)) 
  Mii <- diag(J-1) - Hii
  # Determinant of Mii is approximately the CovRatio (Lesaffre and Albert 1989).
  # They give a threshold of < 1-2DF/n for leverage, 
  # DF = model DF = # parameters in model
  # detM[ii] <- det(x = Mii) 
  Minv <- solve(Mii)
  # 1-step approximation to Cook's D as per Lesaffre and Albert (1989). 
  # They give threshold of chi-square(DF), but values are typically < 2, so this seems unrealistic
  cookD[ii] <- (t(pear.res1[ii,]) %*% Minv %*% Hii %*% Minv %*% pear.res1[ii,]/mod.fit$edf) 
  # Approximate Deviance test for outlier as per Lesaffre and Albert (1989)
  # Compare to critical values of chi-square(J-1) 
  deltaDev[ii] <- dev.res2[ii] + t(pear.res1[ii,]) %*% Minv %*% Hii %*% pear.res1[ii,]
  deltaX2[ii] <- t(pear.res1[ii,]) %*% Minv %*% pear.res1[ii,]
 }

 # Add influence neasures to PostFit stats
 infl.stats <- data.frame(Xmat, binary, pi.hat = round(pi.hat,3), detM = round(detM,3), CookD = round(cookD,2), 
            deltaDev = round(deltaDev, 2), hat = round(hii,2))
 # Print observations with extreme invluence statistics
 print("Observations with large leverage values")
 print(infl.stats[(hii > 3*mod.fit$edf/n),])
 print("Observations with large Delta Pearson")
 print(infl.stats[(deltaX2 > qchisq(p = .95, df = J-1)),])
 print("Observations with large DeltaDeviance (outliers)")
 print(infl.stats[(deltaDev > qchisq(p = .95, df = J-1)),])
 print("Observations with large Cook's D")
 print(infl.stats[(cookD > 4/n),])

 # plot(infl.stats$detM, hii): Looks very nearly 1:1. Either measure will do for leverage.
 # Plots of case-deletion stats vs. leverage
 x11()
 plot(x = hii, y = cookD, main = "Cook's distance against approximate leverage", 
    xlab = "Hat value (approx. leverage)", ylab = "Cook's Distance")
 abline(v = c(2,3)*mod.fit$edf/n, lty = "dotted")

 x11()
 plot(x = hii, y = deltaDev, main = "Change in Deviance from deletion against approximate leverage", 
    xlab = "Hat value (approx. leverage)", ylab = "Delta Deviance")
 abline(v = c(2,3)*mod.fit$edf/n, lty = "dotted")
 abline(h = qchisq(p = 0.95, df = J-1), lty = "dotted")
 abline(h = qchisq(p = 0.99, df = J-1), lty = "dotted")
 
 x11()
 plot(x = hii, y = deltaX2, main = "Change in Pearson from deletion against approximate leverage", 
    xlab = "Hat value (approx. leverage)", ylab = "Change in Pearson")
 abline(v = c(2,3)*mod.fit$edf/n, lty = "dotted")
 abline(h = qchisq(p = 0.95, df = J-1), lty = "dotted")
 abline(h = qchisq(p = 0.99, df = J-1), lty = "dotted")
# Not covered in book 
# # Pregibon Plot
# x11()
# plot(x = hii, y = pear.obs/X2, main = "Fractional Pearson value against Approximate Leverage",
#    xlab = "Approximate leverage", ylab = "Fractional contribution to Pearson Statistic")
# abline(a = 2*(1+mod.fit$edf)/n, b = -1, lty = "dotted")
# abline(a = 3*(1+mod.fit$edf)/n, b = -1, lty = "dotted")
}
