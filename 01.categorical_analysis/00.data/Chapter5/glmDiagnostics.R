#####################################################################
# NAME: Tom Loughin and Chris Bilder                                #
# DATE: 09-22-2013                                                  #
# PURPOSE: Model diagnostics for a glm object                       #
#                                                                   #
# NOTES:                                                            #
#####################################################################

glmInflDiag <- function(mod.fit, print.output = TRUE, which.plots = c(1,2)){

 # Which set of plots to show
 show <- rep(FALSE, 2)  # Idea from plot.lm()
 show[which.plots] <- TRUE

 # Main quantities: Pearson and deviance residual, model Pearson and deviance stats
 pear <- residuals(mod.fit, type = "pearson")
 dres <- residuals(mod.fit, type = "deviance")
 x2 <- sum(pear^2)
 N <- length(pear)
 P <- length(coef(mod.fit))
 # Hat values (leverages)
 hii <- hatvalues(mod.fit)
 # Computed quantities: Standardized Pearson residual, Delta-beta, Delta-deviance
 sres <- pear/sqrt(1-hii)
# D.beta <- (pear^2*hii/(1-hii)^2)
# cookD <- D.beta / (P * summary(mod.fit)$dispersion)
 cookD <- pear^2 * hii / ((1-hii)^2 * (P) * summary(mod.fit)$dispersion) 
 D.dev2 <- dres^2 + hii*sres^2
 D.X2 <- sres^2

 yhat <- fitted(mod.fit)

 # Plots against fitted values 
 if(show[1] == TRUE) {
  x11(height = 7,width = 15, pointsize = 15)
  par(mfrow = c(1,4), lty = "dotted")
  plot(x = yhat, y = hii, xlab = "Estimated Mean or Probability", ylab = "Hat (leverage) value",
   ylim = c(0, max(hii,3*P/N)))
  abline(h = c(2*P/N,3*P/N))

  plot(x = yhat, y = D.X2, xlab = "Estimated Mean or Probability", ylab = "Approx change in Pearson stat",
   ylim = c(0, max(D.X2,9)))
  abline(h = c(4,9), lty = "dotted")

  plot(x = yhat, y = D.dev2, xlab = "Estimated Mean or Probability", ylab = "Approx change in deviance",
   ylim = c(0, max(D.dev2,9)))
  abline(h = c(4,9), lty = "dotted")

  plot(x = yhat, y = cookD, xlab = "Estimated Mean or Probability", ylab = "Approx Cook's Distance",
   ylim = c(0, max(cookD, 1)))
  abline(h = c(4/N,1), lty = "dotted")
 }
 
 # Plots against hat values
 if(show[2] == TRUE) {
  x11(height = 6, width = 12, pointsize = 20)
  par(mfrow = c(1,3))
  plot(x = hii, y = D.X2, xlab = "Hat (leverage) value", ylab = "Approx change in Pearson stat",
   ylim = c(0, max(D.X2, 9)), xlim = c(0, max(hii,3*P/N)))
  abline(h = c(4,9), lty = "dotted")
  abline(v = c(2*P/N,3*P/N), lty = "dotted")

  plot(x = hii, y = D.dev2, xlab = "Hat (leverage) value", ylab = "Approx change in deviance",
   ylim = c(0, max(D.dev2,9)), xlim = c(0, max(hii,3*P/N)))
  abline(h = c(4,9), lty = "dotted")
  abline(v = c(2*P/N,3*P/N), lty = "dotted")

  plot(x = hii, y = cookD, xlab = "Hat (leverage) value", ylab = "Approx Cook's Distance",
   ylim = c(0, max(cookD, 1)), xlim = c(0, max(hii,3*P/N)))
  abline(h = c(4/N,1), lty = "dotted")
  abline(v = c(2*P/N,3*P/N), lty = "dotted")
 }

 # Listing of values to check
 # Create flags to identify high values in listing
 hflag <- ifelse(test = hii > 3*P/N, yes = "**", no = 
          ifelse(test = hii > 2*P/N, yes = "*", no = ""))
 xflag <- ifelse(test = D.X2 > 9, yes = "**", no = 
          ifelse(test = D.X2 > 4, yes = "*", no = ""))
 dflag <- ifelse(test = D.dev2 > 9, yes = "**", no = 
          ifelse(test = D.dev2 > 4, yes = "*", no = ""))
 cflag <- ifelse(test = cookD > 1, yes = "**", no = 
          ifelse(test = cookD > 4/N, yes = "*", no = ""))

 chk.hii2 <- which(hii > 3*P/N)
 chk.DX22 <- which(D.X2 > 9 | (D.X2 > 4 & hii > 2*P/N))
 chk.Ddev2 <- which(D.dev2 > 9 | (D.dev2 > 4 & hii > 2*P/N))
 chk.cook2 <- which(cookD > 4/N)

 all.meas <- data.frame(h = round(hii,2), hflag, Del.X2 = round(D.X2,2), xflag,
             Del.dev = round(D.dev2,2), dflag, Cooks.D = round(cookD,3), cflag)

 if(print.output == TRUE) {
  cat("Potentially influential observations by any measures","\n")
  print(all.meas[sort(unique(c(chk.hii2, chk.DX22, chk.Ddev2, chk.cook2))),])
  cat("\n","Data for potentially influential observations","\n")
  print(cbind(mod.fit$data, yhat = round(yhat, 3))[sort(unique(c(chk.hii2, chk.DX22, chk.Ddev2, chk.cook2))),])
 }

 data.frame(hat  =  hii, CD = cookD, delta.Xsq = D.X2, delta.D = D.dev2)
}