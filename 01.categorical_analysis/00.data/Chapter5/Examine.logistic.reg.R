################################################################################
# NAME: Chris Bilder                                                           #
# DATE: 7-30-13                                                                #
# UPDATE:                                                                      #
# Purpose: Automate part of the logistic regression diagnostic                 #
#     procedures into one function.                                            #
#                                                                              #
# NOTES:                                                                       #
# 1) This function needs to be run just once before it is used. Below is an    #
#   example of how to use the function the first time:                         #
#                                                                              #
#   source(file = "C:\\Chris\\examine.logistic.model.R")                       #
#   mod.fit1 <- glm(formula = y/n ~ x, data = EVP.form, weight = n,            #
#    family = binomial(link = logit))                                          #
#   examine.logistic.reg(mod.fit1)                                             #
#   mod.fit2 <- glm(formula = y/n ~ x + I(x^2), data = EVP.form, weight = n,   #
#    family = binomial(link = logit))                                          #
#   examine.logistic.reg(mod.fit2)                                             # 
#                                                                              #
# 2) Arguments:                                                                # 
#   mod.fit.obj = model fit object from glm()                                  #
#   identify.points = identify points on the plot by mouse clicks              #
#   bubble = produce plots where plotting point is proportional in size        #
#    to a third dimension (number of trials or Cook's D)                       #
#   scale.n, scale.cookd = scaling to use with bubble size; the default is     #
#    the original numerical scale of the quantity; it can be helpful to use    #
#    a transformation, like sqrt, when there is a large difference in          #
#    numerical values.                                                         #
#   pearson.dev = specifies whether "Pearson" or "deviance" based residuals    #
#    should be given in the plots                                              #
################################################################################

examine.logistic.reg <- function(mod.fit.obj = mod.fit, identify.points = TRUE, bubble = TRUE, scale.n = I,
 scale.cookd = I, pearson.dev = "Pearson"){

 pearson <- residuals(mod.fit.obj, type = "pearson")  #Pearson residuals
 stand.resid <- rstandard(model = mod.fit.obj, type = "pearson")  # Standardized Pearson residuals
 deltaXsq <- stand.resid^2
 pred <- mod.fit.obj$fitted.values
 n <- mod.fit.obj$prior.weights   # Number of observations per EVP
 df <- mod.fit.obj$df.residual
 cookd <- cooks.distance(mod.fit.obj)
 h <- hatvalues(mod.fit.obj)
 dev.res <- residuals(mod.fit.obj, type = "deviance")
 stand.dev.resid <- rstandard(model = mod.fit.obj, type = "deviance")  # Standardized deviance residuals
 deltaD <- dev.res^2 + h*stand.resid^2
 pear.stat <- sum(pearson^2)
 dev <- mod.fit.obj$deviance
 p <- length(mod.fit.obj$coefficients)

 # Type of residuals to include on plots
 resid.plot11 <- stand.resid
 resid.plot21 <- deltaXsq
 plot.label11 <- "Pearson"
 plot.label21 <- "Delta X^2"
 if (pearson.dev == "deviance") {
  resid.plot11 <- stand.dev.resid
  resid.plot21 <- deltaD
  plot.label11 <- "deviance"
  plot.label21 <- "Delta D"
 }


 ##############################################################################
 # Four plots

  # Open a new plotting window
  x11(width = 8, height = 6, pointsize = 12)
  # Divide the plot into three rows and two columns. The last row is only 1cm in height to make sure
  #  there is some room for the printed GOF statistics
  layout(mat = matrix(c(1,2,3,4,5,5), byrow = TRUE, ncol = 2), height = c(1,1,lcm(1)))
  # layout.show(5)


  # Standardized residual vs predicted prob.
  plot(x = pred, y = resid.plot11, xlab = "Estimated probabilities", ylab = "Standardized residuals",
   main = paste("Standardized", plot.label11, "residuals vs. est. prob."), ylim = c(min(-3, stand.resid), max(3, stand.resid)))
  abline(h = c(-3, -2, 0, 2, 3), lty = "dotted", col = "blue")
  if(identify.points == TRUE) {
   # labels(pred) uses the row names from the original data set
   #  This can be helpful, rather than the default of 1:n in identify(),
   #  when observations have been removed from the data set (i.e., the same row names will be used
   #  as with the original data set)
   identify(x = pred, y = resid.plot11, labels = labels(pred))
  }

  order.pred <- order(pred)
  smooth.stand <- loess(formula = resid.plot11 ~ pred, weights = n)
  lines(x = pred[order.pred], y = predict(smooth.stand)[order.pred], lty = "solid", col = "red")
  # The ordering of pred leads to one line drawn across the plot. Otherwise, multiple lines will be drawn
  #  between each pred and predict() pair, which may cause a zig-zag-like pattern of lines

  # Very similar way to get the loess model plotted
  # smooth.stand <- loess(formula = resid.plot11 ~ pred, weights = n)
  # x.axis <- seq(from = min(pred), to = max(pred), by = (max(pred) - min(pred))/100)
  # pred.data<-predict(object = smooth.stand, newdata = data.frame(pred = x.axis))
  # lines(x = x.axis, y = pred.data, lty = "solid", col = "red")


  # Cook's distance vs. leverage
  plot(x = h, y = cookd, ylim = c(0, max(4/length(cookd), cookd)), xlim = c(0, max(3*p/length(h), h)),
   xlab = "Leverage (hat matrix diagonal)", ylab = "Cook's distance", main = "Cook's distance vs. leverage")
  abline(h = c(4/length(cookd), 1), lty = "dotted")
  abline(v = c(2*p/length(h), 3*p/length(h)), lty = "dotted")
  if(identify.points == TRUE) {
   identify(x = h, y = cookd, labels = labels(h))
  }


  # Delta X^2 or D vs. predicted prob. with plotting point proportional to n
  if(bubble == TRUE) {
   symbols(x = pred, y = resid.plot21, xlab = "Estimated probabilities", circles = scale.n(n),
    ylab = plot.label21,
    main = paste(plot.label21, "vs. est. prob. \n with plot point proportional to number of trials"),
    inches = 0.1, ylim = c(0, max(9, resid.plot21)))
   abline(h = c(4, 9), lty = "dotted", col = "blue")
   if(identify.points == TRUE) {
    identify(x = pred, y = resid.plot21, labels = labels(pred))
   } }
  else {
   plot(x = pred, y = resid.plot21, xlab = "Estimated probabilities", ylab = plot.label21,
    main = paste(plot.label21, "vs. est. prob."), ylim = c(0, max(9, resid.plot21)))
   abline(h = c(4, 9), lty = "dotted", col = "blue")
   if(identify.points == TRUE) {
    identify(x = pred, y = resid.plot21)
  } }

 
  # Print deviance/df on plot
  dev.df <- dev/df
  gof.threshold <- round(c(1 + 2*sqrt(2/df), 1 + 3*sqrt(2/df)), 2)
  mtext(text = paste("Deviance/df = ", round(dev.df, 2), "; GOF thresholds: 2 SD = ",
   round(gof.threshold[1], 2), ", 3 SD = ", round(gof.threshold[2], 2), sep = ""),
   side = 1, line = 6, cex = 1.0, adj = 0)


  # Delta X^2 or D vs. predicted prob. with plotting point proportional to Cook's distance
  if(bubble == TRUE) {
   symbols(x = pred, y = resid.plot21, circles = scale.cookd(cookd), xlab = "Estimated probabilities",
    ylab = plot.label21,
    main = paste(plot.label21, "vs. est. prob. \n with plot point proportional to Cook's distance"),
    inches = 0.1, ylim = c(0, max(9, resid.plot21)))
   abline(h = c(4, 9), lty = "dotted", col = "blue")
   if(identify.points == TRUE) {
    identify(x = pred, y = resid.plot21, labels = labels(pred))
   } }
  else {
   # Empty plot because the last plot would be exactly the same
   plot(x = c(0, 1), y = c(0,1), type = "n", axes = FALSE, xlab = " ", ylab = " ",)
  }


  # Return to normal layout
  layout(mat = 1)
  
  # Information is stored in the object, but not printed unless requested
  invisible(list(pearson = pearson, stand.resid = stand.resid, stand.dev.resid = stand.dev.resid,
   deltaXsq = deltaXsq, deltaD = deltaD, cookd = cookd,
   pear.stat = pear.stat, dev = dev, dev.df = dev.df, gof.threshold = gof.threshold, pi.hat = pred, h = h))
}

    
