#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-5-11                                                     #
# PURPOSE: Plot of proportional and nonproportional odds models     #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Plot of proportional odds model

  # Remember that beta10 < beta20 < beta30
  beta<-c(0, 2, 4, 2) #beta10, beta20, beta30, beta1
  x.range<-c(-5, 3)
  
  x11(width = 10, height = 6, pointsize = 12)

  # pdf(file = "c:\\figures\\Figure3.4color.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mfrow = c(1, 2))
  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(P(Y<=j)),
    xlab = expression(x[1]), main = "Cumulative probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[4]*x), add = TRUE, lty = "dashed", col = "red", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[4]*x), add = TRUE, lty = "dotted", , col = "blue", lwd = 2)
  legend(x = -5.5, y = 0.9, legend = c(expression(P(Y<=1)), expression(P(Y<=2)), expression(P(Y<=3))),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "red", "blue"),
    bty = "n", lwd = 2)
  
  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(pi[j]),
    xlab = expression(x[1]), main = "Probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[4]*x) - plogis(q = beta[1] + beta[4]*x), add = TRUE,
    lty = "dashed", col = "red", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[4]*x) - plogis(q = beta[2] + beta[4]*x), add = TRUE,
    lty = "dotted", , col = "blue", lwd = 2)
  curve(expr = 1 - plogis(q = beta[3] + beta[4]*x), add = TRUE,
    lty = "dotdash", col = "green", lwd = 2)
  legend(x = -5.5, y = 0.9, legend = c(expression(pi[1]), expression(pi[2]), expression(pi[3]), expression(pi[4])),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "red", "blue", "green"),
    bty = "n", lwd = 2)
  # dev.off()  # Create plot for book


  # Black-and-white version of plot
  # pdf(file = "c:\\figures\\Figure3.4BW.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mfrow = c(1, 2))
  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(P(Y<=j)),
    xlab = expression(x[1]), main = "Cumulative probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[4]*x), add = TRUE, lty = "dashed", col = "black", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[4]*x), add = TRUE, lty = "dotted", , col = "black", lwd = 2)
  legend(x = -5.5, y = 0.9, legend = c(expression(P(Y<=1)), expression(P(Y<=2)), expression(P(Y<=3))),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "black", "black"),
    bty = "n", lwd = 2)

  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(pi[j]),
    xlab = expression(x[1]), main = "Probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[4]*x) - plogis(q = beta[1] + beta[4]*x), add = TRUE,
    lty = "dashed", col = "black", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[4]*x) - plogis(q = beta[2] + beta[4]*x), add = TRUE,
    lty = "dotted", , col = "black", lwd = 2)
  curve(expr = 1 - plogis(q = beta[3] + beta[4]*x), add = TRUE,
    lty = "dotdash", col = "black", lwd = 2)
  legend(x = -5.5, y = 0.9, legend = c(expression(pi[1]), expression(pi[2]), expression(pi[3]), expression(pi[4])),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "black", "black", "black"),
    bty = "n", lwd = 2)
  # dev.off()  # Create plot for book



#####################################################################
# Plot of non-proportional odds model

  beta<-c(0, 2, 4, 2, 3, 6) #beta10, beta20, beta30, beta11, beta21, beta31
  #beta<-c(0, 2, 4, 2, 3, 4) #beta10, beta20, beta30, beta11, beta21, beta31  #Lines do not appear to cross
  x.range<-c(-5, 3)
  
  x11(width = 10, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure3.6color.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book

  par(mfrow = c(1, 2))
  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(P(Y<=j)),
    xlab = expression(x[1]), main = "Cumulative probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[5]*x), add = TRUE, lty = "dashed", col = "red", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[6]*x), add = TRUE, lty = "dotted", , col = "blue", lwd = 2)
  legend(x = -5, y = 0.9, legend = c(expression(P(Y<=1)), expression(P(Y<=2)), expression(P(Y<=3))),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "red", "blue", "green"),
    bty = "n", lwd = 2)

  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(pi[j]),
    xlab = expression(x[1]), main = "Probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[5]*x) - plogis(q = beta[1] + beta[4]*x), add = TRUE,
    lty = "dashed", col = "red", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[6]*x) - plogis(q = beta[2] + beta[5]*x), add = TRUE,
    lty = "dotted", , col = "blue", lwd = 2)
  curve(expr = 1 - plogis(q = beta[3] + beta[6]*x), add = TRUE,
    lty = "dotdash", col = "green", lwd = 2)
  legend(x = -5, y = 0.9, legend = c(expression(pi[1]), expression(pi[2]), expression(pi[3]), expression(pi[4])),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "red", "blue", "green"),
    bty = "n", lwd = 2)
  # dev.off()  # Create plot for book


  # Black-and-white version of plot
  # pdf(file = "c:\\figures\\Figure3.6BW.pdf", width = 8, height = 6, colormodel = "cmyk")   # Create plot for book
  par(mfrow = c(1, 2))
  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(P(Y<=j)),
    xlab = expression(x[1]), main = "Cumulative probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[5]*x), add = TRUE, lty = "dashed", col = "black", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[6]*x), add = TRUE, lty = "dotted", , col = "black", lwd = 2)
  legend(x = -5, y = 0.9, legend = c(expression(P(Y<=1)), expression(P(Y<=2)), expression(P(Y<=3))),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "black", "black"),
    bty = "n", lwd = 2)

  curve(expr = plogis(q = beta[1] + beta[4]*x), xlim = x.range, ylab = expression(pi[j]),
    xlab = expression(x[1]), main = "Probabilities for Y", lwd = 2)
  curve(expr = plogis(q = beta[2] + beta[5]*x) - plogis(q = beta[1] + beta[4]*x), add = TRUE,
    lty = "dashed", col = "black", lwd = 2)
  curve(expr = plogis(q = beta[3] + beta[6]*x) - plogis(q = beta[2] + beta[5]*x), add = TRUE,
    lty = "dotted", , col = "black", lwd = 2)
  curve(expr = 1 - plogis(q = beta[3] + beta[6]*x), add = TRUE,
    lty = "dotdash", col = "black", lwd = 2)
  legend(x = -5, y = 0.9, legend = c(expression(pi[1]), expression(pi[2]), expression(pi[3]), expression(pi[4])),
    lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black", "black", "black", "black"),
    bty = "n", lwd = 2)
  # dev.off()  # Create plot for book

















#
