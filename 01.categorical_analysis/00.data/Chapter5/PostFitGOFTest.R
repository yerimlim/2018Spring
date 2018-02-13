#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 06-24-2013                                                  #
# PURPOSE: Grouped-prediction goodness of fit test for count models #
#                                                                   #
# NOTES:                                                            #
# Program operates on user-supplied numerical objects containing    #
# observed counts for each observation and predicted counts in the  #
# same order. User can supply number of groups; uses n/5 otherwise, #
# unless n>100, whereupn g defaults to 20.                          #
#                                                                   #
# Source this program before calling the function.                  #
#####################################################################



PostFitGOFTest = function(obs, pred, g = 0) {
  if(g == 0) g = round(min(length(obs)/5,20))
 ord <- order(pred)
 obs.o <- obs[ord]
 pred.o <- pred[ord]
 interval = cut(pred.o, quantile(pred.o, 0:g/g), include.lowest = TRUE)  # Creates factor with levels 1,2,...,g
 counts = xtabs(formula = cbind(obs.o, pred.o) ~ interval)
 centers <- aggregate(formula = pred.o ~ interval, FUN = "mean")
 pear.res <- rep(NA,g)
 for(gg in (1:g)) pear.res[gg] <- (counts[gg] - counts[g+gg])/sqrt(counts[g+gg])
 pearson <- sum(pear.res^2)
 if (any(counts[((g+1):(2*g))] < 5))
  warning("Some expected counts are less than 5. Use smaller number of groups")
 P = 1 - pchisq(pearson, g - 2)
 cat("Post-Fit Goodness-of-Fit test with", g, "bins", "\n", "Pearson Stat = ", pearson, "\n", "p = ", P, "\n")
 return(list(pearson = pearson, pval = P, centers = centers$pred.o, observed = counts[1:g], expected = counts[(g+1):(2*g)], pear.res = pear.res))
}
