#####################################################################
# NAME: Chris Bilder                                                #
# DATE: 6-23-11                                                     #
# PURPOSE: Test for independence with data from                     #
# http://lib.stat.cmu.edu/DASL/Stories/HighFiberDietPlan.html       #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Read in data

 diet <- read.csv(file = "C:\\data\\fiber.csv")

 fiber <- factor(x = diet$fiber, levels = c("none", "bran", "gum", "both"))
 bloat <- factor(x = diet$bloat, levels = c("none", "low", "medium", "high"))
 diet2 <- data.frame(fiber, bloat, count = diet$count)

 diet.table <- xtabs(formula = count ~ fiber + bloat, data = diet2)
 diet.table


#####################################################################
# Fisher exact test

 fisher.test(x = diet.table)


################################################
# Permuation test - just p-value

 set.seed(8912)
 chisq.test(x = diet.table, correct = FALSE, simulate.p.value = TRUE, B = 1000)

 # C.I. for p-value
 set.seed(8912)
 save.p <- chisq.test(x = diet.table, correct = FALSE, simulate.p.value = TRUE, B = 1000)
 library(package = binom)
 binom.confint(x = round(save.p$p.value*1000,0), n = 1000, conf.level = 1-0.05, methods = "wilson")

 # Larger number of permutations
 set.seed(8912)
 save.p2 <- chisq.test(x = diet.table, correct = FALSE, simulate.p.value = TRUE, B = 100000)
 binom.confint(x = round(save.p2$p.value*100000,0), n = 100000, conf.level = 1-0.05, methods = "wilson")

##############################################################################
# Permutation test

  # Put the data into its raw form
  set1 <- as.data.frame(as.table(diet.table))
  tail(set1)  # Notice 2 obs. for fiber = both and bloat = high
  set2 <- set1[rep(1:nrow(set1), times = set1$Freq), -3]
  tail(set2)  # Notice 2 obs. for fiber = both and bloat = high

  # Verify data is correct
  xtabs(formula = ~ set2[,1] + set2[,2])
  X.sq <- chisq.test(set2[,1], set2[,2], correct = FALSE)
  X.sq$statistic

  # Could specify the variables directly too in xtabs()
  xtabs(formula = ~ fiber + bloat, data = set2)


 ###########################################################
 # Another way to put the data in a raw form using for loops
 
 # Put data into its raw form
 all.data <- matrix(data = NA, nrow = 0, ncol = 2)

 # Put data in "raw" form
 for (i in 1:nrow(diet.table)) {
   for (j in 1:ncol(diet.table)) {
    all.data <- rbind(all.data, matrix(data = c(i, j), nrow = diet.table[i,j], ncol = 2, byrow = T))
  }
 }
 # Note that warning messages will be generated since diet.table[i,j] = 0 sometimes

 # Verify data is correct
 head(all.data)  # First 6 rows
 tail(all.data)  # Last 6 rows
 xtabs(formula = ~ all.data[,1] + all.data[,2])
 

 ###########################################################
 # Another way to put the data in a raw form using for loops
 #  and no warning messages

 all.data2 <- matrix(data = NA, nrow = sum(diet.table), ncol = 2)
 counter <- 1

 # Put data in "raw" form
 for (i in 1:nrow(diet.table)) {
   for (j in 1:ncol(diet.table)) {
    if (diet.table[i,j] != 0) {
     all.data2[counter:(counter+diet.table[i,j]-1),] <- matrix(data = c(i, j), nrow = diet.table[i,j], ncol = 2, byrow = T)
     counter <- counter + diet.table[i,j]
    }
  }
 }
 
 # Verify data is correct
 xtabs(formula = ~ all.data2[,1] + all.data2[,2])
 X.sq <- chisq.test(all.data[,1], all.data[,2], correct = F)
 X.sq$statistic


 ######################################################################
 # Do one permutation to illustrate 
 
 set.seed(4088)
 set2.star <- data.frame(row = set2[,1], column = sample(set2[,2], replace = FALSE))
 xtabs(formula = ~ set2.star[,1] + set2.star[,2])
 X.sq.star <- chisq.test(set2.star[,1], set2.star[,2], correct = FALSE)
 X.sq.star$statistic



 ######################################################################
 #Repeat the permutation B times 
 
 B <- 1000
 X.sq.star.save <- matrix(data = NA, nrow = B, ncol = 1)

 set.seed(1938)
 # options(warn = -1)
 for(i in 1:B) {
  set2.star <- data.frame(row = set2[,1], column = sample(set2[,2], replace = FALSE))
  X.sq.star <- chisq.test(set2.star[,1], set2.star[,2], correct = FALSE)
  X.sq.star.save[i,1] <- X.sq.star$statistic
 }

 mean(X.sq.star.save >= X.sq$statistic)
 # options(warn = 0)
 
 summarize <- function(result.set, statistic, df, B, color.line = "red") {

  par(mfrow = c(1,3), mar = c(5,4,4,0.5))

  # Histogram
  hist(x = result.set, main = "Histogram", freq = FALSE,
   xlab = expression(X^{"2*"}))
  # , ylim = c(0,0.11) - used for book plot
  curve(expr = dchisq(x = x, df = df), col = color.line, add = TRUE, lwd = 2)
  segments(x0 = statistic, y0 = -10, x1 = statistic, y1 = 10)
  
  # Compare CDFs
  plot.ecdf(x = result.set, verticals = TRUE, do.p = FALSE, main = "CDFs", lwd = 2, col = "black",
      xlab = expression(X^"2*"),
      ylab = "CDF")
  curve(expr = pchisq(q = x, df = df), col = color.line, add = TRUE, lwd = 2, lty = "dotted")
  legend(x = df, y = 0.4, legend = c(expression(Perm.), substitute(chi[df1]^2, list(df1 = df))), lwd = c(2,2),
   col = c("black", color.line), lty = c("solid", "dotted"), bty = "n")  # When expression() is removed, the substitute() part oddly does not work

  # QQ-Plot
  chi.quant <- qchisq(p = seq(from = 1/(B+1), to = 1-1/(B+1), by = 1/(B+1)), df = df)
  plot(x = sort(result.set), y = chi.quant, main = "QQ-plot",
     xlab = expression(X^{"2*"}), ylab = "Chi-square quantiles")
  abline(a = 0, b = 1)

  par(mfrow = c(1,1))

  # p-value
  mean(result.set >= statistic)
 }

 x11(width = 9, height = 6, pointsize = 20)
 # pdf(file = "c:\\figures\\Figure6.2color.pdf", width = 9, height = 6, colormodel = "cmyk",  pointsize = 16)   # Create plot for book
 summarize(result.set = X.sq.star.save, statistic = X.sq$statistic, df = (nrow(diet.table)-1)*(ncol(diet.table)-1), B = B)
 # dev.off()  # Create plot for book

 # Black-and-white version of plot
 # pdf(file = "c:\\figures\\Figure6.2BW.pdf", width = 9, height = 6, colormodel = "cmyk", pointsize = 16)   # Create plot for book
 summarize(result.set = X.sq.star.save, statistic = X.sq$statistic,
   df = (nrow(diet.table)-1)*(ncol(diet.table)-1), B = B, color.line = "black")
 # dev.off()  # Create plot for book

 qqplot <- data.frame(p = seq(from = 1/(B+1), to = 1-1/(B+1), by = 1/(B+1)),
  q = qchisq(p = seq(from = 1/(B+1), to = 1-1/(B+1), by = 1/(B+1)), df = 9),
  X.sq.star = sort(X.sq.star.save)) 
 head(qqplot)
 qqplot[495:505,]



##############################################################################
# Permutation test using boot()

 library(boot)  # Already present in a default installation of R (do not need to download it separately) 
           
 # Perform the test
 x.sq <- function(data, i) {
  perm.data <- data[i]
  chisq.test(all.data[,1], perm.data, correct = F)$statistic
 }


 set.seed(6488)
 perm.test <- boot(data = all.data[,2], statistic = x.sq, R = 1000, sim = "permutation")
 names(perm.test)

 mean(perm.test$t >= perm.test$t0)
 









