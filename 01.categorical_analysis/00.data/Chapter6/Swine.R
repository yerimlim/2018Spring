###########################################################################
# NAME: Chris Bilder                                                      #
# DATE: 6-14-13                                                           #
# PURPOSE: Analyze swine waste management data                            #
# NOTES:                                                                  #
###########################################################################

######################################################################
# Examine the data

 library(package = MRCV)
 
 # Note that the row items are given first in the data set
 #  W1 = nitrogen, W2 = phosphorus, W3 = salt
 #  Y1 = lagoon, Y2 = pit, Y3 = natural drainage, Y4 = holding tank
 head(farmer2)


######################################################################
# Show how to use the functions for first example

 # Tables of data
 marginal.table(data = farmer2, I = 3, J = 4)
 item.response.table(data = farmer2, I = 3, J = 4)

 # Bonferroni
 MI.test(data = farmer2, I = 3, J = 4, type = "bon", add.constant = FALSE)

 # X^2_S and X^2_S,ij
 MI.stat(data = farmer2, I = 3, J = 4, add.constant = FALSE)

 # Bootstrap
 x11(width = 7, height = 6, pointsize = 14)
 set.seed(7812)
 MI.test(data = farmer2, I = 3, J = 4, B = 5000, type = "boot", add.constant = FALSE, plot.hist = TRUE)

 # Rao-Scott
 MI.test(data = farmer2, I = 3, J = 4, type = "rs2", add.constant = FALSE)


 # Can do all three tests at once with type = "all"
 set.seed(7812)
 # pdf(file = "c:\\figures\\Figure6.4color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
 test.stat <- MI.test(data = farmer2, I = 3, J = 4, type = "all",
   B = 5000, plot.hist = TRUE, print.status = TRUE, add.constant = FALSE)
 # dev.off()  # Create plot for book
 test.stat
 names(test.stat)
 names(test.stat$general)
 # Example of computing the Bonferronni values
 3*4*min(1-pchisq(q = test.stat$general$X.sq.S.ij, df = 1))


#####################################################################
# Example of one resample (SPMI test) for illustration purposes

 I <- 3
 J <- 4
 n <- nrow(farmer2)

 # Example resample
 set.seed(7812)
 iW <- sample(x = 1:n, size = n, replace = TRUE)
 iY <- sample(x = 1:n, size = n, replace = TRUE)

 # Use resampled index values to form resample
 farmer2.star <- cbind(farmer2[iW, 1:I], farmer2[iY, (I+1):(I+J)])
 head(farmer2.star)  # The row numbers here are from from iW
 MI.stat(data = farmer2.star, I = I, J = J, add.constant = FALSE)
 
 # Marginal table
 marginal.table(data = farmer2.star, I = 3, J = 4)


#####################################################################
# Regression modeling

 mod.data.format <- item.response.table(data = farmer2, I = 3, J = 4, create.dataframe = TRUE)
 head(mod.data.format)
 tail(mod.data.format)
 
 # Y-main effects model
 set.seed(8922)
 mod.fit1 <- genloglin(data = farmer2, I = 3, J = 4, model = "y.main", boot = TRUE, B = 2000,
  print.status = TRUE)
 # Equivalently, model = count ~ -1 + W:Y + wi:W:Y + yj:W:Y + wi:yj + wi:yj:Y
 #  W:Y   - Ww1:Yy1 to Ww3:Yy4 in output, beta_0(ij) in statement of model
 #  wi:W:Y - Ww1:Yy1:wi to Ww3:Yy4:wi in output, beta_a(ij) in statement of model
 #  yj:W:Y - Ww1:Yy1:yj to Ww3:Yy4:yj in output, beta_b(ij) in statement of model
 #  wi:yj  - wi:yj in output, lambda_ab in statement of model
 #  wi:yj:Y - Yy1:wi:yj to Yy4:wi:yj in output, lambda^Y_ab(ij) in statement of model
 # Also, equivalently, model = count ~ -1 + W:Y + wi%in%W:Y + yj%in%W:Y + wi:yj + wi:yj%in%Y
 #  This format helps to emphasize that a loglinear model under independence is essentially
 #  being fit to each 2x2 table due to the nested effects given. Then additional terms are
 #  added to the model to allow for OR_ij to not be equal to 1 and vary across the 2x2 tables.
 # Turn off buffered output (MISC > BUFFERED OUTPUT) if you would like to see the printed progress through
 #  the iterative proportional fitting algorithm.
 summary(mod.fit1)

 # Model estimated odds ratios
 options(width = 60)  # Helps with book formatting
 OR.mod <- predict(object = mod.fit1, alpha = 0.05)
 OR.mod

 # Standardized residuals
 options(width = 55)  # Helps with book formatting
 resid.mod <- residuals(object = mod.fit1)
 resid.mod$std.pearson.res.asymp.var
 resid.mod$std.pearson.res.boot.var

 # Other ways to specify the same model
 mod.fit2 <- genloglin(data = farmer2, I = 3, J = 4,
  model = count ~ -1 + W:Y + wi:W:Y + yj:W:Y + wi:yj + wi:yj:Y, boot = FALSE)
 # summary(mod.fit2)  # Same as summary(mod.fit1)
 mod.fit3 <- genloglin(data = farmer2, I = 3, J = 4,
  model = count ~ -1 + W:Y + wi%in%W:Y + yj%in%W:Y + wi:yj + wi:yj%in%Y, boot = FALSE)
 # summary(mod.fit3)  # Same as summary(mod.fit1)

 # Model comparison tests
 comp1 <- anova(object = mod.fit1, model.HA = "saturated", type = "all")
 comp1

 # Compare Y-main effects model to W and Y-main effects model
 comp2 <- anova(object = mod.fit1, model.HA = "wy.main", type = "all")
 comp2


 # Final model
 options(width = 80)
 set.seed(9912)
 mod.fit.final <- genloglin(data = farmer2, I = 3, J = 4,
  model = count ~ -1 + W:Y + wi%in%W:Y + yj%in%W:Y + wi:yj + wi:yj%in%Y + wi:yj%in%W3:Y1, boot = TRUE, B = 2000)
 summary(mod.fit.final)  # Excluded to save space
 comp.final1 <- anova(object = mod.fit.final, model.HA = "saturated", type = "all")
 comp.final1

 options(width = 55)  # Helps with book formatting
 resid.mod.final <- residuals(object = mod.fit.final)
 resid.mod.final$std.pearson.res.asymp.var
 resid.mod.final$std.pearson.res.boot.var

 OR.mod.final <- predict(mod.fit.final, alpha = 0.05)
 OR.mod.final


 # Example of a saturated model specification - notice the tested format allows
 #  for a different interaction within each 2x2 table.
 mod.fit.sat <- genloglin(data = farmer2, I = 3, J = 4,
  model = count ~ -1 + W:Y + wi%in%W:Y + yj%in%W:Y + wi:yj%in%W:Y, boot = FALSE, B = 2000)
 summary(mod.fit.sat)


#####################################################################
# This shows what happens when variable names are different from w1, w2, w3, y1, y2, y3, y4
#  Everything still works, but "W" and "Y" are still used as the MRCV names

 farmer2.temp <- farmer2
 names(farmer2.temp) <- c("a1", "a2", "a3", "b1", "b2", "b3", "b4")
 head(farmer2.temp)

 set1 <- item.response.table(data = farmer2.temp, I = 3, J = 4, create.dataframe = TRUE)
 head(set1)
 tail(set1)

 mod.fit.temp <- genloglin(data = farmer2.temp, I = 3, J = 4, model = "y.main", boot = FALSE)
 summary(mod.fit.temp)








#
