#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 14 Feb 13                                                   #
# PURPOSE: LASSO on Placekick Data                                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################
# LASSO on placekick data using glmnet package 

placekick <- read.table(file = "C:\\data\\Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)

library(glmnet)

# Fit LASSO by glmnet(y = , x = ). Gaussian is default, but other families are available 
# Function produces series of fits for many values of lambda. 
# glmnet() requires x to be in matrix class, so saving out 
#  the separate variables to be used as Y and X.

yy <- as.matrix(placekick[,9])
xx <- as.matrix(placekick[,1:8])

# Basic LASSO fit, showing results for many lambda values.
lasso.fit <- glmnet(y = yy, x = xx, family = "binomial")

# List out coefficients for each lambda 
round(coef(lasso.fit), digits = 3)
# Adding lambda values to output 
las.lambda <- lasso.fit$lambda
length(las.lambda)
# Matrix(x,sparse = TRUE) converts zeroes on matrix x into dots for nicer print
las.coefs <- Matrix(rbind(as.matrix(coef(lasso.fit)), las.lambda), sparse = TRUE)
#splitting up range of columns for better printing) 
round(las.coefs[,c(1:5)], digits = 4)
round(las.coefs[,c(21:25)], digits = 4)
round(las.coefs[,c(60:63)], digits = 4)

# cv.glmnet() uses crossvalidation to estimate optimal lambda
# Fix seed for crossvalidation, so the book's results can be duplicated

set.seed(27498272)
cv.lasso.fit <- cv.glmnet(y = yy, x = xx, family = "binomial")

# Default plot method for cv.lambda() produces CV errors +/- 1 SE at each lambda. 
x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure5.1color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(cv.lasso.fit)
# dev.off()  # Create plot for book
# Print out coefficients at optimal lambda
coef(cv.lasso.fit) 
# Another way to do this
coef(lasso.fit,s = cv.lasso.fit$lambda.1se) 
# Predicted response values
predict.las <- predict(cv.lasso.fit, newx = xx, type = "response")
head(cbind(placekick$distance, round(predict.las, digits = 3)))
