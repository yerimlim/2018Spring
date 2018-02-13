#####################################################################
# NAME:  Tom Loughin and Chris Bilder                               #
# DATE:  01-23-12                                                   #
# PURPOSE: Use Poisson regression models with the Larry Bird Data   #
#          to test for independence between first and second shots  #
#                                                                   #
# NOTES:                                                            #
#####################################################################

# Create contingency table - notice the data is entered by columns
c.table <- array(data = c(251, 48, 34, 5), dim = c(2,2), dimnames = list(First = c("made", "missed"),
             Second = c("made", "missed")))

# Convert data to data frame for glm()
all.data <- as.data.frame(as.table(c.table))
all.data

# Fit Poisson Regression
M1 <- glm(formula = Freq ~ First*Second, family = poisson(link = "log"), data = all.data)
summary(M1)

# Odds Ratio Estimate

round(exp(M1$coefficients[4]), digits = 2)
inter <- confint(M1, parm = "Firstmissed:Secondmissed")
round(exp(inter), digits = 2)
round(exp(confint.default(M1, parm = "Firstmissed:Secondmissed")), digits = 2)


library(car)
Anova(M1) # Test for OR = 1 