# This program is not referenced in the book, but it is referenced in
#   workshops that we give corresponding to the book.

c.table <- array(data = c(51, 74, 8146, 8124), dim = c(2,2), dimnames = list(Trt = c("vaccine", "placebo"), Response = c("HIV", "No HIV")))
c.table

ind.test <- chisq.test(x = c.table, correct = FALSE)
ind.test
ind.test$expected

library(package = vcd)
assocstats(x = c.table)

all.data <- as.data.frame(as.table(c.table))
all.data
M1 <- glm(formula = Freq ~ Trt * Response, family = poisson(link = "log"),
data = all.data)
summ1 <- summary(M1)
c(summ1$deviance, summ1$df.residual)
round(summ1$coefficients, digits = 4)

library(car)
Anova(M1)



















#