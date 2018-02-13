diet <- read.csv(file = "C:\\data\\fiber.csv")
head(diet)

diet$fiber<-factor(x = diet$fiber, levels = c("none", "bran", "gum", "both"))
diet$bloat<-factor(x = diet$bloat, levels = c("none", "low", "medium", "high"))

diet.table<-xtabs(formula = count ~ fiber + bloat, data = diet)
diet.table

# Note: Analyzing Fiber as single factor.  Would be an improvement to break it into two
#  2-levels factors for Bran and Gum, and then look for effects of each on the associations
#  via odds ratios.

mod.nom <- glm(data=diet, formula=count~fiber*bloat, family=poisson(link="log"))
library(car)
Anova(mod.nom)

lin1 <- c(rep(3,4), rep(2,4), rep(1,4), rep(0,4))
lin2 <- c(rep(6,4), rep(3,4), rep(1,4), rep(0,4))
lin3 <- c(rep(1,4), rep(1,4), rep(1,4), rep(0,4))

mod.ord1 <-  glm(data=diet, formula=count~fiber + bloat + fiber:lin1, family=poisson(link="log"))
summary(mod.ord1)
Anova(mod.ord1)
anova(mod.ord1, mod.nom, test="Chisq")

mod.ord2 <-  glm(data=diet, formula=count~fiber + bloat + fiber:lin2, family=poisson(link="log"))
summary(mod.ord2)
Anova(mod.ord2)
anova(mod.ord2, mod.nom, test="Chisq")

mod.ord3 <-  glm(data=diet, formula=count~fiber + bloat + fiber:lin3, family=poisson(link="log"))
summary(mod.ord3)
Anova(mod.ord3)
anova(mod.ord3, mod.nom, test="Chisq")

# Model 2 seems to give the best fit, so will estimate ORs for that.
#  Note that there are gaps of 1 (none-mild), (2 (mild-moderate), and 3(moderate-severe)
#  so need to estimate all three ORs.
#  All ORs are for fiber source relative to None


coef.mat <- rbind(c(rep(x=0, times=7), 0,1,0),c(rep(x=0, times=7), 0,2,0),
c(rep(x=0, times=7), 0,3,0),c(rep(x=0, times=7), 0,0,1),
c(rep(x=0, times=7), 0,0,2),c(rep(x=0, times=7), 0,0,3),
c(rep(x=0, times=7), 1,0,0),c(rep(x=0, times=7), 2,0,0),
c(rep(x=0, times=7), 3,0,0)
                  )

# Instead compute Wald intervals for predicted means in each group
# Wald CIs by manual computation
beta <- matrix(coef(mod.ord2)[-11],ncol=1)
v.beta <-vcov(mod.ord2)
# Estimate Lin Combos and standard errors as matrix computations
log.lincomb <- (coef.mat) %*% beta
SElog.lincomb <- matrix(sqrt(diag((coef.mat) %*% v.beta %*% t(coef.mat))),ncol=1)
# Compute confidence intervals in linear predictor scale
alpha=0.05
lower.log <- log.lincomb + qnorm(alpha/2)*SElog.lincomb
upper.log <- log.lincomb + qnorm(1-alpha/2)*SElog.lincomb
# Combine Lin Combo coefficients, estimates of linear combs, and confidence intervals in mean scale
wald.ci <- round(data.frame(exp(log.lincomb),exp(lower.log),exp(upper.log)),2)
# Attach contrast names to rows and columns.                  
row.names(wald.ci) <- c("Bran,1","Bran,2", "Bran,3", "Gum,1","Gum,2","Gum,3",
                        "Both,1","Both,2","Both,3")
colnames(wald.ci) <- c("Estimate","Lower CI","Upper CI")
wald.ci


# Model 2 seems to give the best fit, so will estimate ORs for that.
#  Note that there are gaps of 1 (none-mild), (2 (mild-moderate), and 3(moderate-severe)
#  so need to estimate all three ORs.
#  All ORs are for fiber source relative to None

# Next do the 3-2-1-0 model, since that is likely to be a popular choice

coef.mat <- rbind(c(rep(x=0, times=7), 0,1,0),c(rep(x=0, times=7), 0,2,0),
                  c(rep(x=0, times=7), 0,3,0),c(rep(x=0, times=7), 0,0,1),
                  c(rep(x=0, times=7), 0,0,2),c(rep(x=0, times=7), 0,0,3),
                  c(rep(x=0, times=7), 1,0,0),c(rep(x=0, times=7), 2,0,0),
                  c(rep(x=0, times=7), 3,0,0)
)

# Instead compute Wald intervals for predicted means in each group
# Wald CIs by manual computation
beta <- matrix(coef(mod.ord2)[-11],ncol=1)
v.beta <-vcov(mod.ord2)
# Estimate Lin Combos and standard errors as matrix computations
log.lincomb <- (coef.mat) %*% beta
SElog.lincomb <- matrix(sqrt(diag((coef.mat) %*% v.beta %*% t(coef.mat))),ncol=1)
# Compute confidence intervals in linear predictor scale
alpha=0.05
lower.log <- log.lincomb + qnorm(alpha/2)*SElog.lincomb
upper.log <- log.lincomb + qnorm(1-alpha/2)*SElog.lincomb
# Combine Lin Combo coefficients, estimates of linear combs, and confidence intervals in mean scale
wald.ci <- round(data.frame(exp(log.lincomb),exp(lower.log),exp(upper.log)),2)
# Attach contrast names to rows and columns.                  
row.names(wald.ci) <- c("Bran,1","Bran,2", "Bran,3", "Gum,1","Gum,2","Gum,3",
                        "Both,1","Both,2","Both,3")
colnames(wald.ci) <- c("Estimate","Lower CI","Upper CI")
wald.ci
library(xtable)
print(xtable(wald.ci), floating=FALSE)
