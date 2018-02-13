#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 1-10-13                                                     #
# PURPOSE: Stepwise variable selection techniques in Placekick data #
#                                                                   #
# NOTES:                                                            #
#####################################################################

placekick <- read.table(file = "C:\\data\\Placekick.csv", header = TRUE, sep = ",")
head(placekick)
tail(placekick)

# Must first fit the smallest and largest models to be considered

empty.mod <- glm(formula = good ~ 1, family = binomial(link = logit), data = placekick)
full.mod <- glm(formula = good ~ ., family = binomial(link = logit), data = placekick)

#####################################################################
# Selection using Information Criteria

# The step() function uses information criteria for variable selection, 
#  k = 2 (default) gives AIC, k = log(nrow(...)) gives BIC when "..." is 
#  replaced with the data set name
# Setting direction = "forward", "backward", or "both" (which is the default) controls 
#  which algorithm gets used.

# Showing use with BIC

forw.sel <- step(object = empty.mod, scope = list(upper = full.mod), direction = "forward", 
        k = log(nrow(placekick)), trace = TRUE)
anova(forw.sel)
# For illustration purposes, below is the code for k = 2 (AIC)
# forw.sel <- step(object = empty.mod, scope = list(upper = full.mod), direction = "forward",
#         k = 2, trace = TRUE)

# For backward elimination, start with full model (object = ) and work down to empty model (scope = )

back.sel <- step(object = full.mod, scope = list(lower = empty.mod), direction = "backward", 
         k = log(nrow(placekick)), trace = TRUE)
anova(back.sel)
# For illustration purposes, below is the code for k = 2 (AIC)
# back.sel <- step(object = full.mod, scope = list(lower = empty.mod), direction = "backward",
#          k = 2, trace = TRUE)

# For (alternating) stepwise selection, start with empty model (object = ) and work up to full model (scope = )

step.sel <- step(object = empty.mod, scope = list(upper = full.mod), 
         k = log(nrow(placekick)), trace = TRUE)
anova(step.sel)
# For illustration purposes, below is the code for k = 2 (AIC)
# step.sel <- step(object = empty.mod, scope = list(upper = full.mod),
#          k = 2, trace = TRUE)


#####################################################################
# Selection using p-values

# The step() function can be "tricked" into something that is equivalent to 
#  selection using p-values by specifying parameters test = "Chisq" and k = 0.

# For forward selection, start with empty model (object = ) and work up to full model (scope = )

forw.sel.p <- step(object = empty.mod, scope = list(upper = full.mod), direction = "forward", 
         test = "Chisq", k = 0, trace = TRUE)
anova(forw.sel.p)

# For backward elimination, start with full model (object = ) and work down to empty model (scope = )

back.sel.p <- step(object = full.mod, scope = list(lower = empty.mod), direction = "backward", 
         test = "Chisq", k = 10, trace = TRUE)
anova(back.sel.p)

# For (alternating) stepwise selection, start with empty model (object = ) and work up to full model (scope = )

step.sel.p <- step(object = empty.mod, scope = list(upper = full.mod), 
         test = "Chisq", k = 0, trace = TRUE)
anova(step.sel.p)
