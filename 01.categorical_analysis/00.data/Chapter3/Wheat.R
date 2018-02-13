#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-10-11                                                    #
# PURPOSE: Multicategory logit models with the wheat data           #
#                                                                   #
# NOTES:                                                            #
#####################################################################

wheat <- read.csv(file  = "C:\\data\\wheat.csv")

head(wheat, n = 3)  # n argument gives the number of rows to print
tail(wheat, n = 3)


###################################################################################
# Plots
  
  ################################
  # Stars plot - every observation is a "star" where the radius of a ray is proportional to a variable value
  
    x11(width = 7, height = 6, pointsize = 12)
    
    stars(x = wheat[order(wheat$type),-1], ncol = 20, key.loc = c(10, 0), draw.segments=TRUE, label = NULL,
      cex=0.75)

  ################################
  # Parallel coordinate plot

    library(package = MASS)  # Location of parcoord() function

    x11(width = 10, height = 6, pointsize = 12)
 
    # Reorder variables because class is binary (may distort plot)
    # Create indicator variable for class
    wheat2<-data.frame(kernel = 1:nrow(wheat), wheat[,2:6],  
           class.new = ifelse(test = wheat$class == "hrw", yes = 0, no = 1))
    head(wheat2)

    # Colors by condition:
    wheat.colors<-ifelse(test = wheat$type=="Healthy", yes = "black", 
                    no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))
    # Line type by condition:
    wheat.lty<-ifelse(test = wheat$type=="Healthy", yes = "solid", 
                    no = ifelse(test = wheat$type=="Sprout", yes = "longdash", no = "dotdash"))
    # pdf(file = "c:\\figures\\Figure3.2color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    parcoord(x = wheat2, col = wheat.colors, lty = wheat.lty)  # Plot
    legend(x = 6.15, y = 0.75, legend = c("Healthy", "Sprout", "Scab"), lty = c("solid", "longdash", "dotdash"),
      col=c("black", "red", "green"), cex=0.8, bty="n")
    #legend(locator(1), legend = c("Healthy", "Sprout", "Scab"), lty = c("solid", "longdash", "dotdash"),
    #  col=c("black", "red", "green"), cex=0.8, bty="n")  #Original legend using locator(1)
    # dev.off()  # Create plot for book

    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure3.2BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    parcoord(x = wheat2, col = 1, lty = wheat.lty) #Plot
    legend(x = 6.15, y = 0.75, legend = c("Healthy", "Sprout", "Scab"), lty = c("solid", "longdash", "dotdash"),
      col=1, cex=0.8, bty="n")
    # dev.off()  # Create plot for book

    # What observation has the largest weight
    wheat[wheat$weight == min(wheat2$weight),]  # 269
    order(wheat$size)  # 268 has the 2nd largest size
   
    
    #############
    # OTHER PLOTS:
        
    # Example highlighting observation #269
    wheat[269,]  # scab
    wheat.colors<-ifelse(test = wheat$type=="Healthy", yes = "black", 
                    no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))
    wheat.colors[269]<-"purple"
    line.width<-c(rep(x = 1, times = 268), 10, rep(x = 1, times = 6))            
    parcoord(x = wheat2, col = wheat.colors, lwd = line.width, lty = wheat.lty, 
      main = "Parallel coordinate plot for wheat data - highlight kernel 269")
    legend(locator(1), legend=c("Healthy", "Sprout", "Scab", "Kernel 269"), lty = c("solid", "longdash", "dotted", "dotted"), 
       col=c("black", "red", "green", "purple"), cex=0.75, bty="n", lwd = c(1, 1, 1, 10)) 

    # Sort by wheat type
    wheat.colors2<-ifelse(test = wheat$type=="Healthy", yes = 1, 
                   no = ifelse(test = wheat$type=="Sprout", yes = 2, no = 3))
    wheat3<-data.frame(wheat.colors2, wheat2)     
    x11(width = 7, height = 7, pointsize = 9)         
    parcoord(x = wheat3[order(wheat.colors2),], col = wheat.colors[order(wheat.colors2)], 
           main = "Parallel coordinate plot for wheat data - sort by Type")
    legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), lty=c(1,1,1), col=c("black", "red", "green"), cex=1, bty="n") 

    # Another way to do these plots with brushing (highlight parts of plot to change colors)
    library(iplots)  
    ipcp(wheat3[order(wheat.colors2),])
  
 
  ###################################################################################
  # Principal cordinate analysis in order to plot in 3D

    save<-princomp(formula = ~ density + class.new + hardness + size + weight + moisture, data = wheat2, 
      cor = TRUE, scores = TRUE)
    summary(save, loadings = TRUE, cutoff = 0.0)
   
    par(pty = "s")
    #bubble.color<-ifelse(wheat2$class.new == 1, yes = "black", no = "blue")  # Can also see separation of SRW and HRW kernels - use instead of wheat.colors
    wheat.colors<-ifelse(test = wheat$type=="Healthy", yes = "black", 
                    no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))
    symbols(x = save$scores[,1], y = save$scores[,2], circles = save$scores[,3]-min(save$scores[,3]), 
            inches=0.25, xlab = "Principal component 1", ylab = "Principal component 2", fg = wheat.colors, 
            main = "Bubble plot for first three principal components \n Wheat data")  # Note that circles can not take on a negative value!
    abline(h = 0, lty = 1, lwd = 2)  
    abline(v = 0, lty = 1, lwd = 2)  
    text(x = save$scores[,1], y = save$scores[,2], col = 2, cex = 0.5)  # Put kernel number on plot
    legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), pch = c(1,1,1), 
      col=c("black", "red", "green"), cex=1, bty="n") 


###################################################################################
#Estimate multinomial regression model  

  library(package = nnet)
  
  levels(wheat$type)  #Shows the 3 categories
    
  #Estimate model
  mod.fit<-multinom(formula = type ~ class + density + hardness + size + weight + moisture, data=wheat)
  summary(mod.fit)

  #Check out method functions available
  class(mod.fit)
  #options(width = 60) #Formatting for book - 60 characters per line
  methods(class = multinom)
  #options(width = 80) #Back to default
  sqrt(vcov(mod.fit)[2,2])
  

  ################################
  # Hypothesis tests
   
    # Wald tests for one beta - usually not done since it tests only one parameter
    sum.fit<-summary(mod.fit)
    test.stat<-sum.fit$coefficients/sum.fit$standard.errors
    p.value<-2*(1-pnorm(q = abs(test.stat), mean = 0, sd = 1))
    round(test.stat,2)
    round(p.value,2)
  
    # LRT for class:
    mod.fit.Ho<-multinom(formula = type ~ density + hardness + size + weight + moisture, data=wheat)
    anova(mod.fit.Ho, mod.fit)   
    library(package = car)
    Anova(mod.fit)
    qchisq(p = 0.95, df = 2)
   
    # Additional code showing the LRT
    G.sq.Ho<-mod.fit.Ho$deviance 
    G.sq.Ha<-mod.fit$deviance 
    G.sq<-G.sq.Ho-G.sq.Ha
    p.value<-1-pchisq(q = G.sq, df = 2)
    data.frame(G.sq.Ho, G.sq.Ha, G.sq, p.value, df = 2)
   
 
  ################################
  # Odds ratios

    # Information about each variable to help with choosing c
    summary(wheat)
    sd.wheat<-apply(X = wheat[,-c(1,7,8)], MARGIN = 2, FUN = sd)
    #round(sd(wheat[,-c(1,7,8)]), 2)  # Older way
    c.value<-c(1, sd.wheat)  # class = 1 is first value
    round(c.value,2)
    
    # beta.hat_jr for r = 1, ..., 6  and j = 2, 3
    beta.hat2<-coefficients(mod.fit)[1,2:7]
    beta.hat3<-coefficients(mod.fit)[2,2:7]
    
    # Odds ratios for j = 2 vs. j = 1 (scab vs. healthy)
    round(exp(c.value*beta.hat2),2)
    round(1/exp(c.value*beta.hat2),2)
  
    # Odds ratios for j = 3 vs. j = 2 (sprout vs. healthy)
    round(exp(c.value*beta.hat3),2)
    round(1/exp(c.value*beta.hat3),2)
                       
    # Wald CIs
    conf.beta<-confint(object = mod.fit, level = 0.95)
    round(conf.beta,2)  # Results are stored in a 3D array
    conf.beta[2:7,1:2,1]  # C.I.s for beta_2r
    conf.beta[2:7,1:2,2]  # C.I.s for beta_3r

    # Test the multiplication
    c.value*conf.beta[2:7,1:2,1]
    c.value[2]*conf.beta[2,1:2,1]

    # CIs for OR
    ci.OR2<-exp(c.value*conf.beta[2:7,1:2,1])
    ci.OR3<-exp(c.value*conf.beta[2:7,1:2,2])  
    
    round(data.frame(low = ci.OR2[,1], up = ci.OR2[,2]), 2)
    round(data.frame(low = 1/ci.OR2[,2], up = 1/ci.OR2[,1]), 2)[c(2,5),]  # Specific rows to cut down on output in book
        
    round(data.frame(low = ci.OR3[,1], up = ci.OR3[,2]), 2)
    round(data.frame(low = 1/ci.OR3[,2], up = 1/ci.OR3[,1]), 2)[c(2,3),]  # Specific rows to cut down on output in book
       
    # Another way to do the Wald
    #vcov(mod.fit)  # Var^(beta^_1) is the (2,2) element of the matrix
    beta.ci<-coef(mod.fit)[1,2] + qnorm(p = c(0.025, 0.975))*sqrt(vcov(mod.fit)[2,2])
    beta.ci  # C.I. for beta_12 (sprout vs. healthy for SRW)
    round(exp(beta.ci*c.value[1]),2)



  ################################
  # Estimate probability of being in a particular category
  
    # pi^
    pi.hat<-predict(object = mod.fit, newdata = wheat, type = "probs")
    head(pi.hat)

    head(predict(object = mod.fit, newdata = wheat, type = "class"))


    # Predicted classification
    predict(object = mod.fit, newdata = wheat[1,], type = "class")  # default - this is helpful for discriminant analysis purposes
  
    # Compute pi^ using formulas
    expl.var<-c(1,0,as.numeric(wheat[1,2:6]))
    round(expl.var, 4)
    beta.hat<-coefficients(mod.fit)
    scab.part<-exp(sum(beta.hat[1,]*expl.var)) 
    sprout.part<-exp(sum(beta.hat[2,]*expl.var)) 
    pi.hat.scab<-scab.part/(1+scab.part+sprout.part)
    pi.hat.sprout<-sprout.part/(1+scab.part+sprout.part)
    pi.hat.healthy<-1/(1+scab.part+sprout.part)
    round(data.frame(pi.hat.healthy, pi.hat.scab, pi.hat.sprout), 4)
 
    ##############################
    # Confidence intervals for pi_j
    
    # Obtain observation values, ";" ends a command
    x1<-0;          x2<-wheat[1,2]; x3<-wheat[1,3]
    x4<-wheat[1,4]; x5<-wheat[1,5]; x6<-wheat[1,6]

    # Parts of character string
    scab<-"exp(b20 + b21*x1 + b22*x2 + b23*x3 + b24*x4 + b25*x5 + b26*x6)"
    sprout<-"exp(b30 + b31*x1 + b32*x2 + b33*x3 + b34*x4 + b35*x5 + b36*x6)"

    # pi^_Healthy

    # Direct way
    # g.healthy<-"1/(1 + exp(b20 + b21*x1 + b22*x2 + b23*x3 + b24*x4 + b25*x5 + b26*x6)
    #   + exp(b30 + b31*x1 + b32*x2 + b33*x3 + b34*x4 + b35*x5 + b36*x6))"

    g.healthy<-paste("1 / (1 + ", scab, "+", sprout, ")")
    g.healthy
    calc.healthy<-deltaMethod(object =  mod.fit, g = g.healthy,
      parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                         "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
    names(calc.healthy)
    calc.healthy$Estimate  # pi^_Healthy
    calc.healthy$SE        # sqrt(Var^(pi^_Healthy))
    alpha<-0.05
    calc.healthy$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.healthy$SE

    # pi^_Scab
    g.scab<-paste(scab, "/ (1 + ", scab, "+", sprout, ")")
    g.scab
    calc.scab<-deltaMethod(object =  mod.fit, g = g.scab,
      parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                         "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
    calc.scab
    calc.scab$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.scab$SE

    # pi^_Sprout
    g.sprout<-paste(sprout, "/ (1 + ", scab, "+", sprout, ")")
    g.sprout
    calc.sprout<-deltaMethod(object =  mod.fit, g = g.sprout,
      parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                         "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
    calc.sprout
    calc.sprout$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.sprout$SE




  ################################
  # Check out what is in mod.fit
  
    names(mod.fit)  # Note that there is no "coefficients" - need to use coefficients(mod.fit) instead
      
    mod.fit$deviance
    mod.fit$convergence
    head(mod.fit$fitted.values) 
    
    # Residuals
    head(mod.fit$residuals, n = 3)
    head(residuals(mod.fit), n = 3)
    c(1,0,0) - mod.fit$fitted.values[1,]  # First residual
    
    
  ################################
  # Plot density only model
  
    x11(width = 7, height = 6, pointsize = 12)

    # Estimate model with density only
    mod.fit.nom.density<-multinom(formula = type ~ density, data = wheat)
    summary(mod.fit.nom.density)
    beta.hat<-coefficients(mod.fit.nom.density)

    # Create plotting area first to make sure get the whole region with respect to x-axis
    #  pdf(file = "c:\\figures\\Figure3.3color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, type = "n",
      panel.first = grid(col = "gray", lty = "dotted"))
    # Plot each pi_j
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))  # Healthy
    curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "green", lty = "dotdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))  # Scab
    curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "red", lty = "longdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))  # Sprout
    legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
      col=c("black","red","green"), bty="n", lwd = c(2,2,2), seg.len = 4)
    # dev.off()  # Create plot for book
    # Verify plot is correct
    density.values<-seq(from = 0.8, to = 1.6, by = 0.1)
    data.frame(density.values, round(predict(object = mod.fit.nom.density, newdata = data.frame(density = density.values), type = "probs"), 2))


    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure3.3BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, type = "n")
    # Plot each pi_j
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))  # Healthy
    curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "dotdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))  # Scab
    curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "longdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))  # Sprout
    legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
      col=c("black","black","black"), bty="n", lwd = c(2,2,2), seg.len = 4)
    # dev.off()  # Create plot for book


    predict.data<-data.frame(class = "hrw", density = c(mean(wheat$density), mean(wheat$density)), hardness = mean(wheat$hardness),
       size = mean(wheat$size), weight = mean(wheat$weight), moisture = mean(wheat$moisture))
    pi.hat<-predict(object = mod.fit, newdata = predict.data, type = "probs")
    head(pi.hat)
    pi.hat[,1]

    # Create plotting area first to make sure get the whole region with respect to x-axis
    curve(expr = predict(object = mod.fit, newdata = data.frame(class = "hrw", density = x,
        hardness = mean(wheat$hardness), size = mean(wheat$size), weight = mean(wheat$weight),
        moisture = mean(wheat$moisture)), type = "probs")[,1], ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000,
      panel.first = grid(col = "gray", lty = "dotted"))
    curve(expr = predict(object = mod.fit, newdata = data.frame(class = "hrw", density = x,
        hardness = mean(wheat$hardness), size = mean(wheat$size), weight = mean(wheat$weight),
        moisture = mean(wheat$moisture)), type = "probs")[,2], ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "green", lty = "dotdash", lwd = 2, n = 1000,
      add = TRUE, panel.first = grid(col = "gray", lty = "dotted"))
    curve(expr = predict(object = mod.fit, newdata = data.frame(class = "hrw", density = x,
        hardness = mean(wheat$hardness), size = mean(wheat$size), weight = mean(wheat$weight),
        moisture = mean(wheat$moisture)), type = "probs")[,3], ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "red", lty = "longdash", lwd = 2, n = 1000,
      add = TRUE, panel.first = grid(col = "gray", lty = "dotted"))
    legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"), col=c("black","red","green"), bty="n", lwd = c(2,2,2))




  ################################
  # Additional investigations

    # Quadtratic and interaction terms example
    mod.fit.trans1<-multinom(formula = type ~ class + density + I(density^2) + density:class, data=wheat)  # Does not converge (notice says "stopped after 100")
    summary(mod.fit.trans1)
    mod.fit.trans2<-multinom(formula = type ~ class + density + I(density^2) + density:class, data=wheat,
      maxit = 1000)  # Converges
    summary(mod.fit.trans2)
   
    # Make the last level the base level
    wheat.original<-wheat  # Save original format
    wheat$type<-relevel(x = wheat$type, ref = "Sprout") 
    levels(wheat$type)
    mod.fit.relevel<-multinom(formula = type ~ class + density + hardness + size + weight + moisture, data=wheat)
    summary(mod.fit.relevel)
    wheat<-wheat.original  # Change back to original format
 
  
  ###############################################################################
  # Using vglm() from the VGAM (Vector Generalized Additive Models) package
  
    library(package = VGAM)
  
    # This will match mod.fit with multinom()
    mod.fit2<-vglm(formula = type ~ class + density + hardness + size + weight + moisture, 
       family = multinomial(refLevel = 1), data = wheat)
    summary(mod.fit2)
    names(mod.fit2)  # Does not provide useful information
    attributes(mod.fit2)  # EVERYTHING inside of mod.fit2 - a long list of items
    names(attributes(mod.fit2))  # Just list object names within it.
    class(mod.fit2)
    

    # Provides sqrt( Var^(log(pi^_j / pi^_1)) )
    save.pred<-predictvglm(object = mod.fit2, newdata = wheat[1,], type = "link", se.fit = TRUE)
    save.pred
    # predictvglm(object = mod.fit2, newdata = wheat[1,], type = "response", se.fit = TRUE)  # Not allowed
    
   
    # This is the default which uses the "last" level as the baseline
    mod.fit3<-vglm(formula = type ~ class + density + hardness + size + weight + moisture, family = multinomial(refLevel = "last"),
      data = wheat)
    summary(mod.fit3)
 

  
 

###################################################################################
# Proportional odds (cumulative logit) models
  
  library(package = MASS)  # If not done already
  
  # Need to reorder factor so that Scab < Sprout < Healthy
  levels(wheat$type)
  wheat$type.order<-factor(wheat$type, levels = c("Scab",  "Sprout", "Healthy"))
  head(wheat)
  levels(wheat$type.order)
  # Other ways:
  # ordered(x = wheat$type, levels = c("Scab",  "Sprout", "Healthy"))  #Also will do ordering
  # within(data = wheat, expr = type.ordered<-ordered(x = wheat$type, levels = c("Scab",  "Sprout", "Healthy")))
  
 
  # Model with all explanatory variables
  mod.fit.ord<-polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
  class(mod.fit.ord)
  summary(mod.fit.ord)
  library(package = car)  # If not done already
  Anova(mod.fit.ord)

  # Other ways to do the LRT without Anova()
  mod.fit.ord.class<-polr(formula = type.order ~ density + hardness + size + weight + moisture, data = wheat, method = "logistic")
  anova(mod.fit.ord.class, mod.fit.ord)
  test.stat<-mod.fit.ord.class$deviance - mod.fit.ord$deviance
  df<-mod.fit.ord.class$df.residual - mod.fit.ord$df.residual
  1 - pchisq(q = test.stat, df = df)  # LRT p-value
  sum.fit<-summary(mod.fit.ord)
  2*(1 - pnorm(q = sum.fit$coefficients[1,3]))  # Wald p-value
 
 
  ################################
  # Check method functions 
  
    class(mod.fit.ord) 
    #options(width = 60)  # Formatting for book - 60 characters per line
    methods(class = polr)
    #options(width = 80) #Back to default

   
  ################################
  # Odds ratios
    
    # Information about each variable to help with choosing c
    summary(wheat)
    sd.wheat<-apply(X = wheat[,-c(1,7,8)], MARGIN = 2, FUN = sd)
    c.value<-c(1, sd.wheat)
    round(c.value, 2)  # class = 1 is first value

    
    # OR
    round(exp(c.value*(-mod.fit.ord$coefficients)),2)
    round(1/exp(c.value*(-mod.fit.ord$coefficients)),2)

    # CIs
    conf.beta<-confint(object = mod.fit.ord, level = 0.95)
    conf.beta
    c.value*(-conf.beta)
    c.value[2]*(-conf.beta[2,])
    
    ci<-exp(c.value*(-conf.beta))
    round(data.frame(low = ci[,2], up = ci[,1]), 2)
    round(data.frame(low = 1/ci[,1], up = 1/ci[,2]), 2)
        
    # Example with only density
    confint(object = mod.fit.ord, parm = "density", level = 0.95)  
    
    # Wald
    vcov(mod.fit.ord)  # Var^(beta^_1) is the (2,2) element of the matrix
    beta.ci<-(-mod.fit.ord$coefficients[2]) + qnorm(p = c(0.025, 0.975))*sqrt(vcov(mod.fit.ord)[2,2])
    beta.ci
    round(rev(1/exp(beta.ci*c.value[2])),2)
   
   
  ################################
  # Estimate probability of being in a particular category
 
    pi.hat.ord<-predict(object = mod.fit.ord, type = "probs")
    head(pi.hat.ord)
    
    # Example with newdata argument and first kernel
    predict(object = mod.fit.ord, newdata = wheat[1,], type = "probs")

    # Predicted classifications
    head(predict(object = mod.fit.ord, type = "class"))

    # Estimate pi_j without predict()
    kern1<-as.numeric(c(0, wheat[1,-c(1,7,8)]))  # Kernel #1
    round(kern1,2)
    mod.fit.ord$zeta  # beta^_10, beta^_20
    pi.scab<-plogis(q = mod.fit.ord$zeta[1] + sum(-mod.fit.ord$coefficients*kern1))
    pi.sprout<-plogis(q = mod.fit.ord$zeta[2] + sum(-mod.fit.ord$coefficients*kern1)) - 
               plogis(q = mod.fit.ord$zeta[1] + sum(-mod.fit.ord$coefficients*kern1))
    pi.healthy<-1 - plogis(q = mod.fit.ord$zeta[2] + sum(-mod.fit.ord$coefficients*kern1))
    pi.hat<-data.frame(pi.scab, pi.sprout, pi.healthy, row.names = NULL) 
    head(pi.hat)
 
 
    ##############################
    # Confidence intervals for pi_j

    # Obtain observation values, ";" means end of expression
    x1<-0;          x2<-wheat[1,2]; x3<-wheat[1,3]
    x4<-wheat[1,4]; x5<-wheat[1,5]; x6<-wheat[1,6]

    # Direct way
    beta.hat<-c(-mod.fit.ord$coefficients, mod.fit.ord$zeta)
    names(beta.hat)<-c("b1", "b2", "b3", "b4", "b5", "b6", "b10", "b20")
    beta.hat
    vcov(mod.fit.ord)
    g.scab<-"exp(b10 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)/(1 + exp(b10 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6))"

    vcov(mod.fit.ord)
    cov.mat<-vcov(mod.fit.ord)
    numb.int<-length(mod.fit.ord$zeta)
    numb.slope<-length(mod.fit.ord$coefficients)
    cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]<-
      -cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]
    cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]<-
      -cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]
    cov.mat

    # Uses deltaMethod.default()
    deltaMethod(object = beta.hat, g = g.scab, vcov. = cov.mat)


    # Replacement function for deltaMethod.polr()
    deltaMethod.polr2<-function(object, g)  {
      # All beta^'s where the slope estimates are adjusted
      beta.hat<-c(-object$coefficients, object$zeta)

      # Count the number of slopes and intercepts
      numb.slope<-length(object$coefficients)
      numb.int<-length(object$zeta)

      # Name corresponding parameters
      names(beta.hat)<-c(paste("b", 1:numb.slope, sep=""), paste("b", 1:numb.int, "0", sep=""))

      # Fix covariance matrix - All covariances between slopes and intercepts
      #  need to be multiplied by -1
      cov.mat<-vcov(object)
      # Above diagonal
      cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]<-
        -cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]
      # Below diagonal
      cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]<-
        -cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]

      # deltaMethod.default() method function completes calculations
      deltaMethod(object = beta.hat, g = g, vcov. = cov.mat)
    }

    alpha<-0.05
    
    # Parts of character string
    scab<-"exp(b10 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)"
    sprout<-"exp(b20 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)"

    # pi^_Scab
    g.scab<-paste(scab, "/ (1 + ", scab, ")")
    g.scab
    calc.scab<-deltaMethod.polr2(object = mod.fit.ord, g = g.scab)
    calc.scab$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.scab$SE

    # pi^_Sprout
    g.sprout<-paste(sprout, "/ (1 + ", sprout, ")", " - ", scab, "/ (1 + ", scab, ")")
    g.sprout
    calc.sprout<-deltaMethod.polr2(object = mod.fit.ord, g = g.sprout)
    calc.sprout$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.sprout$SE

    # pi^_Healthy
    g.healthy<-paste("1 - ", sprout, "/ (1 + ", sprout, ")")
    g.healthy
    # Alternatively, directly enter the string:
    g.healthy<-"1 - exp(b20 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6) /
      (1 + exp(b20 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6))"
    calc.healthy<-deltaMethod.polr2(object = mod.fit.ord, g = g.healthy)
    calc.healthy$Estimate  # pi^_Healthy
    calc.healthy$SE  # sqrt(Var^(pi^_Healthy))
    calc.healthy$Estimate + qnorm(p = c(alpha/2, 1-alpha/2))*calc.healthy$SE



  ################################
  # Plots
 
    x11(width = 7, height = 6, pointsize = 12)
  
    # Estimate model with density only
    mod.fit.dens<-polr(formula = type.order ~ density, data = wheat, method = "logistic")
    summary(mod.fit.dens)

    min(wheat$density)
    max(wheat$density)
    
    # Get whole plot area first without model plotted
    # pdf(file = "c:\\figures\\Figure3.5color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, type = "n",
      panel.first = grid(col = "gray", lty = "dotted"))
    lwd.mult<-2
    # Plot each pi_j for multinommial regression model
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))  # Healthy
    curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "green", lty = "dotdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))  # Scab
    curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "red", lty = "longdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))  # Sprout

    lwd.po<-4
    # Plot each pi_j for proportional odds model
    curve(expr = plogis(q = mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "green",
     type = "l", xlim = c(min(wheat$density[wheat$type.order == "Scab"]), max(wheat$density[wheat$type.order == "Scab"])),
     add = TRUE, lty = "dotdash", lwd = lwd.po, n = 1000)  # Scab
    curve(expr = plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x) - plogis(q =mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "red",
     type = "l", xlim = c(min(wheat$density[wheat$type.order == "Sprout"]), max(wheat$density[wheat$type.order == "Sprout"])),
     add = TRUE, lty = "longdash", lwd = lwd.po, n = 1000)  # Sprout
    curve(expr = 1 - plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x), col = "black",
     type = "l", xlim = c(min(wheat$density[wheat$type.order == "Healthy"]), max(wheat$density[wheat$type.order == "Healthy"])),
     add = TRUE, lty = "solid", lwd = lwd.po, n = 1000)  # Healthy
    legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
      col=c("black","red","green"), bty="n", lwd = c(2,2,2), seg.len = 4)
    # dev.off()  # Create plot for book

    
    # Black-and-white version of plot
    # pdf(file = "c:\\figures\\Figure3.5BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, type = "n")
    lwd.mult<-2
    # Plot each pi_j for multinommial regression model
    curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))  # Healthy
    curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "dotdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))  # Scab
    curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "longdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))  # Sprout

    lwd.po<-4
    # Plot each pi_j for proportional odds model
    curve(expr = plogis(q = mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "black",
     type = "l", xlim = c(min(wheat$density[wheat$type.order == "Scab"]), max(wheat$density[wheat$type.order == "Scab"])),
     add = TRUE, lty = "dotdash", lwd = lwd.po, n = 1000)  # Scab
    curve(expr = plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x) - plogis(q =mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "black",
     type = "l", xlim = c(min(wheat$density[wheat$type.order == "Sprout"]), max(wheat$density[wheat$type.order == "Sprout"])),
     add = TRUE, lty = "longdash", lwd = lwd.po, n = 1000)  # Sprout
    curve(expr = 1 - plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x), col = "black",
     type = "l", xlim = c(min(wheat$density[wheat$type.order == "Healthy"]), max(wheat$density[wheat$type.order == "Healthy"])),
     add = TRUE, lty = "solid", lwd = lwd.po, n = 1000)  # Healthy
    legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
      col=c("black","black","black"), bty="n", lwd = c(2,2,2), seg.len = 4)
    # dev.off()  # Create plot for book

    

    # Note that I was not able to get curve(expr = predict(object = mod.fit.ord, newdata = data.frame(density = x), ...) to work
   
   
    # Histograms of the observed density levels help to show the density levels with respect to the kernel types
    par(mfrow = c(3,1))
    hist(wheat$density[wheat$type.order == "Scab"], xlim = c(0.5, 2.1), main = "Scab", col = "green", 
      breaks = seq(from = 0.5, to = 2.1, by = 0.1), xlab = "Density")
    hist(wheat$density[wheat$type.order == "Sprout"], xlim = c(0.5, 2.1), main = "Sprout", col = "red", 
      breaks = seq(from = 0.5, to = 2.1, by = 0.1), xlab = "Density")
    hist(wheat$density[wheat$type.order == "Healthy"], xlim = c(0.5, 2.1), main = "Healthy", col = "black", 
      breaks = seq(from = 0.5, to = 2.1, by = 0.1), xlab = "Density")
    par(mfrow = c(1,1))
 
 
  ################################
  # Additional calculations
   
  # Include transformation in model
  mod.fit.dens2<-polr(formula = type.order ~ density + I(density^2), data = wheat, method = "logistic")
  summary(mod.fit.dens2)
 
  # Residuals
  mod.fit.ord$residuals  # Residuals is not a component of mod.fit.ord
  residuals(object = mod.fit.ord)  # Does not work either
  obs<-data.frame(Scab = ifelse(test = wheat$type.order == "Scab", yes = 1, no = 0), 
             Sprout = ifelse(test = wheat$type.order == "Sprout", yes = 1, no = 0),
             Healthy = ifelse(test = wheat$type.order == "Healthy", yes = 1, no = 0))  # Observed outcomes using 0's and 1's
  resid.ord<-obs - predict(object = mod.fit.ord, type = "probs")
  head(resid.ord)
  
  # Using the lrm() function in the Design package
  library(package = Design) 
  library(package = rms)
  mod.fit.ord2<-lrm(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat)
  class(mod.fit.ord2)
  methods(class = lrm)  #Note: no summary()
  mod.fit.ord2  # Notice that beta^_10 are beta^_20 negative of what they should be
  attributes(mod.fit.ord2)


#
