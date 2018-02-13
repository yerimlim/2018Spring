###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  11-30-10                                                         #
# PURPOSE: True confidence level for Wald interval for pi1 - pi2          #
# NOTES: Need to install rgl package for the 3D plots                     #
###########################################################################


# Initial settings
alpha<-0.05
pi1<-0.2
pi2<-0.4
n1<-10
n2<-10
numb.bin.samples<-1000  # Number of binomial samples


###########################################################################
# Estimated true confidence level

  # Simulate w1 and w2
  set.seed(2349)
  w1<-rbinom(n = numb.bin.samples, size = n1, prob = pi1)
  w2<-rbinom(n = numb.bin.samples, size = n2, prob = pi2)

  pi.hat1<-w1/n1
  pi.hat2<-w2/n2

  # Wald
  var.wald<-pi.hat1*(1-pi.hat1) / n1 + pi.hat2*(1-pi.hat2) / n2
  lower<-pi.hat1 - pi.hat2 - qnorm(p = 1-alpha/2) * sqrt(var.wald)
  upper<-pi.hat1 - pi.hat2 + qnorm(p = 1-alpha/2) * sqrt(var.wald)

  # Intervals 1-5
  data.frame(w1, w2, lower, upper)[1:5,]

  # Calculate estimated true confidence level
  save<-ifelse(test = pi1-pi2 > lower,
               yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
  save[1:5]
  true.conf<-mean(save)
  round(true.conf,4)

  # Agresti-Caffo
  pi.tilde1<-(w1+1)/(n1+2)
  pi.tilde2<-(w2+1)/(n2+2)
  var.AC<-pi.tilde1*(1-pi.tilde1) / (n1+2) + pi.tilde2*(1-pi.tilde2) / (n2+2)
  lower.AC<-pi.tilde1 - pi.tilde2 - qnorm(p = 1-alpha/2) * sqrt(var.AC)
  upper.AC<-pi.tilde1 - pi.tilde2 + qnorm(p = 1-alpha/2) * sqrt(var.AC)
  save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                  yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), no = 0)
  save.AC[1:10]
  true.conf.AC<-mean(save.AC)
  round(true.conf.AC,4)



###########################################################################
# Estimated true confidence level holding pi2 fixed at 0.3

  numb.bin.samples<-10000  # Number of binomial samples - changed to reduce simulation variability (makes plot look nicer)

  pi1seq<-seq(from = 0.001, to = 0.999, by = 0.0005)
  # pi1seq<-0.2  # Testing
  # pi1seq<-seq(from = 0.1, to = 0.9, by = 0.1)  # Testing

  # Save true confidence levels in a matrix
  save.true.conf<-matrix(data = NA, nrow = length(pi1seq), ncol = 3)

  # Create counter for the loop
  counter<-1

  set.seed(2114)
  # Loop over each pi1 that the true confidence level is calculated on
  for(pi1 in pi1seq) {
   
    w1<-rbinom(n = numb.bin.samples, size = n1, prob = pi1)
    w2<-rbinom(n = numb.bin.samples, size = n2, prob = pi2)

    pi.hat1<-w1/n1
    pi.hat2<-w2/n2

    # Wald
    lower<-pi.hat1 - pi.hat2 - qnorm(p = 1-alpha/2) *
      sqrt(pi.hat1*(1-pi.hat1) / n1 + pi.hat2*(1-pi.hat2) / n2)
    upper<-pi.hat1 - pi.hat2 + qnorm(p = 1-alpha/2) *
      sqrt(pi.hat1*(1-pi.hat1) / n1 + pi.hat2*(1-pi.hat2) / n2)
    save<-ifelse(test = pi1-pi2 > lower,
                 yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
    wald<-mean(save)

    # Agresti-Caffo
    pi.tilde1<-(w1+1)/(n1+2)
    pi.tilde2<-(w2+1)/(n2+2)
    lower.AC<-pi.tilde1 - pi.tilde2 - qnorm(p = 1-alpha/2) *
            sqrt(pi.tilde1*(1-pi.tilde1) / (n1+2) +
              pi.tilde2*(1-pi.tilde2) / (n2+2))
    upper.AC<-pi.tilde1 - pi.tilde2 + qnorm(p = 1-alpha/2) *
            sqrt(pi.tilde1*(1-pi.tilde1) / (n1+2) +
              pi.tilde2*(1-pi.tilde2) / (n2+2))
    save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                    yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), no = 0)
    AC<-mean(save.AC)
  
    save.true.conf[counter,]<-c(pi1, wald, AC)
    counter<-counter+1
  }
  
  # Plot
  x11(width = 7, height = 6, pointsize = 12)
  plot(x = save.true.conf[,1], y = save.true.conf[,2], xlab = expression(pi[1]),
    ylab = "Estimated true confidence level", type = "l", ylim = c(0.85,1), lty = "solid", col = "blue")
  lines(x = save.true.conf[,1], y = save.true.conf[,3], lty = "dashed", col = "red")
  abline(h = 1-alpha, lty = "dotted")
  legend(x = 0.1, y = 0.88, legend = c("Wald", "Agresti-Caffo"), lty = c("solid", "dashed"),
    bty = "n", col = c("blue", "red"))



###########################################################################
# True confidence level

  # All possible combinations of w1 and w2
  w.all<-expand.grid(w1 = 0:n1, w2 = 0:n2)
  
  # All possible combinations of pi^_1 and pi^_2
  pi.hat1<-(0:n1)/n1
  pi.hat2<-(0:n2)/n2
  pi.hat.all<-expand.grid(pi.hat1 = pi.hat1, pi.hat2 = pi.hat2)
 
  # Find joint probability for w1 and w2
  prob.w1<-dbinom(x = 0:n1, size = n1, prob = pi1)
  prob.w2<-dbinom(x = 0:n2, size = n2, prob = pi2)
  prob.all<-expand.grid(prob.w1 = prob.w1, prob.w2 = prob.w2)
  pmf<-prob.all$prob.w1*prob.all$prob.w2
  
  # Joint probability of observing w1 and w2 (i.e., P(W1 = w1, W2 = w2))
  head(data.frame(w.all, pmf = round(pmf,4)))
  tail(data.frame(w.all, pmf = round(pmf,4)))
  
  # Wald
  var.wald<-pi.hat.all[,1]*(1-pi.hat.all[,1]) / n1 + pi.hat.all[,2]*(1-pi.hat.all[,2]) / n2
  lower<-pi.hat.all[,1] - pi.hat.all[,2] - qnorm(p = 1-alpha/2) * sqrt(var.wald)
  upper<-pi.hat.all[,1] - pi.hat.all[,2] + qnorm(p = 1-alpha/2) * sqrt(var.wald)
  save<-ifelse(test = pi1-pi2 > lower,
               yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
  sum(save*pmf)
  data.frame(w.all, round(data.frame(pmf, lower, upper),4), save)[1:15,] #Example
  
  # Agresti-Caffo
  pi1tilde<-(0:n1+1)/(n1+2)
  pi2tilde<-(0:n2+1)/(n2+2)
  pi.all.tilde<-expand.grid(pi1tilde = pi1tilde, pi2tilde = pi2tilde)
  var.ac<-pi.all.tilde[,1]*(1-pi.all.tilde[,1]) / (n1+2) +
          pi.all.tilde[,2]*(1-pi.all.tilde[,2]) / (n2+2)
  lower.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] - qnorm(p = 1-alpha/2) * sqrt(var.ac)
  upper.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] + qnorm(p = 1-alpha/2) * sqrt(var.ac)
  save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                  yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), no = 0)
  sum(save.AC*pmf)
  data.frame(w.all, round(data.frame(pmf, lower, upper),4), save)[1:15,]  #Example

  
  
###########################################################################
# True confidence level holding pi2 fixed
 
  pi1seq<-seq(from = 0.001, to = 0.999, by = 0.0005)
  # pi1seq<-0.2  # Testing
  # pi1seq<-seq(from = 0.1, to = 0.9, by = 0.1)  # Testing

  # Save true confidence levels in a matrix
  save.true.conf<-matrix(data = NA, nrow = length(pi1seq), ncol = 3)

  # Create counter for the loop
  counter<-1

  # All possible combinations of w1 and w2
  w.all<-expand.grid(w1 = 0:n1, w2 = 0:n2)

  # All possible combinations of pi^_1 and pi^_2
  pi.hat1<-0:n1/n1
  pi.hat2<-0:n2/n2
  pi.hat.all<-expand.grid(pi.hat1 = pi.hat1, pi.hat2 = pi.hat2)
  
  # Wald
  lower<-pi.hat.all[,1] - pi.hat.all[,2] - qnorm(p = 1-alpha/2) * 
         sqrt(pi.hat.all[,1]*(1-pi.hat.all[,1]) / n1 + pi.hat.all[,2]*(1-pi.hat.all[,2]) / n2)
  upper<-pi.hat.all[,1] - pi.hat.all[,2] + qnorm(p = 1-alpha/2) * 
         sqrt(pi.hat.all[,1]*(1-pi.hat.all[,1]) / n1 + pi.hat.all[,2]*(1-pi.hat.all[,2]) / n2)

  # Agresti-Caffo
  pi1tilde<-(0:n1+1)/(n1+2)
  pi2tilde<-(0:n2+1)/(n2+2)
  pi.all.tilde<-expand.grid(pi1tilde = pi1tilde, pi2tilde = pi2tilde)
  lower.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] - qnorm(p = 1-alpha/2) *
            sqrt(pi.all.tilde[,1]*(1-pi.all.tilde[,1]) / (n1+2) +
              pi.all.tilde[,2]*(1-pi.all.tilde[,2]) / (n2+2))
  upper.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] + qnorm(p = 1-alpha/2) *
            sqrt(pi.all.tilde[,1]*(1-pi.all.tilde[,1]) / (n1+2) +
              pi.all.tilde[,2]*(1-pi.all.tilde[,2]) / (n2+2))


  # Loop over each pi1 that the true confidence level is calculated on
  for(pi1 in pi1seq) {

    # Find joint probability for w1 and w2
    prob.w1<-dbinom(x = 0:n1, size = n1, prob = pi1)
    prob.w2<-dbinom(x = 0:n2, size = n2, prob = pi2)
    prob.all<-expand.grid(prob.w1 = prob.w1, prob.w2 = prob.w2)
    pmf<-prob.all$prob.w1*prob.all$prob.w2
   
    # Wald
    save<-ifelse(test = pi1-pi2 > lower,
                 yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
    wald<-sum(save*pmf)

    # Agresti-Caffo
    save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                    yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), no = 0)
    AC<-sum(save.AC*pmf)
  
    save.true.conf[counter,]<-c(pi1, wald, AC)
    counter<-counter+1
  }
  
  
  # Plot
  x11(width = 7, height = 6, pointsize = 12)
  # pdf(file = "c:\\figures\\Figure1.4color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  plot(x = save.true.conf[,1], y = save.true.conf[,2], xlab = expression(pi[1]),
    ylab = "True confidence level", type = "l", ylim = c(0.85,1), lty = "solid", col = "blue")
  lines(x = save.true.conf[,1], y = save.true.conf[,3], lty = "dashed", col = "red")
  abline(h = 1-alpha, lty = "dotted")
  legend(x = 0.1, y = 0.88, legend = c("Wald", "Agresti-Caffo"), lty = c("solid", "dashed"),
    bty = "n", col = c("blue", "red"))
  # dev.off()  # Create plot for book

  # Black-and-white version of plot
  # pdf(file = "c:\\figures\\Figure1.4BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
  plot(x = save.true.conf[,1], y = save.true.conf[,2], xlab = expression(pi[1]),
    ylab = "True confidence level", type = "l", ylim = c(0.85,1), lty = "solid", col = "black")
  lines(x = save.true.conf[,1], y = save.true.conf[,3], lty = "dashed", col = "black")
  abline(h = 1-alpha, lty = "dotted")
  legend(x = 0.1, y = 0.88, legend = c("Wald", "Agresti-Caffo"), lty = c("solid", "dashed"),
    bty = "n", col = c("black", "black"))
  # dev.off()  # Create plot for book



###########################################################################
# 3D plot of the true confidence level
#   NOTE: This code can take a significant amount of time to run. We recommend
#         using the test cases of 0.1 to 0.9 by 0.1 to obtain an estimate of how
#         long it should run for the 0.001 to 0.999 by 0.0005 case. 

  # Find start time
  start.time<-proc.time()

  pi1seq<-seq(from = 0.001, to = 0.999, by = 0.0025)  # using a smaller "by" argument value can lead to slow rendering when rotating plots
  # pi1seq<-seq(from = 0.1, to = 0.9, by = 0.1)  # Testing

  pi2seq<-seq(from = 0.001, to = 0.999, by = 0.0025)
  # pi2seq<-seq(from = 0.1, to = 0.9, by = 0.1)  # Testing


  # Save true confidence levels in a matrix
  save.true.conf<-matrix(data = NA, nrow = length(pi1seq)*length(pi2seq), ncol = 4)

  # Create counter for the loop
  counter<-1

  # All possible combinations of w1 and w2
  w.all<-expand.grid(w1 = 0:n1, w2 = 0:n2)


  # All possible combinations of pi^_1 and pi^_2
  pi.hat1<-0:n1/n1
  pi.hat2<-0:n2/n2
  pi.hat.all<-expand.grid(pi.hat1 = pi.hat1, pi.hat2 = pi.hat2)
  
  # Wald
  lower<-pi.hat.all[,1] - pi.hat.all[,2] - qnorm(p = 1-alpha/2) * 
         sqrt(pi.hat.all[,1]*(1-pi.hat.all[,1]) / n1 + pi.hat.all[,2]*(1-pi.hat.all[,2]) / n2)
  upper<-pi.hat.all[,1] - pi.hat.all[,2] + qnorm(p = 1-alpha/2) * 
         sqrt(pi.hat.all[,1]*(1-pi.hat.all[,1]) / n1 + pi.hat.all[,2]*(1-pi.hat.all[,2]) / n2)

  # Agresti-Caffo
  pi1tilde<-(0:n1+1)/(n1+2)
  pi2tilde<-(0:n2+1)/(n2+2)
  pi.all.tilde<-expand.grid(pi1tilde = pi1tilde, pi2tilde = pi2tilde)
  lower.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] - qnorm(p = 1-alpha/2) *
            sqrt(pi.all.tilde[,1]*(1-pi.all.tilde[,1]) / (n1+2) +
              pi.all.tilde[,2]*(1-pi.all.tilde[,2]) / (n2+2))
  upper.AC<-pi.all.tilde[,1] - pi.all.tilde[,2] + qnorm(p = 1-alpha/2) *
            sqrt(pi.all.tilde[,1]*(1-pi.all.tilde[,1]) / (n1+2) +
              pi.all.tilde[,2]*(1-pi.all.tilde[,2]) / (n2+2))


  # Loop over each pi1 and pi2 that the true confidence level is calculated on
  for(pi1 in pi1seq) {
    for(pi2 in pi2seq) {

      # Find joint probability for w1 and w2
      prob.w1<-dbinom(x = 0:n1, size = n1, prob = pi1)
      prob.w2<-dbinom(x = 0:n2, size = n2, prob = pi2)
      prob.all<-expand.grid(prob.w1 = prob.w1, prob.w2 = prob.w2)
      pmf<-prob.all$prob.w1*prob.all$prob.w2
   
      # Wald
      save<-ifelse(test = pi1-pi2 > lower,
                   yes = ifelse(test = pi1-pi2 < upper, yes = 1, no = 0), no = 0)
      wald<-sum(save*pmf)

      # Agresti-Caffo
      save.AC<-ifelse(test = pi1-pi2 > lower.AC,
                      yes = ifelse(test = pi1-pi2 < upper.AC, yes = 1, no = 0), no = 0)
      AC<-sum(save.AC*pmf)
  
      save.true.conf[counter,]<-c(pi1, pi2, wald, AC)
      counter<-counter+1
    }
    print(pi1)
  }
  
  # Find end time and total time elapsed
  end.time<-proc.time()
  save.time<-end.time-start.time
  cat("\n Number of minutes running:", save.time[3]/60, "\n \n")

  # Write file out with results to save for later (if needed)
  # write.table(x = save.true.conf, file = "c:\\chris\\save.true.conf.txt", quote = FALSE, row.names = FALSE)
  # save.true.conf<-read.table(file = "c:\\chris\\save.true.conf.txt", header = TRUE)
  
  # 3D plot package
  library(rgl) 
  
  # Wald plot with plane at 0.95
  open3d()
  persp3d(x = pi1seq, y = pi2seq, z = save.true.conf[,3], xlim = c(0,1), ylim =
      c(0,1), zlim = c(0.85, 1), xlab = "pi1", ylab = "pi2", zlab = "True confidence level", ticktype = "detailed", col="red")
  # grid3d(side = c("x-", "y-", "z"), col = "lightgray")
  true.conf<-data.frame(x = c(0,0,0.1,0.1), y = c(0,0.1,0,0.1), z = c(0.95, 
      0.95, 0.95, 0.95))
  persp3d(x = c(0,1), y = c(0,1), z = matrix(data = c(0.95,0.95, 0.95, 
      0.95), nrow = 2, ncol = 2), add = TRUE, col = "green")

  # AC plot with plane at 0.95
  open3d()
  persp3d(x = pi1seq, y = pi2seq, z = save.true.conf[,4], xlim = c(0,1), ylim =
      c(0,1), zlim = c(0.85, 1), xlab = "pi1", ylab = "pi2", zlab = "True confidence level",
      aspects = 10, ticktype = "detailed", col="red")
  # grid3d(side = c("x-", "y-", "z"), col = "lightgray")
  true.conf<-data.frame(x = c(0,0,0.1,0.1), y = c(0,0.1,0,0.1), z = c(0.95, 
      0.95, 0.95, 0.95))
  persp3d(x = c(0,1), y = c(0,1), z = matrix(data = c(0.95,0.95, 0.95, 
      0.95), nrow = 2, ncol = 2), add = TRUE, col = "green")


  # The zlim option in persp3d does not fix the axis limits like it should. Below is a fix
  #  to the problem in order to get both the Wald and AC plots on the same scale.
  test.true.conf.wald<-ifelse(test = save.true.conf[,3]<0.85, yes = NA, no = 1)
  save.true.conf.wald2<-test.true.conf.wald*save.true.conf[,3]  # Put NA's in vector if true confidence level is < 0.85
  open3d()
  persp3d(x = pi1seq, y = pi2seq, z = save.true.conf.wald2, xlim = c(0,1), ylim =
      c(0,1), xlab = "pi1", ylab = "pi2",  zlim = c(0.85, 1),
      zlab = "True confidence level", ticktype = "detailed", col="red")
  true.conf<-data.frame(x = c(0,0,0.1,0.1), y = c(0,0.1,0,0.1), z = c(0.95, 
      0.95, 0.95, 0.95))
  persp3d(x = c(0,1), y = c(0,1), z = matrix(data = c(0.95,0.95, 0.95, 
      0.95), nrow = 2, ncol = 2), add = TRUE, col = "green")
 
  # Note that there is not a par(mfrow = c(1,2)) type of option yet in the rgl package, so I used a
  #  graphics editor to get the two plots side-by-side. Also, I used a print screen to obtain
  #  the plot graphics to put into the graphics editor.  
  
  
           

#