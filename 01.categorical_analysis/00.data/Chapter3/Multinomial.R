#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  3-18-11                                                    #
# PURPOSE: Simulate data from a multinomial distribution            #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Simulate one set of observations

  set.seed(2195)  # Set a seed to be able to reproduce the sample
  pi.j<-c(0.25, 0.35, 0.2, 0.1, 0.1)
  n.j<-rmultinom(n = 1, size = 1000, prob = pi.j)
  data.frame(n.j, pihat.j = n.j/1000, pi.j)
  
  # m sets of n = 1000 observations
  set.seed(9182)
  n.j<-rmultinom(n = 5, size = 1000, prob = pi.j)
  n.j
  n.j/1000
  

#####################################################################
# Simulate m sets of observations

  set.seed(7812)
  save2<-rmultinom(n = 1000, size = 1, prob = c(0.25, 0.35, 0.2, 0.1, 0.1))
  save2[1:5,1:3]  # Each column is one set of observations from a n = 1 multinomial
  rowMeans(save2)
 
 
#####################################################################
# Probability mass function evaluated
  
   # Simple example with obvious result of 0.25 - P(n_1 = 1, n_2 = 0, n_3 = 0, n_4 = 0, n_5 = 0)
   dmultinom(x = c(1,0,0,0,0), size = NULL, prob = c(0.25, 0.35, 0.2, 0.1, 0.1))
   
   # Example of finding P(n_1 = 1, n_2 = 0, n_3 = 0, n_4 = 0, n_5 = 1)
   dmultinom(x = c(1,0,0,0,1), size = NULL, prob = c(0.25, 0.35, 0.2, 0.1, 0.1))


#####################################################################
# Simulate n_ij for a 2x3 contingency table

  # 1 multinomial distribution
  #  Probabilities entered by column for array()
  pi.ij<-c(0.2, 0.3, 0.2, 0.1, 0.1, 0.1)  # pi_ij
  pi.table<-array(data = pi.ij, dim = c(2,3), dimnames = list(X = 1:2, Y = 1:3))
  pi.table

  set.seed(9812)
  save<-rmultinom(n = 1, size = 1000, prob = pi.ij)
  save

  c.table1<-array(data = save, dim = c(2,3), dimnames = list(X = 1:2, Y = 1:3))
  c.table1
  c.table1/sum(c.table1)


  # I multinomial distributions
  pi.cond<-pi.table/rowSums(pi.table)
  pi.cond  # pi_j|i
 
  set.seed(8111)
  save1<-rmultinom(n = 1, size = 400, prob = pi.cond[1,])
  save2<-rmultinom(n = 1, size = 600, prob = pi.cond[2,])

  c.table2<-array(data = c(save1[1], save2[1], save1[2], save2[2], save1[3], save2[3]),
    dim = c(2,3), dimnames = list(X = 1:2, Y = 1:3))
  c.table2

  rowSums(c.table2)
  c.table2/rowSums(c.table2)

  round(c.table1/rowSums(c.table1),4)


#####################################################################
# Simulate n_ij under independence for a 2x3 contingency table

  # 1 multinomial under independence
  pi.i<-rowSums(pi.table)
  pi.j<-colSums(pi.table)
  pi.ij.ind<-pi.i%o%pi.j  # Quick way to find pi_i+ * pi_+j
  pi.ij.ind

  set.seed(9218)
  save.ind<-rmultinom(n = 1, size = 1000, prob = pi.ij.ind)
  save.ind

  c.table1.ind<-array(data = save.ind, dim = c(2,3), dimnames = list(X = 1:2, Y = 1:3))
  c.table1.ind/sum(c.table1.ind)  # pi^_ij is similar to pi^_i+ * pi^_+j

  # Using methods found later in the chapter
  chisq.test(x = c.table1.ind, correct = FALSE)  # Do not reject independence


  # I multinomials under independence
  set.seed(7718)
  save1.ind<-rmultinom(n = 1, size = 400, prob = pi.j)
  save2.ind<-rmultinom(n = 1, size = 600, prob = pi.j)

  c.table2.ind<-array(data = c(save1.ind[1], save2.ind[1], save1.ind[2], save2.ind[2], save1.ind[3], save2.ind[3]),
    dim = c(2,3), dimnames = list(X = 1:2, Y = 1:3))
  c.table2.ind

  rowSums(c.table2.ind)
  c.table2.ind/rowSums(c.table2.ind)  # pi^_j|1 is similar to pi^_j|2
  chisq.test(x = c.table2.ind, correct = FALSE)  # Do not reject independence


















#
