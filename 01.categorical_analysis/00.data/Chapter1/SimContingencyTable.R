###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  11-22-10                                                         #
# PURPOSE: Simulate a contingency table                                   #
# NOTES:                                                                  #
###########################################################################

pi1<-0.2
pi2<-0.4
n1<-10
n2<-10

set.seed(8191)
w1<-rbinom(n = 1, size = n1, prob = pi1)
w2<-rbinom(n = 1, size = n2, prob = pi2)

c.table<-array(data = c(w1, w2, n1-w1, n2-w2), dim = c(2,2), dimnames = list(Group = c(1,2),
             Response = c(1, 2)))
c.table


###########################################################################
# Simulate 1,000 contingency tables

set.seed(8191)
w1<-rbinom(n = 1000, size = n1, prob = pi1)
w2<-rbinom(n = 1000, size = n2, prob = pi2)

# Table #1
c.table1<-array(data = c(w1[1], w2[1], n1-w1[1], n2-w2[1]), dim = c(2,2), dimnames = list(Group = c(1,2),
             Response = c(1, 2)))
c.table1
c.table1[1,1]  # w1
c.table1[1,2]  # n1-w1
c.table1[1,]  # w1 and n1-w1
sum(c.table1[1,])  # n1



# Table #5
c.table5<-array(data = c(w1[5], w2[5], n1-w1[5], n2-w2[5]), dim = c(2,2), dimnames = list(Group = c(1,2),
             Response = c(1, 2)))
c.table5

pihat1<-w1/n1
mean(pihat1)  # Close to pi1 as expected

pihat2<-w2/n2
mean(pihat2)  # Close to pi2 as expected









#

