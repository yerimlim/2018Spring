n1<-10
y1<-c(3,5,6,6,7,10,13,15,18,22)
lfac1 <- lfactorial(y1)
sumlfac1 <- sum(lfac1)
sumy1 <- sum(y1)
mu <- c(1:25)
loglik1 <- -n1*mu + log(mu)*sumy1 - sumlfac1
mle1 <- sumy1/n1
mle1
loglikmax <- -n1*mle1 + log(mle1)*sumy1 - sumlfac1
loglik9 <- -n1*9 + log(9)*sumy1 - sumlfac1
loglik5 <- -n1*5 + log(5)*sumy1 - sumlfac1

# Likelihood Ratio Plot
x11(height=7,width=8,pointsize=12)
# pdf(file = "c:\\figures\\FigureB.4BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
curve(expr=-n1*x + log(x)*sumy1 - sumlfac1, from=3, to=25, ylim=c(-94,-34),xlab=expression(mu),ylab="Log Likelihood", cex.lab=1.5,cex.axis=1.5)

lines(x=c(mle1,mle1),y=c(loglikmax,-94),lty="dotted")
lines(x=c(9,9),y=c(loglik9,-94),lty="dotted")
lines(x=c(5,5),y=c(loglik5,-94),lty="dotted")
lines(x=c(2,8),y=c(loglik5-3*(-n1+(sumy1/5)),loglik5+3*(-n1+(sumy1/5))),lty="dashed")
lines(x=c(6,12),y=c(loglik9-3*(-n1+(sumy1/9)),loglik9+3*(-n1+(sumy1/9))),lty="dashed")
lines(x=c(7.5,13.5),y=c(loglikmax,loglikmax),lty="dashed")
text(x=4.5,y=-94,expression(mu==5),cex=1.5)
text(x=8.5,y=-94,expression(mu==9),cex=1.5)
text(x=mle1+.5,y=-93.5,expression(mu==hat(mu)),cex=1.5)
# dev.off()  # Create plot for book

