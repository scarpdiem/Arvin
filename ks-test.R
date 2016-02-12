source('C:/Users/Administrator/Desktop/Spot DataSet/Gmm.R')
# xseq<-seq(min(x), max(x), by=0.001)
xseq<-dens$x;
pdf<-Gmm(miu,sigma,alpha,xseq)
plot(xseq,dens$y,type = "l",col='forestgreen')
lines(xseq,pdf,type = "l",col='orange')

ks<-ks.test(x,"pGmm",miu,sigma,alpha,alternative="two.side")
print(ks)
stop("Gmm ks test")

library(MASS)
x <- rpois(100,5)
res <- fitdistr(x, 'gamma')
print(res)
k<-ks.test(x, "pgamma",res[[1]][1],res[[1]][2],exact = NULL)
print(k)
stop("Exp test")

y=scan(file="Sample.txt",sep=" ")
u=sort(unique(y))
print(table(u))
print(table(y))
x<-y
k<-ks.test(x, "pnorm",80,6)
print(k)
stop("norm test")
