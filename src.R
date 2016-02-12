rm(list = ls())
setwd("C:/Users/Administrator/Desktop/Spot DataSet/m1-xlarge.linux")
b=scan(file="us-east-1b.csv",sep=",")
# c=scan(file="us-east-1c.csv",sep=",")
# d=scan(file="us-east-1d.csv",sep=",")
# e=scan(file="us-east-1e.csv",sep=",")
x<-b
n<-length(x)
x<-x+1e-5*runif(n)
hist(x,freq=F,breaks=100)
lines(density(x),col='red')
dens<-density(x)

stop("us-east price")

b=scan(file="us-west-1b.csv",sep=",")
c=scan(file="us-west-1c.csv",sep=",")
stop("us-west")

a=scan(file="ap-northeast-1a.csv",sep=",")
c=scan(file="ap-northeast-1c.csv",sep=",")
stop("ap-northeast")



