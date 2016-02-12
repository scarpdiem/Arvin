rm(list = ls())
miu1 <- 3
miu2 <- -2
sigma1 <- 1
sigma2 <- 2
alpha1 <- 0.4
alpha2 <- 0.6
# 生成两种高斯分布的样本
n <- 5000
x <- rep(0,n)
n1 <- floor(n*alpha1)
n2 <- n - n1
x[1:n1]<-rnorm(n1)*sigma1 + miu1
x[(n1+1):n]<-rnorm(n2)*sigma2 + miu2

# hist(x,freq=F)
# lines(density(x),col='red')

source('C:/Users/Administrator/Desktop/Spot DataSet/Gmm.R')
source('C:/Users/Administrator/Desktop/Spot DataSet/EM.R')

xsample=seq(min(x),max(x),by=0.001)
p<-Gmm(miu,sigma,alpha,xsample)
plot(xsample,p,type = "l")
lines(density(x),col='red')

ks<-ks.test(x,"pGmm",miu,sigma,alpha,alternative="two.side")
print(ks)

stop("sample")



