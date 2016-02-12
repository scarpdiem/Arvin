library(MASS)
x<-b
res <- fitdistr(x, 'normal')
ks<-ks.test(x,"pnorm",res[[1]][1],res[[1]][2])
print(res)
print(ks)
stop("ks-test finish")

h <-hist(x,breaks=50)
xfit <-seq(min(x), max(x), by=(max(x)-min(x))/50)
yfit <-dgamma(xfit,res[[1]][1],res[[1]][2])
# 区间概率修正
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)