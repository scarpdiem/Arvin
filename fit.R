library(MASS)
x<-rexp(100,5)
res <- fitdistr(x, 'gamma')
print(res)
k<-ks.test(x, "pgamma",res[[1]][1],res[[1]][2])
print(k)
stop("Gamma distr")

x<-b
# t=sort(table(x),decreasing = TRUE)
pd=table(x)
cd=cumsum(pd)
p=as.vector(pd)
c=as.vector(cd)
axis=sort(unique(b),decreasing = FALSE);
plot(axis,c,type="l")
stop("cdf")

y=scan(file="Sample.txt",sep=" ")
u=sort(unique(y))
print(table(u))
print(table(y))
x<-u
k<-ks.test(x, "pnorm",80,6)
print(k)
stop("norm text")


