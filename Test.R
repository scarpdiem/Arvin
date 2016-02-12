x = faithful$waiting
dens = densityMclust(x)
summary(dens)
summary(dens, parameters = TRUE)
plot(dens, what = "BIC")
plot(dens)
plot(dens, x)

x = as.matrix(faithful)
dens = densityMclust(x)
summary(dens)
summary(dens, parameters = TRUE)
plot(dens)
plot(dens, x, what = "density", col = "cadetblue", drawlabels = FALSE, pch = 20,
     levels = quantile(dens$density, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
plot(dens, x, what = "density", col = "grey", 
     points.col = mclust.options("classPlotColors")[dens$classification], 
     pch = dens$classification)
plot(dens, what = "density", type = "image", col = topo.colors(50))
plot(dens, what = "density", type = "persp")

x = iris[,1:4]
dens = densityMclust(x)
summary(dens, parameters = TRUE)
plot(dens)
plot(dens, x, what = "density", col = "cadetblue", drawlabels = FALSE,
     levels = quantile(dens$density, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
plot(dens, what = "density", type = "image", col = gray.colors(50))
plot(dens, what = "density", type = "persp", col = gray(0.5))
stop("arvin lucien")
########Arvin Lucien###########

library(gmm)
n = 500
phi<-c(.2,.7)
thet <- 0
sd <- .2
x <- matrix(arima.sim(n=n,list(order=c(2,0,1),ar=phi,ma=thet,sd=sd)),ncol=1)
y <- x[7:n]
ym1 <- x[6:(n-1)]
ym2 <- x[5:(n-2)]

H <- cbind(x[4:(n-3)], x[3:(n-4)], x[2:(n-5)], x[1:(n-6)])
g <- y ~ ym1 + ym2
x <- H
t0 <- c(0,.5,.5)

res <- gel(g, x, t0)

coef(res)
coef(res, lambda = TRUE)
###################
res <- gmm(g, x)
coef(res)

stop("quantile prob")
data(mtcars)
x<-scale(mtcars$mpg)
qqnorm(x,cex=0.7,asp=1)
abline(0,1)

stop("Quantile Prob")
xseq<-dens$x;
pdf<-Gmm(miu,sigma,alpha,xseq)
plot(xseq,dens$y,type = "l",col='forestgreen')
lines(xseq,pdf,type = "l",col='orange')