m <- 2
n <- length(x)
miu <- runif(m)
sigma <- runif(m)
alpha <- runif(m)
prob <- matrix(rep(0,n*m),ncol=m)

for (step in 1:100){
  # E步骤
  for (j in 1:m){
    prob[,j]<- sapply(x,dnorm,miu[j],sigma[j])
  }
  sumprob <- rowSums(prob)
  prob<- prob/sumprob
  
  oldmiu <- miu
  oldsigma <- sigma
  oldalpha <- alpha
  
  # M步骤
  for (j in 1:m){
    p1 <- sum(prob[ ,j])
    p2 <- sum(prob[ ,j]*x)
    miu[j] <- p2/p1
    alpha[j] <- p1/n
    p3 <- sum(prob[ ,j]*(x-miu[j])^2)
    sigma[j] <- sqrt(p3/p1)
  }
  
  # 变化
  epsilo <- 1e-6
  if (sum(abs(miu-oldmiu))<epsilo &
      sum(abs(sigma-oldsigma))<epsilo &
      sum(abs(alpha-oldalpha))<epsilo) break
  cat('step',step,'miu',miu,'sigma',sigma,'alpha',alpha,'\n')
}


