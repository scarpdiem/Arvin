Gmm<-function(mean,sig,alpha,x)
{
  if(length(mean)!=length(sig) || length(mean)!=length(alpha)){
    return("parameter error")
  }
  p=rep(0,length(x))
  for(j in 1:length(x)){
    for(i in 1:length(mean)){
      p[j]<-p[j]+alpha[i]*dnorm(x[j],mean[i],sig[i])
    }
  }
  return(p);
}

pGmm<-function(x,mean,sig,alpha)
{
  if(length(mean)!=length(sig) || length(mean)!=length(alpha)){
    return("parameter error")
  }
  p=rep(0,length(x))
  for(j in 1:length(x)){
    for(i in 1:length(mean)){
      p[j]<-p[j]+alpha[i]*pnorm(x[j],mean[i],sig[i])
    }
  }
  return(p);
}






