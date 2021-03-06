#' mymlnorm
#'
#' @param x
#' @param mu
#' @param sig
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun mymlnorm(x=c(10,12,13,15,12,11,10),mu=seq(8,15,length=1000),sig=seq(.1,4,length=1000),lwd=2,labcex=1)
mymlnorm=function(x,mu,sig,...){
  nmu=length(mu)
  nsig=length(sig)
  n=length(x)
  zz=c()
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j])
    y=apply(z,2,sum)

    zz=cbind(zz,y)

  }
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),...)
  mlx=round(mean(x),2)
  mly=round(sqrt((n-1)/n)*sd(x),2)
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  muest=mu[coord[1]]
  sigest=sig[coord[2]]

  abline(v=muest, h=sigest)
  return(list(x=x,coord=coord,maxl=maxl))
}
