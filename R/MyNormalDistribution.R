#' Norm Prob Function
#'
#' @description Will show a shaded normal probability curve
#'
#' @param mu is mean
#' @param sigma is standard deviation
#' @param a is q value
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun myncurve(mu=10,sigma=5, a=6)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu-3*sigma,a,length=1000)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Blue")
  prob=pnorm(a,mu,sigma)
  prob=round(prob,4)
  text(mu,dnorm((mu),mu,sigma)/2, paste("Area =",prob,sep=""))
  return(list(prob))

}
