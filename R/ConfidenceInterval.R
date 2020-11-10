#' Confidence Interval
#'
#' @description This function will find a confidence interval for the mean of a given set of data
#'
#' @param x data
#' @param confint confidence interval desired
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun myci(x,.95)
myci = function(x, confint=.95){
  ybar = mean(x)

  t=qt(1-(1-confint)/2,length(x)-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(length(x))
  ci[2]=mean(x)+t*sd(x)/sqrt(length(x))
  return(ci)
}
