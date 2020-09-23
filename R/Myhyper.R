#' Hypergeomtric Distribution
#'
#' @description Finds bargraph of hypergeometric distribution
#'
#' @param iter is number of iterations
#' @param N is the total number of marbles
#' @param r number of white marbles
#' @param n is the sample size
#'
#' @return puts out a bar graph
#' @export
#'
#' @examples
#' \dontrun myhyper(iter=1000, N=20, r=12, n=5)
myhyper=function(iter=100,N=20,r=12,n=5){

  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)

  succ=c()
  for( i in 1:iter){

    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)

    succ[i]=sum(sam.mat[,i])
  }

  succ.tab=table(factor(succ,levels=0:n))

  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}
