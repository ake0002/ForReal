#' Reading FIREDAM Data
#'
#' @description for fire damage data
#'
#' @param fire is the data variable
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun data(fire)
data =function(fire){
  fire <- read.csv("FIREDAM.csv")
  return(head(fire))
}
