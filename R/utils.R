#' @title mymean file
#' @description calculate the mean of a input vector
#' @param v a vector
#' @return the mean of the vector

mymean <- function(v) {
  m <- round(sum(v)/length(v), digits = 3)
  return(m)
}
