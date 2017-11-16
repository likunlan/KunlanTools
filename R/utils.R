#' @title mymean file
#' @description calculate the mean of a input vector
#' @param v a vector
#' @return the mean of the vector

mymean <- function(v) {
  assert(checkTRUE(length(v) != 0), checkNumeric(v, lower = 0.00, upper = 2.00, any.missing = FALSE))
   m <- round(sum(v)/length(v), digits = 3)
   checkNumeric(m, lower=0.00,upper = 2.50, any.missing = FALSE)
  return(m)
}
