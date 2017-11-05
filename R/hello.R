# height
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Script for simple function that checks the difference in height from the sex-
# specific mean or the overall mean height for each of the students in the given dataframe
# Date: 02.11.2017
# Author: likunlan

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("hello, my R world")
}

#' @importFrom stats filter
#' @export
checkHeight3 = function(students.input, sex.specific = TRUE) {
  #define result.frame
     result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
     colnames(result.frame) = c("name", "difference")
  #calculate the mean height of man
  male.mean = students.input %>%
    filter(sex == "M") %>%
    summarise(mean = mean(height))
  #calculate the mean height of female
  female.mean = students.input %>%
    filter(sex == "F") %>%
    summarise(mean = mean(height))
  #calculate the  overall mean height
  overall.mean = students.input%>%
    summarise(mean = mean(height))
  #if calculate the difference from the sex-specific
  if(sex.specific==TRUE){
  # use apply  to calculate the difference
  l = apply(students.input,1,function(r){
    if (r['sex'] == "F") {
      height.diff = 100*(as.numeric(r['height']) - female.mean$mean)
    }
    else {
      height.diff = 100*(as.numeric(r['height']) - male.mean$mean)
    }
  }
  )
  }
  #if calculate the difference from the overall mean height
  else{
    l = apply(students.input,1,function(r){
      height.diff = 100*(as.numeric(r['height']) - overall.mean$mean)
    }
      )
  }
  result.frame[, "name"]=students.input[,"name"]
  result.frame[, "difference"]=l
  return(result.frame)
}

