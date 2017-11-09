#Add a welcome text
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my first R-package and thanks for using it!")}

#' an example function named 'checkheight'
#' Script for simple function that checks the difference in height from the sex-
#' specific mean or the overall mean height for each of the students in the given #'dataframe
#' Date: 02.11.2017
#' @author Kunlan Li <li.kunlan@campus.lmu.de>
#' @title checkHeight
#' @description Script for simple function that checks the difference in height     from the sex-specific mean or the overall mean height for each of the students
#' @importFrom stats filter
#' @export
#' @param students.input a input dataframe
#' @param  sex.specific logical value
#' @param r a vector
#' @return result of the computation
#' @example checkHeight(students,TRUE)
checkHeight = function(students.input, sex.specific = TRUE) {
  #define result.frame
     result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
     colnames(result.frame) = c("name", "difference")
  #calculate the mean height of man
  male.mean = mymean(students.input[students.input['sex']=='M',]['height']])
    #students.input %>%
    #filter(sex == "M") %>%
    #summarise(mean = mymean(height))
   #select(height)%>%mymean
  #calculate the mean height of female
  female.mean = mymean(students.input[students.input['sex']=='F',]['height']])
    #students.input %>%
    #filter(sex == "F") %>%
    #summarise(mean = mymean(height))
  #select(height)%>%mymean
  #calculate the  overall mean height
  overall.mean = mymean(students.input[,"height"])
    #students.input%>%
    #summarise(mean = mymean(height))
  #select(height)%>%mymean
  #if calculate the difference from the sex-specific
  if(sex.specific==TRUE){
  # use apply  to calculate the difference
  l = apply(students.input,1,function(r){
    if (r['sex'] == "F") {
      height.diff = 100*(as.numeric(r['height']) - female.mean)
    }
    else {
      height.diff = 100*(as.numeric(r['height']) - male.mean)
    }
  }
  )
  }
  #if calculate the difference from the overall mean height
  else{
    l = apply(students.input, 1, function(r){
      height.diff = 100*(as.numeric(r['height']) - overall.mean)
    }
      )
  }
  result.frame[, "name"]=students.input[,"name"]
  result.frame[, "difference"]=round(l/100,3)
  return(result.frame)
}

#library(dplyr)

#age = c(19, 22, 21, 23, 22, 20, 28, 25)
#weight = c(50, 75, 80, 56, 75, 58, 65, 82)
#height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
#sex = c("F", "M", "M", "F", "M", "F", "F", "M")

#students = data.frame(cbind(age, weight, height, sex))
#students = transform(students, age = as.numeric(as.character(age)))
#students = transform(students, height = as.numeric(as.character(height)))
#students = transform(students, weight = as.numeric(as.character(weight)))
#students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

checkHeight(students,TRUE)

