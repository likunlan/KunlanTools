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
#' @param students.input a input dataframe
#' @param  sex.specific logical value
#' @param print.statement logical value
#' @param r a vector
#' @return result of the computation
#' @example checkHeight(students,TRUE)
#'  @export
#'  @import dplyr
#'  @import checkmate
checkHeight = function(students.input, sex.specific = TRUE, print.statement = FALSE){
  #Check if the variable that controls the sex specificity of the mean calculation is boolean
  checkmate::assertLogical(x = sex.specific)
  #Check if the variable that controls the print statement is boolean
  checkmate::assertLogical(x = print.statement)
  #Check if students.input is a data frame with a minimum of 4 rows and exactly 5 columns with the types c("numeric", "numeric", "numeric", "factor", "character") without any missing values
  checkmate::assertDataFrame(x = students.input, types = c("numeric", "numeric", "numeric", "factor", "character"), any.missing = FALSE, min.rows = 4, min.cols = 5)
  # Check if the 3rd column aka the height-column of the students data frame contains numerics from the interval [1.30, 2.40]
  checkmate::assertNumeric(x =students.input[,'height'] , lower = 1.30, upper = 2.40)
  #Check if the 4th aka the sex-column contains a factor variable with maximum two levels M and F
  checkmate::assertFactor(x=students.input[,"sex"], max.levels=2,levels=c("M", "F"))
  #define result.frame
  result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
  colnames(result.frame) = c("name", "difference")
  #calculate the mean height of man
  male.mean = mymean(students.input[students.input['sex']=='M',][['height']])
  #students.input %>%
  #filter(sex == "M") %>%
  #summarise(mean = mymean(height))
  #select(height)%>%mymean
  #calculate the mean height of female
  female.mean = mymean(students.input[students.input['sex']=='F',][['height']])
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
  result.frame[, "name"]=students.input[,"names"]
  result.frame[, "difference"]=round(l/100,3)
  #if print.statement = TRUE
  if(print.statement == TRUE){
    print("Yippie, I calculated the mean differences!")
  }

  return(result.frame)
}


#Frage 3-12
#students=read.csv(file="C:\\Users\\likunlan\\Desktop\\praktikum\\ex1\\students.csv")
#save(students,file='data/students.rda')
#devtools::use_data(students,overwrite = TRUE)
