library(testthat)
library(KunlanTools)

data('students')

check_observations_num <- function(){
  result = checkHeight(students.input = students, sex.specific = TRUE, print.statement = FALSE)
  if (nrow(students) == nrow(result)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_sex <- function(){
  students[,'height'] = apply(students, 1, function(r){
    r['height']=1.80
  })
  result1 = checkHeight(students.input = students, sex.specific = TRUE, print.statement = FALSE)
  result2 = checkHeight(students.input = students, sex.specific = FALSE, print.statement = FALSE)
  if (identical(result1[,'difference'],result2[,'difference'])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


test_that("Test that the output contains as many observations as the input",{
  expect_equal( check_observations_num(), TRUE)
})

test_that("Test if both options of sex.specific work equally",{
  expect_equal ( check_sex(),TRUE)
})




#3
#in checkHeight
 #assessing if input variables are present in input data frame
#if (! ("age" %in% colnames(students.input) )) stop("Variable <age> not found.")
#if (! ("weight" %in% colnames(students.input) )) stop("Variable <weight> not found.")
#if (! ("height" %in% colnames(students.input) )) stop("Variable <height> not found.")
#if (! ("sex" %in% colnames(students.input) )) stop("Variable <sex> not found.")
#if (! ("names" %in% colnames(students.input) )) stop("Variable <names> not found.")


#in test
# unit test if all required inputs are present in input data frame

test_that(desc = "throws error message if variable age not present in input", code = {
  test.data = students
  colnames(test.data)[which(names(test.data) == "age")] <- "test"
  expect_that(checkHeight(test.data), throws_error("Variable <age> not found."))
})

test_that(desc = "throws error message if variable age not present in input", code = {
  test.data = students
  colnames(test.data)[which(names(test.data) == "weight")] <- "test"
  expect_that(checkHeight(test.data), throws_error("Variable <weight> not found."))
})

test_that(desc = "throws error message if variable age not present in input", code = {
  test.data = students
  colnames(test.data)[which(names(test.data) == "height")] <- "test"
  expect_that(checkHeight(test.data), throws_error("Variable <height> not found."))
})

test_that(desc = "throws error message if variable age not present in input", code = {
  test.data = students
  colnames(test.data)[which(names(test.data) == "sex")] <- "test"
  expect_that(checkHeight(test.data), throws_error("Variable <sex> not found."))
})

test_that(desc = "throws error message if variable age not present in input", code = {
  test.data = students
  colnames(test.data)[which(names(test.data) == "names")] <- "test"
  expect_that(checkHeight(test.data), throws_error("Variable <names> not found."))
})

#4
test_that("Functioning of the argument check ", {
  #test if sex.specific is boolean
  expect_error(checkHeight(students, sex.specific = 5))
  #test if print.statemnet is boolean
  expect_error(checkHeight(students, print.statement = 5))
  #test if data frame is long enough
  expect_error(checkHeight(students[1:3]))
  #test if persons not in range are registered
  students_test <- students
  students_test[1,3] <- 5.80
  expect_error(checkHeight(students_test))
  #test if persons wrong gender is registered
  students_test <- students
  levels(students_test[,4]) <- c("F", "M", "C")
  expect_error(checkHeight(students_test))
  #test mean function
  expect_error(compareheight:::mean(NaN))
})
#Fehlermeldung kann mit substring

# test print.statement==TRUE
test_that("print checking", {
  expect_output(checkHeight(students, print.statement = TRUE), "Yippie, I calculated the mean differences!")
})
