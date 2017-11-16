library(testthat)
library(KunlanTools)

data('students')

check_observations_num <- function(){
  data('students')
  result = checkHeight(students.input = students, sex.specific = TRUE, print.statement = FALSE)
  if (nrow(students) == nrow(result)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_sex <- function(){
  data('students')
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

prints_text()
test_that("Test that the output contains as many observations as the input",{
  expect_equal( check_observations_num(), TRUE)
})

test_that("Test if both options of sex.specific work equally",{
  expect_equal ( check_sex(),TRUE)
})


