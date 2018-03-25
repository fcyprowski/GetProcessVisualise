library(getProcessVisualise)
context('testColumnsCosistency')

test_that('testColumnsCosistency is passing good data', {
  expect_identical(
    testColumnsCosistency(
      iris,
      c("Sepal.Length > 0", "Petal.Length > 0")),
    iris
  )
})
test_that('testColumnsCosistency is giving right error', {
  expect_error(testColumnsCosistency(iris, "Sepal.Length > 7"), 
               'Condition Sepal.Length > 7 not fulfilled.')
})