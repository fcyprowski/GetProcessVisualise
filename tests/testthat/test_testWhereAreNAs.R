library(getProcessVisualise)
context('testWhereAreNAs')

data("iris")

test_that('testWhereAreNAs is giving the right errors', {
  expect_identical(testWhereAreNAs(iris), iris)
  iris2 = iris
  iris2$Sepal.Length[5:10] = NA
  expect_error(testWhereAreNAs(iris2), 'Column: Sepal.Length. NAs found on indexes: 5, 6, 7, 8, 9, 10')
  iris2$Petal.Length[1:2] = NA
  expect_error(testWhereAreNAs(iris2),
               'Column: Sepal.Length. NAs found on indexes: 5, 6, 7, 8, 9, 10;\nColumn: Petal.Length. NAs found on indexes: 1, 2')
})