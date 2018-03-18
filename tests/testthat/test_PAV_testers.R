library(getProcessVisualise)
context("testing functions are ok")

data("iris")

test_that('expecting data frame works fine', {
  expect_error(expectDataFrame("foo"), "Object is not a data.frame!")
  expect_identical(expectDataFrame(iris), iris)
})


test_that('raiseErrorIfFalse is ok', {
  expect_error(FALSE %>%
    raiseErrorIfFalse(message = 'Wrong'), 'Wrong')
  expect_identical(TRUE %>%
                raiseErrorIfFalse(data = iris), iris)
})

test_that('testIfThereAreNAs works fine', {
  expect_identical(testIfThereAreNAs(iris), iris)
  expect_error(testIfThereAreNAs(iris %>%
                                   dplyr::mutate(Sepal.Length = NA)),
               "NA's in data!")

})

test_that('testIfNamesAreThere works fine', {
  expect_identical(testIfNamesAreThere(iris, "Sepal.Length"), iris)
  expect_error(testIfNamesAreThere(iris, "dupa"),  "These columns are not in the data.frame: dupa")
})

test_that('testIfTypesAreCorrect', {
  definately.good.types = c(is.numeric, is.numeric, is.numeric, is.numeric)
  good.types = c(definately.good.types, is.factor)
  incomplete.types = good.types[1:2]
  bad.types = definately.good.types %>%
    c(is.character)
  expect_error(testIfTypesAreCorrect(iris, incomplete.types), "is not TRUE")
  expect_identical(testIfTypesAreCorrect(iris, good.types), iris)
  expect_error(testIfTypesAreCorrect(iris, bad.types), "Column Species have wrong data type")
})