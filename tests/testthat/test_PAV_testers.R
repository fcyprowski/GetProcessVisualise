library(getProcessVisualise)
context("testing functions are ok")

data("iris")

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
