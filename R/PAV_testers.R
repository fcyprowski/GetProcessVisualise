require(testthat)
require(dplyr)
require(magrittr)
require(purrr)

raiseErrorIfFalse = function(lgl, message, message.function = paste, data) {
  lgl %>%
    purrr::when(. ~ data,
       ~stop(message %>%
               message.function))
}

raiseErrorOrPassForward = function(lgl, message.function, data) {
  lgl %>%
    purrr::when(. ~ data,
                ~stop(message.function()))

}

testIfNamesAreThere = function(data, expected.columns) {
  names(data) %>%
    intersect(expected.columns) %>%
    length() %>%
    magrittr::equals(length(expected.columns)) %>%
    raiseErrorIfFalse(
      "These columns are not in the data.frame:",
      purrr::partial(paste, setdiff(expected.columns, names(data)), .first = F)
    )
}

testIfThereAreNAs = function(data) {
  data %>%
    nrow() %>%
    magrittr::equals(data %>%
             na.omit() %>%
             nrow()) %>%
    raiseErrorIfFalse(
      "NA's in data!",
      data = data
    )
}
testWhereAreNAs = function(data) {
  data %>%
    map(~which(is.na(.))) %>%
    keep(is_longer_than, 0) -> result

  result %>%
    when(length(.) == 0 ~TRUE, ~FALSE) %>%
    raiseErrorOrPassForward(
      message.function = partial(
        map2, result, names(result),
        ~paste('Column:', .y, 'NAs found on indexes:', toString(.x))
      ),
      data = data
    )
}

is_longer_than = function(e1, e2) {
  length(e1) %>% is_greater_than(e2)
}

testIfTypesAreCorrect = function(data, lTypes) {
  # lTypes - list of functions like "is.numeric" etc.
  data %>%
    purrr::map2(lTypes, ~.y(.x)) %>%
    map(~raiseErrorIfFalse(., message = 'Data type not as expected'))

}
