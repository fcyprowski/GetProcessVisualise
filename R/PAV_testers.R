require(testthat)
require(dplyr)
require(magrittr)
require(purrr)

raiseErrorIfFalse = function(lgl, message, message.function = paste) {
  lgl %>%
    when(. ~ .,
       ~stop(message %>%
               message.function))
}

testIfNamesAreThere = function(data, expected.columns) {
  names(data) %>%
    intersect(expected.columns) %>%
    length() %>%
    magrittr::equals(length(expected.columns)) %>%
    raiseErrorIfFalse(
      "These columns are not in the data.frame:",
      partial(paste, setdiff(expected.columns, names(data)), .first = F)
    )
}

testIfThereAreNAs = function(data) {
  data %>%
    nrow() %>%
    magrittr::equals(data %>%
             na.omit() %>%
             nrow()) %>%
    raiseErrorIfFalse(
      "NA's in data!"
    )
}

testIfTypesAreCorrect = function(data, lTypes) {
  # lTypes - list of functions like "is.numeric" etc.
  data %>%
    map(~invoke(lTypes, .))

}
