#' @importFrom purrr when
#' @importFrom dplyr intersect
#' @importFrom magrittr equals
#' @importFrom magrittr is_greater_than
#' @importFrom dplyr setdiff
#' @importFrom purrr partial
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom purrr discard

raiseErrorOrPassForward = function(lgl, data, ...) {
  lgl %>%
    when(. ~ data, ~stop(paste(...)))

}
#' Test column naming
#'
#' @param data data.frame you want to test
#' @param expected.columns character vector of expected column names
#'
#' @return same data.frame or raise error
#' @export testIfNamesAreThere
#'
#' @examples
testIfNamesAreThere = function(data, expected.columns) {
  names(data) %>%
    intersect(expected.columns) %>%
    length() %>%
    equals(length(expected.columns)) %>%
    raiseErrorIfNamesAreNotThere(data, expected.columns)
}
raiseErrorIfNamesAreNotThere = function(lgl, data, expected.columns) {
  raiseErrorOrPassForward(lgl, data,
                          "These columns are not in the data.frame:", 
                          setdiff(expected.columns, names(data)))
}

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
testIfThereAreNAs = function(data) {
  data %>%
    nrow() %>%
    equals(data %>%
             na.omit() %>%
             nrow()) %>%
    raiseErrorOrPassForward(
      data,
      "NA's in data!"
    )
}
#' Test if types of columns are ok
#'
#' @param data data.frame you want to check
#' @param lTypes list of logical condition for every column, e.g.
#' c(is.numeric, is.numeric)
#'
#' @return the same data or raise error
#' @export testIfTypesAreCorrect
#'
#' @examples
testIfTypesAreCorrect = function(data, lTypes) {
  stopifnot(length(data) == length(lTypes))
  data %>%
    expectDataFrame() %>%
    map2(lTypes, ~.y(.x)) %>%
    discard(is_true) -> wrong.types
  wrong.types %>%
    when(length(.) > 0 ~FALSE, TRUE) %>%
    raiseErrorOrPassForward(
      data, 
      names(wrong.types) %>%
        map(~paste("Column", ., "have wrong data type")) %>%
        paste(collapse = ', ')
    )
}
