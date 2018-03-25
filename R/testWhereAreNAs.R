#' @importFrom purrr keep
#' @importFrom purrr map2_chr

#' @title Test if you have NA's in the data.frame and if so - where
#'
#' @param data data.frame
#'
#' @return the same data.frame or raise error (exact positions of NAs)
#' @export testWhereAreNAs
#'
#' @examples
#' iris$Sepal.Length[5:10] = NA
#' testWhereAreNAs(iris)
testWhereAreNAs = function(data) {
  data %>%
    map(~which(is.na(.))) %>%
    keep(is_longer_than, 0) -> result
  
  result %>%
    when(length(.) == 0 ~TRUE, ~FALSE) %>%
    raiseErrorOrPassForward(
      data,
      tell_where_are_NAs(result)
    )
}
tell_where_are_NAs = function(result) {
  map2_chr(result, names(result), 
           ~paste0('Column: ', .y, '. NAs found on indexes: ', toString(.x))) %>%
    paste(collapse = ';\n')
}