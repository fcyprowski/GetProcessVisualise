#' @importFrom magrittr equals
#' @importFrom magrittr or
expectDataFrame = function(data) {
  data %>%
    when(is.data.frame(.) ~., ~stop("Object is not a data.frame!"))
}
is_longer_than = function(e1, e2) {
  length(e1) %>% is_greater_than(e2)
}
is_true = function(x){
  stopifnot(is.logical(x))
  x
}
is_date = function(x) { 
    class(x) %>% equals('Date') %>% or(
      is_pseudodate(x)
    )
}
is_pseudodate = function(x) {
  stringr::str_extract(x, "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    equals(x)
}