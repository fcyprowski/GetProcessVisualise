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