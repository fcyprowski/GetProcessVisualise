#' @importFrom purrr detect_index
testColumnsCosistency = function(data, conditions) {
  other_than_data = partial(other_than_data_, data = data)
  conditions %>%
    map(~filter_(data, .)) -> result
  result  %>%
    keep(~other_than_data(.)) %>%
    when(length(.) == 0 ~TRUE, ~FALSE) %>%
    raiseErrorOrPassForward(
      data,
      paste0('Condition ', conditions[detect_index(result, other_than_data)], ' not fulfilled.') %>%
        paste(collapse = ';\n')
    )
    
}
other_than_data_ = function(results, data) {
  nrow(results) != nrow(data) 
}
conditions_that_data_fails_ = function() {
  
}
tell_where_data_fails = function(conditions, result) {
  paste0(
    'Condition ', 
    conditions[detect_index(result, other_than_data)],
    ' not fulfilled.') %>%
    paste(
      collapse = ';\n'
    )
}