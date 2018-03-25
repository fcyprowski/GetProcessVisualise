#' @importFrom purrr detect_index
testColumnsCosistency = function(data, conditions) {
  # create function for data~result comparison
  other_than_data = partial(other_than_data_, data = data)
  # run first conditions
  conditions %>%
    map(~filter_(data, .)) -> result
  # create function for throw the message
  conditions_that_gives_results = partial(
    conditions_that_gives_results_,
    conditions = conditions,
    result = result
  )
  # now at last throw it or pass it
  result  %>%
    keep(~other_than_data(.)) %>%
    when(length(.) == 0 ~TRUE, ~FALSE) %>%
    raiseErrorOrPassForward(
      data,
      conditions_that_gives_results(other_than_data)
    )
    
}
other_than_data_ = function(results, data) {
  nrow(results) != nrow(data) 
}
conditions_that_gives_results_ = function(fun, conditions, result) {
  paste0('Condition ', conditions[detect_index(result, fun)], ' not fulfilled.') %>%
    paste(collapse = ';\n')
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