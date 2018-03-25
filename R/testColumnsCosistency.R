testColumnsCosistency = function(data, conditions) {
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
other_than_data = function(results) {
  nrow(results) != nrow(data)
}