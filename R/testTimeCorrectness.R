#' @importFrom magrittr extract2
testTimeCorrectness = function(data, start.date, end.date, 
                               date.column = 'date') {
  create_daterange_from(start.date, end.date) %>%
    setdiff(data %>% extract2(date.column) %>% as.character()) %>%
    when(
      . %>% is_longer_than(0) ~raise_error_with_unchecked_dates(.),
      ~data
    )
}
raise_error_with_unchecked_dates = function(dates) {
  paste0('Data is missing some dates: ', toString(dates))
}
create_daterange_from = function(start.date, end.date) {
  stopifnot(is_date(start.date) & is_date(end.date))
  seq(as.Date(start.date), as.Date(end.date), by = 1) %>%
    as.character()
}