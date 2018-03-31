library(getProcessVisualise)
context('testTimeCorrectness')

start.date = "2018-01-01"
end.date = "2018-01-04"
end.date.not.in.data = "2018-01-07"
test_that('testTimeCorrectness works fine', {
  expect_identical(
    testTimeCorrectness(daterange.data, start.date, end.date),
    daterange.data
  )
  expect_error(
    testTimeCorrectness(daterange.data, start.date, end.date.not.in.data),
    'Data is missing some dates: "2018-01-05", "2018-01-06", "2018-01-07"'
  )
})

test_that('creating date range works fine', {
  daterange = create_daterange_from(start.date, end.date)
  expect_identical(
    daterange, c("2018-01-01", "2018-01-02", "2018-01-03", "2018-01-04")
  )
})