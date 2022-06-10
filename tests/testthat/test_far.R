library(fars)

test_that("fars_read returns a data frame", {
  dates <- 2013:2015
  expect_is(fars_read(make_filename(2014)), "data.frame")
})

