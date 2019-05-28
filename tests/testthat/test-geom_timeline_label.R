context("Labeled Timeline")
library(testthat)
library(Equaker)

test_check("Equaker")

test_that("eq_clean_data works", {
  path = system.file("extdata", "results.tsv", package = "Equaker")
  df = read.delim(path)
  expect_gt(nrow(df),0)
  df = eq_clean_data(df)
  expect_is(df$DATE, "Date")
  expect_is(df$LONGITUDE, "numeric")
  expect_is(df$LATITUDE, "numeric")
})
