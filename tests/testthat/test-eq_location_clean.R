context("Clean the Data2")
library(testthat)
library(Equaker)

test_that("eq_clean_data() creates a data frame", {
  df = readr::read_delim("./results.tsv", delim = "\t")
  datLC <- eq_location_clean(df)
  expect_that(datLC, is_a("data.frame"))
})