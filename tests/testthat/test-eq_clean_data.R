context("Clean the Data1")
library(testthat)
library(Equaker)

test_that("eq_clean_data() creates a data frame", {
  df = readr::read_delim("./results.tsv", delim = "\t")
  dataF <- eq_clean_data(df)
  expect_that(dataF, is_a("data.frame"))
})
