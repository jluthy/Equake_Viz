context("Create Label")
library(testthat)
library(Equaker)


test_that("eq_create_label() creates data frame", {
  df <- readr::read_delim("./results.tsv", delim = "\t") %>% eq_clean_data() %>% eq_location_clean()
  label <- eq_create_label(df)
  expect_that(label, is_a("data.frame"))
})
