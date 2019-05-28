context("Clean the Data2")
library(testthat)
library(Equaker)

test_check("Equaker")

test_that("eq_clean_data() creates a data frame", {
  datLC <- eq_location_clean()
  expect_that(datLC, is_a("data.frame"))
})