context("Clean the Data1")
library(testthat)
library(Equaker)

test_check("Equaker")

test_that("eq_clean_data() creates a data frame", {
  datF <- eq_clean_data
  expect_that(datF, is_a("data.frame"))
})
