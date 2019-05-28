context("Mapping")
library(testthat)
library(Equaker)

test_check("Equaker")

test_that("eq_map() retuns a map", {
  map <- eq_map(df)
  expect_that(map, is_a("leaflet"))
})
