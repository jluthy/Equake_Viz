context("Timeline")
library(testthat)
library(Equaker)

test_check("Equaker")

test_that("geom_timeline() creates a timeline plot", {
  plot <- geom_timeline(df)
  expect_that(plot, is_a("ggplot"))
})
