context("Labeled Timeline")
library(testthat)
library(Equaker)
library(ggplot2)

test_that("geom_timeline_label() creates a timeline plot", {
  df <- readr::read_delim("./results.tsv", delim = "\t") %>% eq_clean_data() %>% eq_location_clean()
  plot <- geom_timeline_label(df)
  expect_that(plot, is_a("ggplot"))
})
