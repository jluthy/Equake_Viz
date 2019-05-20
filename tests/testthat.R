library(testthat)
library(Equaker)

test_check("Equaker")

test_that("eq_readIn_data() reads in the data and returns a data frame or tibble", {
  df <- eq_readIn_data("./data/results.tsv")
  expect_that(df, is_a("data.frame"))
})

test_that("eq_clean_data() reads in raw data frame and returns cleanded data frame or tibble", {
  df_clean <- eq_clean_data(df)
  expect_that(df_clean, is_a("data.frame"))
})

test_that("geom_timeline() creates a timeline plot", {
  plot <- geom_timeline(df_clean)
  expect_that(plot, is_a("ggplot"))
})

test_that("geom_timeline_label() creates a labeled timeline plot", {
  plot <- geom_timeline_label(df_clean)
  expect_that(plot, is_a("ggplot"))
})

test_that("eq_map() retuns a map", {
  map <- eq_map(df_filtered)
  expect_that(map, is_a("leaflet"))
})
