context("Mapping")
library(testthat)
library(Equaker)
library(ggplot2)

test_that("eq_map() retuns a map", {
  df <- readr::read_delim("./results.tsv", delim = "\t") %>% eq_clean_data() %>% eq_location_clean()
  eq_create_label(df)
    map <- eq_map(df, annot_col = 'DATE')
  expect_that(map, is_a("leaflet"))
})
