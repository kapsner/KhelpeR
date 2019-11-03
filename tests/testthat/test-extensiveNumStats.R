context("test extensive_stats")

prefix <- "./"
#prefix <- "tests/testthat/"

test_that("test extensive_stats", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(s1 <- dataset[,extensive_stats(get("wt"))])
  expect_known_hash(s1, "2d98143852")
})


test_that("test stats_table", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(nt1 <- stats_table(dataset))
  expect_known_hash(nt1, "0edfd70a80")

  expect_silent(nt2 <- stats_table(dataset, group_var = "gear"))
  expect_known_hash(nt2, "fbd27dd015")
})
