context("test extensiveNumStats")

prefix <- "./"
#prefix <- "tests/testthat/"

test_that("test extensiveNumStats", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(s1 <- dataset[,extensiveNumStats(get("wt"))])
  expect_known_hash(s1, "2d98143852")
})


test_that("test numStatsTable", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(nt1 <- numStatsTable(dataset))
  expect_known_hash(nt1, "0edfd70a80")

  expect_silent(nt2 <- numStatsTable(dataset, group_var = "gear"))
  expect_known_hash(nt2, "fbd27dd015")
})
