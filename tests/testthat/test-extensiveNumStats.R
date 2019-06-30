context("test extensiveNumStats")

prefix <- "./"
#prefix <- "tests/testthat/"

test_that("test extensiveNumStats", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(s1 <- dataset[,extensiveNumStats(get("wt"))])
  expect_known_hash(s1, "cb99c670f3")
})


test_that("test numStatsTable", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(nt1 <- numStatsTable(dataset))
  expect_known_hash(nt1, "fb982e0959")

  expect_silent(nt2 <- numStatsTable(dataset, group_var = "gear"))
  expect_known_hash(nt2, "aedbf8145b")
})
