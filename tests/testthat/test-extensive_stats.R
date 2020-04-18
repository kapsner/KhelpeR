context("test extensive_stats")

prefix <- "./"
#% prefix <- "tests/testthat/"

test_that("test extensive_stats", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[, ("gear") := factor(get("gear"))]

  expect_silent(s1 <- dataset[, extensive_stats(get("wt"))])
  expect_known_hash(s1, "93b5de052e")
})


test_that("test continuous_stats", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[, ("gear") := factor(get("gear"))]

  expect_silent(nt1 <- continuous_stats(dataset))
  expect_known_hash(nt1, "1ad4175f51")

  expect_silent(nt2 <- continuous_stats(dataset, group_var = "gear"))
  expect_known_hash(nt2, "36f2944060")
})
