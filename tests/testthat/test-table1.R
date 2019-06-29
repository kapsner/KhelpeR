context("test functioning table1")

prefix <- "./"
#prefix <- "tests/testthat/"

test_that("test creation of table1", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(tb1 <- fillTable1(dataset))
  expect_known_hash(tb1, "8043a24a1c")

  expect_silent(tb2 <- fillTable1(dataset, group_var = "gear"))
  expect_known_hash(tb2, "e251d805f5")
})
