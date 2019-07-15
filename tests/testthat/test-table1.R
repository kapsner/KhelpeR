context("test functioning table1")

prefix <- "./"
#prefix <- "tests/testthat/"

test_that("test creation of table1Num", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(tb1 <- fillTable1Num(dataset))
  expect_known_hash(tb1, "d1719a1a82") # 77fece0600

  expect_silent(tb2 <- fillTable1Num(dataset, group_var = "gear"))
  expect_known_hash(tb2, "a49c014d60") # 9d19101d6e
})
