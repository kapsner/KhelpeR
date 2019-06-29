context("test statistics")

prefix <- "./"
#prefix <- "tests/testthat/"

test_that("test shapiroUtil", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(s1 <- shaprioUtil(dataset, variable = "disp"))
  expect_known_hash(s1, "09a8871923")
})

test_that("test leveneUtil", {
  dataset <- data.table::data.table(datasets::mtcars)
  dataset[,("gear"):=factor(get("gear"))]

  expect_silent(l1 <- leveneUtil(dataset, variable = "disp", group = "gear"))
  expect_known_hash(l1, "5cef2ff056")
})
