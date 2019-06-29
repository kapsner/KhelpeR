context("test utilities")

prefix <- "./"
#prefix <- "tests/testthat/"

test_that("test pMarker", {
  expect_equal(pMarker(0.1), "")
  expect_equal(pMarker(0.07), ".")
  expect_equal(pMarker(0.05), ".")
  expect_equal(pMarker(0.02), "*")
  expect_equal(pMarker(0.01), "*")
  expect_equal(pMarker(0.0099), "**")
  expect_equal(pMarker(0.001), "**")
  expect_equal(pMarker(0.0008), "***")
  expect_equal(pMarker("Error"), "")
})
