context("test utilities")

prefix <- "./"
#% prefix <- "tests/testthat/"

test_that("test p_marker", {
  expect_equal(p_marker(0.1), "")
  expect_equal(p_marker(0.07), "°")
  expect_equal(p_marker(0.05), "°")
  expect_equal(p_marker(0.02), "*")
  expect_equal(p_marker(0.01), "*")
  expect_equal(p_marker(0.0099), "**")
  expect_equal(p_marker(0.001), "**")
  expect_equal(p_marker(0.0008), "***")
  expect_equal(p_marker("Error"), "")
})
