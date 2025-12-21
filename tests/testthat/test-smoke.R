library(testthat)
test_that("package loads", {
  expect_true(requireNamespace("link2GI", quietly = TRUE))
})
