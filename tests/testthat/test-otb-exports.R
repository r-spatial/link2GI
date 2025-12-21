library(testthat)
test_that("core entry points exist", {
  expect_true(exists("linkOTB", where = asNamespace("link2GI")))
  expect_true(exists("runOTB", where = asNamespace("link2GI")))
  expect_true(exists("runOTB_isolated", where = asNamespace("link2GI")))
})
