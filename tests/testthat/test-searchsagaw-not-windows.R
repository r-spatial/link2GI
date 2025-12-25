# tests/testthat/test-searchsagaw-not-windows.R
test_that("searchSAGAW returns FALSE on non-Windows", {
  testthat::skip_on_os("windows") # we run this on Linux/macOS CI
  res <- searchSAGAW(DL = "C:/", quiet = TRUE)
  testthat::expect_identical(res, FALSE)
})
