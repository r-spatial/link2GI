# tests/testthat/test-grass-find.R
testthat::test_that("findGRASS() validates Unix vs Windows path syntax", {
  # On Unix, Windows-style drive specs must be rejected deterministically.
  if (Sys.info()[["sysname"]] != "Windows") {
    res <- link2GI::findGRASS(searchLocation = "C:/", quiet = TRUE)
    testthat::expect_true(isFALSE(res) || (is.list(res) && isFALSE(res$exist)))
  }
})
