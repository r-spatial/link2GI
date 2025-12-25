# tests/testthat/test-searchsagax-empty.R
test_that("searchSAGAX returns FALSE when nothing found", {
  testthat::skip_on_os("windows")
  
  testthat::with_mocked_bindings(
    .link2gi_dir_exists = function(x) rep(TRUE, length(x)),
    .link2gi_sys2 = function(...) character(0),
    {
      res <- searchSAGAX(MP = "/does/not/matter", quiet = TRUE)
      testthat::expect_identical(res, FALSE)
    }
  )
})
