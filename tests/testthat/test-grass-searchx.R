testthat::test_that("searchGRASSX returns FALSE when grass not on PATH", {
  testthat::local_mocked_bindings(
    .link2gi_which = function(x) "",
    .env = asNamespace("link2GI")
  )
  testthat::expect_false(link2GI::searchGRASSX(quiet = TRUE))
})
