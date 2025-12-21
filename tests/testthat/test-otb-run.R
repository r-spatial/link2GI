# tests/testthat/test-otb-run.R

testthat::test_that("runOTB retCommand builds a command string", {
  testthat::skip_if_not(requireNamespace("link2GI", quietly = TRUE))
  
  gili <- list(exist = TRUE, otbCmd = "otbcli", launcher = "otbApplicationLauncherCommandLine")
  
  cmd <- list(
    "DimensionalityReduction",
    "in"  = "in.tif",
    "out" = "out.tif"
  )
  
  s <- link2GI::runOTB(cmd, gili = gili, retCommand = TRUE, quiet = TRUE)
  
  testthat::expect_type(s, "character")
  testthat::expect_true(grepl("DimensionalityReduction", s, fixed = TRUE))
  testthat::expect_true(grepl("-in",  s, fixed = TRUE))
  testthat::expect_true(grepl("in.tif", s, fixed = TRUE))
  testthat::expect_true(grepl("-out", s, fixed = TRUE))
})
