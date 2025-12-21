testthat::test_that("runOTB retCommand omits NULL and NA args", {
  gili <- list(
    exist   = TRUE,
    otbCmd  = "otbcli",
    launcher = "otbApplicationLauncherCommandLine"
  )
  
  cmd <- list(
    "DimensionalityReduction",
    "in"  = "in.tif",
    "out" = "out.tif",
    "foo" = NA_character_,  # must be omitted
    "bar" = NULL            # must be omitted
  )
  
  s <- link2GI::runOTB(cmd, gili = gili, retCommand = TRUE, quiet = TRUE)
  
  testthat::expect_false(grepl("-foo", s, fixed = TRUE))
  testthat::expect_false(grepl("-bar", s, fixed = TRUE))
  
  # sanity: required args are present
  testthat::expect_true(grepl("DimensionalityReduction", s, fixed = TRUE))
  testthat::expect_true(grepl("-in", s, fixed = TRUE))
  testthat::expect_true(grepl("in.tif", s, fixed = TRUE))
  testthat::expect_true(grepl("-out", s, fixed = TRUE))
  testthat::expect_true(grepl("out.tif", s, fixed = TRUE))
  testthat::expect_false(grepl("-foo", s, fixed = TRUE))
  testthat::expect_false(grepl("-bar", s, fixed = TRUE))
})
