library(testthat)
test_that("runOTB retCommand returns a CLI string", {
  gili <- list(exist = TRUE, otbCmd = "otbcli", launcher = "otbApplicationLauncherCommandLine")
  cmd <- list(
    "DimensionalityReduction",
    "in"= "in.tif",
    "out" = "out.tif"
  )
  
  s <- link2GI::runOTB(cmd, gili = gili, retCommand = TRUE, quiet = TRUE)
  
  expect_type(s, "character")
  expect_match(s, "DimensionalityReduction", fixed = TRUE)
  expect_match(s, "-in", fixed = TRUE)
  expect_match(s, "in.tif", fixed = TRUE)
  expect_match(s, "-out", fixed = TRUE)
  expect_match(s, "out.tif", fixed = TRUE)
})
