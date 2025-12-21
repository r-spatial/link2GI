testthat::test_that("runOTB retCommand is deterministic for identical cmd", {
  gili <- list(
    exist   = TRUE,
    otbCmd  = "otbcli",
    launcher = "otbApplicationLauncherCommandLine"
  )
  
  cmd <- list(
    "DimensionalityReduction",
    "in"     = "in.tif",
    "method" = "pca",
    "nbcomp" = "3",
    "out"    = "out.tif"
  )
  
  s1 <- link2GI::runOTB(cmd, gili = gili, retCommand = TRUE, quiet = TRUE)
  s2 <- link2GI::runOTB(cmd, gili = gili, retCommand = TRUE, quiet = TRUE)
  
  testthat::expect_identical(s1, s2)
})
