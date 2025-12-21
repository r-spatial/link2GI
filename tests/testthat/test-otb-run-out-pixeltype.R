testthat::test_that("runOTB serializes pixel-typed outputs as two CLI tokens", {
  gili <- list(
    exist   = TRUE,
    otbCmd  = "otbcli",
    launcher = "otbApplicationLauncherCommandLine"
  )
  
  cmd <- list(
    "DimensionalityReduction",
    "in"  = "in.tif",
    "out" = c("out.tif", "float")
  )
  
  s <- link2GI::runOTB(cmd, gili = gili, retCommand = TRUE, quiet = TRUE)
  
  # must contain: -out out.tif float (in this order)
  testthat::expect_true(grepl("-out", s, fixed = TRUE))
  testthat::expect_true(grepl("out.tif", s, fixed = TRUE))
  testthat::expect_true(grepl("float", s, fixed = TRUE))
  
  # stronger: token sequence check
  testthat::expect_true(grepl("-out\\s+out\\.tif\\s+float", s))
})
