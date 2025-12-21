# tests/testthat/test-runotb-contract.R

testthat::test_that("runOTB() enforces basic command-list contract", {
  gili <- list(exist = TRUE, otbCmd = "otbcli", launcher = "otbApplicationLauncherCommandLine")
  
  testthat::expect_error(
    link2GI::runOTB(NULL, gili = gili, retCommand = TRUE),
    regexp = "is\\.list\\(otbCmdList\\) is not TRUE"
  )
  
  testthat::expect_error(
    link2GI::runOTB(list(), gili = gili, retCommand = TRUE),
    regexp = "length\\(otbCmdList\\) > 0 is not TRUE"
  )
  
  testthat::expect_error(
    link2GI::runOTB(list(1, 2, 3), gili = gili, retCommand = TRUE),
    regexp = NA
  )
})

testthat::test_that("runOTB() fails clearly when gili is invalid", {
  cmd <- list("DimensionalityReduction", `in` = "in.tif", out = "out.tif")
  
  testthat::expect_error(
    link2GI::runOTB(cmd, gili = NULL, retCommand = TRUE),
    "No valid OTB installation found",
    fixed = TRUE
  )
})