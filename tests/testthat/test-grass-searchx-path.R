testthat::test_that("searchGRASSX resolves GISBASE via --config path", {
  testthat::local_mocked_bindings(
    .link2gi_which = function(x) "/usr/bin/grass",
    .link2gi_sys2  = function(command, args, stdout = TRUE, stderr = TRUE, ...) {
      if (identical(args, c("--config","path"))) return("/usr/lib/grass83")
      if (identical(args, "--version")) return("GRASS GIS 8.3.2")
      character(0)
    },
    .link2gi_dir_exists = function(path) identical(path, "/usr/lib/grass83"),
    .env = asNamespace("link2GI")
  )
  
  out <- link2GI::searchGRASSX(quiet = TRUE)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(nrow(out) == 1)
  testthat::expect_equal(out$instDir[[1]], "/usr/lib/grass83")
  testthat::expect_equal(out$version[[1]], "8.3.2")
})
