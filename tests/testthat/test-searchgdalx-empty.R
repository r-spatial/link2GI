test_that("searchGDALX returns empty structure when nothing found", {
  
  testthat::skip_on_os("windows")
  
  with_mocked_bindings(
    .link2gi_which = function(cmd) "",
    .link2gi_glob = function(x) character(0),
    .link2gi_dir_exists = function(x) FALSE,
    .link2gi_file_exists = function(x) FALSE,
    {
      res <- searchGDALX(
        MP = tempdir(),
        quiet = TRUE
      )
      
      expect_true(is.list(res))
      expect_true("gdalInstallations" %in% names(res))
      expect_equal(nrow(res$gdalInstallations), 0)
    }
  )
})
