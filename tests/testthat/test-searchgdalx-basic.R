test_that("searchGDALX detects GDAL via gdalinfo", {
  
  fake_gdalinfo <- "/usr/bin/gdalinfo"
  
  with_mocked_bindings(
    .link2gi_which = function(cmd) fake_gdalinfo,
    .link2gi_sys2 = function(...) "GDAL 3.8.0, released",
    .link2gi_dir_exists = function(x) TRUE,
    .link2gi_file_exists = function(x) TRUE,
    .link2gi_glob = function(x) character(0),
    {
      res <- searchGDALX(quiet = TRUE)
      
      expect_true(is.list(res))
      expect_true("gdalInstallations" %in% names(res))
      expect_true(nrow(res$gdalInstallations) >= 1)
      expect_true("binDir" %in% names(res$gdalInstallations))
    }
  )
})
