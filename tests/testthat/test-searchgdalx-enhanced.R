test_that("searchGDALX classifies installer type from path fragments", {
  
  testthat::skip_on_os("windows")
  
  td <- tempfile("gdalx-")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  
  # --- helper to create a fake gdalinfo executable in a synthetic install tree
  mk_install <- function(root_name, extra_bins = TRUE) {
    root <- file.path(td, root_name)
    bin  <- file.path(root, "bin")
    dir.create(bin, recursive = TRUE, showWarnings = FALSE)
    
    gdalinfo <- file.path(bin, "gdalinfo")
    file.create(gdalinfo)
    Sys.chmod(gdalinfo, mode = "0755")  # make it executable so `find ... -executable` sees it
    
    if (isTRUE(extra_bins)) {
      file.create(file.path(bin, "gdalwarp"))
      Sys.chmod(file.path(bin, "gdalwarp"), mode = "0755")
      
      file.create(file.path(bin, "gdal_calc.py"))
      # no chmod needed for .py (not required by your listing logic)
    }
    
    root
  }
  
  # Create two installs whose *paths* trigger your heuristic classification
  p_conda <- mk_install(file.path("miniconda3"))
  p_grass <- mk_install(file.path("apps", "grass"))
  
  res <- searchGDALX(MP = c(p_conda, p_grass), quiet = TRUE)
  
  expect_true(is.list(res))
  expect_true(all(c("gdalInstallations", "bin", "py") %in% names(res)))
  
  inst <- res$gdalInstallations
  expect_true(is.data.frame(inst))
  expect_true(nrow(inst) >= 2)
  
  types <- sort(unique(inst$installation_type))
  expect_true(all(c("conda", "grass") %in% types))
  
  # Basic sanity: we should have collected "gdal*" tools in binDir for each installation
  expect_equal(length(res$bin), nrow(inst))
  expect_equal(length(res$py),  nrow(inst))
  
  # At least one installation should list our fake tools
  any_has_gdalwarp <- any(vapply(res$bin, function(x) any(grepl("gdalwarp$", x$gdal_bin)), logical(1)))
  any_has_py <- any(vapply(res$py, function(x) any(grepl("gdal_calc\\.py$", x$gdal_py)), logical(1)))
  
  expect_true(any_has_gdalwarp)
  expect_true(any_has_py)
})
