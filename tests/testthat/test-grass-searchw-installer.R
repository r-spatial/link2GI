# tests/testthat/test-grass-searchw-installer.R

testthat::test_that("searchGRASSW normalizes installer type", {
  
  testthat::skip_on_os("linux")
  testthat::skip_on_os("mac")
  
  testthat::with_mocked_bindings(
    
    # keep path normalizer trivial for the test
    .bf_wpath = function(x) x,
    
    # wrappers used by searchGRASSW()
    .link2gi_sys2 = function(command, args = character(), stdout = TRUE, stderr = TRUE, ...) {
      
      # 1) WHERE /R <DL> grass*.bat
      if (identical(tolower(command), "cmd.exe") &&
          length(args) >= 5 &&
          identical(tolower(args[1:2]), c("/c", "where"))) {
        return("C:\\OSGeo4W64\\bin\\grass84.bat")
      }
      
      # 2) TYPE <bat>
      if (identical(tolower(command), "cmd.exe") &&
          length(args) >= 3 &&
          identical(tolower(args[1:2]), c("/c", "type"))) {
        
        return(c(
          "@echo off",
          "set OSGEO4W_ROOT=C:\\OSGeo4W64",
          "call \"%OSGEO4W_ROOT%\\bin\\o4w_env.bat\"",
          "call \"%OSGEO4W_ROOT%\\bin\\qt5_env.bat\"",
          "call \"%OSGEO4W_ROOT%\\bin\\py3_env.bat\""
        ))
      }
      
      stop("Unexpected .link2gi_sys2 call: ", command, " ", paste(args, collapse = " "))
    },
    
    .link2gi_dir_exists = function(path) {
      # root_dir existence check: searchGRASSW checks root_dir (OSGeo4W root)
      grepl("OSGeo4W64$", path)
    },
    
    .link2gi_file_exists = function(path) FALSE,
    
    {
      res <- link2GI::searchGRASSW(DL = "C:/", quiet = TRUE)
      
      testthat::expect_s3_class(res, "data.frame")
      testthat::expect_true(nrow(res) >= 1)
      
      testthat::expect_true("installation_type" %in% names(res))
      testthat::expect_equal(tolower(res$installation_type[[1]]), "osgeo4w")
    }
  )
})
