test_that("findGDAL respects platform guards", {
  
  skip_if_not(.Platform$OS.type == "windows")
  
  with_mocked_bindings(
    searchGDALW = function(DL, quiet) "OK_WIN",
    {
      expect_false(findGDAL(searchLocation = "/usr/bin", quiet = TRUE))
      expect_equal(findGDAL("C:/", quiet = TRUE), "OK_WIN")
    }
  )
})
