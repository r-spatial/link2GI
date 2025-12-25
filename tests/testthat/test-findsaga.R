test_that("findSAGA Windows guard rejects non-drive searchLocation", {
  testthat::local_mocked_bindings(
    searchSAGAW = function(DL, quiet) stop("must not be called"),
    .package = "link2GI"
  )
  
  res <- link2GI::findSAGA(searchLocation = "/nope", quiet = TRUE, sysname = "Windows")
  testthat::expect_false(res)
})

test_that("findSAGA Windows default dispatches to searchSAGAW", {
  called <- FALSE
  
  testthat::local_mocked_bindings(
    searchSAGAW = function(DL, quiet) {
      called <<- TRUE
      testthat::expect_equal(DL, "C:/")
      testthat::expect_true(is.logical(quiet))
      data.frame(binDir = "C:/SAGA/bin", stringsAsFactors = FALSE)
    },
    .package = "link2GI"
  )
  
  res <- link2GI::findSAGA(searchLocation = "default", quiet = TRUE, sysname = "Windows")
  testthat::expect_true(called)
  testthat::expect_s3_class(res, "data.frame")
})

test_that("findSAGA Unix default dispatches to searchSAGAX with vector roots", {
  called <- FALSE
  
  testthat::local_mocked_bindings(
    searchSAGAX = function(MP, quiet) {
      called <<- TRUE
      testthat::expect_true(is.character(MP))
      testthat::expect_true(length(MP) >= 2)
      testthat::expect_true(any(MP %in% c("~", "/usr", "/usr/local", "/opt")))
      testthat::expect_true(is.logical(quiet))
      list(found = FALSE)
    },
    .package = "link2GI"
  )
  
  res <- link2GI::findSAGA(searchLocation = "default", quiet = TRUE, sysname = "Linux")
  testthat::expect_true(called)
  testthat::expect_type(res, "list")
})
