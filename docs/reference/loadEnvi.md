# Load data from rds format and associated yaml metadata file.

Load data from rds format and associated yaml metadata file.

## Usage

``` r
loadEnvi(file_path)
```

## Arguments

- file_path:

  name and path of the rds file.

## Value

list of 2 containing data and metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
a <- 1
meta <- list(a = 'a is a variable')
saveEnvi(a, file.path(tempdir(), 'test.rds'), meta)
b <- loadEnvi(file.path(tempdir(), 'test.rds'))
} # }
```
