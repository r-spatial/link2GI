# Saves data in rds format and adds a yaml metadata file.

Saves data in rds format and saves metadata in a corresponding yaml
file.

## Usage

``` r
saveEnvi(variable, file_path, meta)
```

## Arguments

- variable:

  name of the data variable to be saved.

- file_path:

  name and path of the rds file.

- meta:

  name of the metadata list.

## Examples

``` r
if (FALSE) { # \dontrun{
a <- 1
meta <- list(a = 'a is a variable')
saveEnvi(a, file.path(tempdir(), 'test.rds'), meta)
} # }
```
