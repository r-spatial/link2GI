# Build package manually

This function was specifically designed to build a package from local
source files manually, i.e., without using the package building
functionality offered e.g. by RStudio.

## Usage

``` r
manuallyBuild(dsn = getwd(), pkgDir = "H:/Dokumente", document = TRUE, ...)
```

## Arguments

- dsn:

  'character'. Target folder containing source files; defaults to the
  current working directory.

- pkgDir:

  'character'. Target folder containing the result ing package of the
  invoked build process. According to Marburg University pools the
  default is set to pkgDir='H:/Dokumente'. If you want to use it in a
  different setting you may set pkgDir to whatever you want.

- document:

  'logical'. Determines whether or not to invoke roxygenize with default
  roclets for documentation purposes.

- ...:

  Further arguments passed on to devtools build.

## Details

NOTE the default setting are focussing HRZ environment at Marburg
University

## Author

Florian Detsch, Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
## when in a package directory, e.g. '~/link2GI' 
manuallyBuild()
} # }

```
