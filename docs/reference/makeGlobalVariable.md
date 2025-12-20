# Generates a variable with a certain value in the R environment

Generates a variable with a certain value in the R environment.

## Usage

``` r
makeGlobalVariable(names, values)
```

## Arguments

- names:

  vector with the names of the variable(s)

- values:

  vector with values of the variable(s)

## Examples

``` r
if (FALSE) { # \dontrun{
# creates the global variable \code{path_data} with the value \code{~/data}
makeGlobalVariable(names = 'path_data', values = '~/data')
} # }
```
