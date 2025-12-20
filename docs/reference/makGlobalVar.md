# Generates a variable with a certain value in the R environment

Generates a variable with a certain value in the R environment

## Usage

``` r
makGlobalVar(name, value)
```

## Arguments

- name:

  character string name of the variable

- value:

  character string value of the variable

## Examples

``` r
if (FALSE) { # \dontrun{

# creates the global var \code{pathToData} with the value \code{~/home/data}
makGlobalVar('pathToData','~/home/data') 

} # }
```
