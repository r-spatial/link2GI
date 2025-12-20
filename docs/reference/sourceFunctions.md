# Source functions from standard or given directory

Source functions into the R environment located in a specified folder.

## Usage

``` r
sourceFunctions(fcts_folder, setup_script)
```

## Arguments

- fcts_folder:

  path of the folder holding the functions. All files in this folder
  will be sourced at project start.

## Value

Information if sourcing was successfull based on try function.

## Examples

``` r
if (FALSE) { # \dontrun{
# sourceFunctions(fcts_folder = '~/project/src/fcts')
} # }
```
