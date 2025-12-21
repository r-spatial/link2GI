# Locate Orfeo ToolBox (OTB) installations

Dispatcher that calls the OS-specific search function and returns a
normalized installations table.

## Usage

``` r
findOTB(searchLocation = NULL, quiet = TRUE)
```

## Arguments

- searchLocation:

  Character. On Linux: mountpoints/roots to search. If NULL, defaults to
  "default" which expands to c("~","/opt","/usr/local","/usr").

- quiet:

  Logical.

## Value

data.frame of installations or FALSE.
