# Returns attributes of valid 'GRASS GIS' installation(s) on the system.

Retrieve a list of valid 'GRASS GIS' installation(s) on your system. On
Windows, uses searchGRASSW() (cmd-free). On Unix, uses searchGRASSX().

## Usage

``` r
findGRASS(searchLocation = "default", ver_select = FALSE, quiet = TRUE)
```

## Arguments

- searchLocation:

  On Windows MUST start with drive letter + colon, e.g. "C:", "C:/",
  "C:/Users/...". Defaults to "C:/". On Unix defaults to "/usr/bin".

- ver_select:

  If TRUE and more than one installation is found, interactively select
  one.

- quiet:

  Suppress messages.

## Value

FALSE or data.frame(instDir, version, installation_type)
