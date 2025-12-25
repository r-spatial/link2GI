# Get GRASS GIS parameters on Windows

Detects available GRASS GIS installations on Windows systems, selects a
suitable installation, and optionally activates the GRASS runtime
environment for use with rgrass.

This function is primarily intended for internal use by
[`linkGRASS`](https://r-spatial.github.io/link2GI/reference/linkGRASS.md)
and related helpers.

## Usage

``` r
paramGRASSw(
  set_default_GRASS = NULL,
  DL = "C:/",
  ver_select = FALSE,
  quiet = TRUE,
  activate = TRUE,
  sysname = Sys.info()[["sysname"]]
)
```

## Arguments

- set_default_GRASS:

  Optional predefined GRASS installation. Must be coercible to a data
  frame with columns `instDir`, `version`, and `installation_type`.

- DL:

  Character. Search root for GRASS installations. Defaults to `"C:/"`.

- ver_select:

  Logical or numeric. If `FALSE` (default), the latest version is
  selected automatically. If numeric, selects the corresponding
  installation index. If `TRUE`, interactive selection is used.

- quiet:

  Logical. If `TRUE` (default), suppress informational messages.

- activate:

  Logical. If `TRUE` (default), activate the selected GRASS GIS
  environment.

- sysname:

  Character. Internal/testing hook overriding `Sys.info()[["sysname"]]`.

## Value

A named list with elements:

- gisbase_GRASS:

  Path to the selected GRASS GIS base directory.

- version:

  Detected GRASS GIS version string (may be `NA`).

- type:

  Installation type (e.g. `"osgeo4w"`, `"standalone"`).

- installed:

  Data frame of all detected installations.

- exist:

  Logical indicating whether a valid installation was found.

If no installation is found or the platform is not Windows,
`list(exist = FALSE)` is returned.

## Details

The function searches for valid GRASS GIS installations using
[`findGRASS`](https://r-spatial.github.io/link2GI/reference/findGRASS.md).
If multiple installations are found, the selection can be controlled via
`ver_select`.

If `activate = TRUE`, the GRASS environment variables are initialized
via
[`setenvGRASSw`](https://r-spatial.github.io/link2GI/reference/setenvGRASSw.md),
enabling immediate use of GRASS GIS through rgrass and command-line
calls.

No spatial data object is required at this stage; spatial initialization
is handled later by
[`linkGRASS`](https://r-spatial.github.io/link2GI/reference/linkGRASS.md).

## See also

[`findGRASS`](https://r-spatial.github.io/link2GI/reference/findGRASS.md),
[`setenvGRASSw`](https://r-spatial.github.io/link2GI/reference/setenvGRASSw.md),
[`linkGRASS`](https://r-spatial.github.io/link2GI/reference/linkGRASS.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# automatic detection and activation
g <- paramGRASSw()

# select a specific installation by index
g <- paramGRASSw(ver_select = 1)

# predefined installation (advanced use)
g <- paramGRASSw(
  set_default_GRASS = c("C:/OSGeo4W64", "8.3.2", "osgeo4w")
)
} # }
```
