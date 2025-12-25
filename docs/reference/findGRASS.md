# Find GRASS GIS installation(s)

Detects valid GRASS GIS installation(s) on the current system and
returns their installation paths and version information.

On Windows,
[`searchGRASSW`](https://r-spatial.github.io/link2GI/reference/searchGRASSW.md)
is used. On Unix-alike systems (Linux/macOS),
[`searchGRASSX`](https://r-spatial.github.io/link2GI/reference/searchGRASSX.md)
is used.

## Usage

``` r
findGRASS(searchLocation = "default", ver_select = FALSE, quiet = TRUE)
```

## Arguments

- searchLocation:

  Character. Search hint/root.

  - On Windows this MUST include a drive letter and colon, e.g. `"C:"`,
    `"C:/"`, `"C:/OSGeo4W64"`. The default is `"C:/"`.

  - On Unix the default is `"/usr/bin"`. You may pass either a directory
    or a full path to a `grass` executable; if a file is provided, its
    directory is used as hint.

- ver_select:

  Logical. If `TRUE` and more than one installation is found, a
  selection prompt is shown and only the chosen row is returned.

- quiet:

  Logical. If `TRUE` (default), suppress informational messages.

## Value

Returns `FALSE` if no installation is detected. Otherwise returns a
`data.frame` with columns:

- instDir:

  Installation directory (GISBASE).

- version:

  Parsed version string (may be `NA`).

- installation_type:

  Installation type identifier (e.g. `"osgeo4w"`, `"standalone"`; on
  Unix this may be derived from the executable name).

## See also

[`searchGRASSW`](https://r-spatial.github.io/link2GI/reference/searchGRASSW.md),
[`searchGRASSX`](https://r-spatial.github.io/link2GI/reference/searchGRASSX.md),
[`linkGRASS`](https://r-spatial.github.io/link2GI/reference/linkGRASS.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Windows
findGRASS("C:/", quiet = FALSE)

# Unix: hint directory
findGRASS("/usr/bin", quiet = FALSE)

# Unix: explicit executable path
findGRASS("/usr/bin/grass", quiet = FALSE)
} # }
```
