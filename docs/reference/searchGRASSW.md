# Search for valid GRASS GIS installations on Windows

Searches for GRASS GIS installations on \*\*Windows\*\* using a
\*bounded\* set of plausible installation roots (no full-disk crawl).
The function supports:

- OSGeo4W / QGIS-style layouts via
  `<root>/apps/grass/grass*/etc/VERSIONNUMBER`

- Standalone GRASS installs via
  `<Program Files>/GRASS GIS */etc/VERSIONNUMBER`

- Optional per-user OSGeo4W installs under
  `<USERPROFILE>/AppData/Local/Programs/OSGeo4W`

## Usage

``` r
searchGRASSW(DL = "C:/", quiet = TRUE)

searchGRASSW(DL = "C:/", quiet = TRUE)
```

## Arguments

- DL:

  Character. Search root (e.g. \`"C:/"\`).

- quiet:

  Logical. Suppress messages.

## Value

Returns `FALSE` if no installation was detected. Otherwise returns a
`data.frame` with columns:

- instDir:

  Root directory of the installation candidate.

- version:

  Parsed version string (from `VERSIONNUMBER`) or `NA`.

- installation_type:

  One of `"osgeo4w"`, `"qgis"`, `"standalone"`.

The result is sorted by decreasing semantic version (unknown versions
treated as `0.0.0`).

\`FALSE\` or a \`data.frame\` with columns \`instDir\`, \`version\`,
\`installation_type\`.

## Details

The argument `DL` can be a full path or a Windows drive root (e.g.
`"C:"` or `"C:/"`). Drive roots are expanded to a fixed set of candidate
directories: `OSGeo4W64`, `OSGeo4W`, `Program Files`,
`Program Files (x86)`.

This function is intentionally conservative to remain fast and
deterministic on large Windows volumes. It does \*\*not\*\* recurse the
entire drive.

If multiple installations are present under the searched roots, all are
returned. Version parsing extracts the first `x.y[.z...]` pattern from
the first line of `VERSIONNUMBER`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search from the C: drive root (bounded roots, no full-disk scan)
searchGRASSW("C:/", quiet = FALSE)

# Search a concrete directory only
searchGRASSW("C:/OSGeo4W64", quiet = FALSE)

# Drive letter without slash is accepted
searchGRASSW("C:", quiet = TRUE)
} # }
```
