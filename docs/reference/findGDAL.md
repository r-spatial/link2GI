# Find GDAL installation(s)

Detects GDAL command line installations on Windows and Unix-like systems
(Linux/macOS). On Windows, the search is restricted to valid
drive-letter paths. On Unix systems, the function searches common system
locations for a \`gdalinfo\` executable.

This function is a thin, platform-aware dispatcher that calls
\[searchGDALW()\] on Windows and \[searchGDALX()\] on Unix systems.

## Usage

``` r
findGDAL(searchLocation = "default", quiet = TRUE)
```

## Arguments

- searchLocation:

  Character. Search root or hint. Use \`"default"\` (recommended) to
  search standard locations (\`"C:/"\` on Windows; \`"~"\`, \`"/usr"\`,
  \`"/usr/local"\`, \`"/opt"\` on Unix).

- quiet:

  Logical. If \`TRUE\`, suppress messages.

## Value

Platform-specific GDAL installation descriptor:

- On Windows: the result of \[searchGDALW()\] or \`FALSE\`

- On Unix: the result of \[searchGDALX()\]
