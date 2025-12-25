# Find SAGA GIS installation(s)

Detects SAGA GIS command line installations on Windows and Unix-like
systems (Linux/macOS). This is a thin, platform-aware dispatcher:

- Windows: calls \[searchSAGAW()\]

- Unix: calls \[searchSAGAX()\]

## Usage

``` r
findSAGA(
  searchLocation = "default",
  quiet = TRUE,
  sysname = Sys.info()[["sysname"]]
)
```

## Arguments

- searchLocation:

  Character. Search root or hint. Use \`"default"\` to search standard
  locations (\`"C:/"\` on Windows; \`"~"\`, \`"/usr"\`,
  \`"/usr/local"\`, \`"/opt"\` on Unix).

- quiet:

  Logical. If \`TRUE\`, suppress messages.

- sysname:

  Character. Internal/testing hook overriding
  \`Sys.info()\[\["sysname"\]\]\`.

## Value

Platform-specific SAGA installation descriptor or \`FALSE\`.
