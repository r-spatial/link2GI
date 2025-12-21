# Return attributes of valid GRASS GIS installation(s) on Unix

Searches recursively for valid GRASS GIS installations on Unix-like
systems (Linux/macOS) and returns basic attributes for each detected
installation.

The implementation avoids GNU-specific \`find\` primaries (e.g.
\`-readable\`) and relies on a portable \`system2()\` call.

## Usage

``` r
searchGRASSX(MP = "/usr/bin", quiet = TRUE)
```

## Arguments

- MP:

  Character. Search root directory. \`"default"\` expands to \`c("~",
  "/opt", "/usr/local", "/usr")\`.

- quiet:

  Logical. If \`TRUE\`, suppress messages.

## Value

\`FALSE\` if no installation is found, otherwise a \`data.frame\` with
columns:

- instDir:

  GRASS installation root directory

- version:

  Detected GRASS version (character, may be \`NA\`)

- installation_type:

  Launcher or installation type

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
## Typical system-wide location
searchGRASSX("/usr/bin")

## Search user home
searchGRASSX("~")
} # }
```
