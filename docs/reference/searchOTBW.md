# Search for OTB installations on Windows (bounded, cmd-free)

Detects Orfeo Toolbox (OTB) installations on Windows using a bounded set
of plausible roots (no full-disk crawl). Modern standalone bundles (OTB
9.x) are detected by the presence of:

- an environment script: `otbenv.ps1` (preferred) or `otbenv.bat`

- a launcher: `bin/otbApplicationLauncherCommandLine.exe`

- at least one CLI wrapper in `bin/`: `otbcli_*.ps1`, `otbcli_*.bat`, or
  `otbcli_*.exe`

## Usage

``` r
searchOTBW(searchLocation = "C:/", DL = NULL, maxdepth = 8L, quiet = FALSE)
```

## Arguments

- searchLocation:

  Character scalar. Root directory to search (default `"C:/"`).

- DL:

  Character scalar. Deprecated alias for `searchLocation`.

- maxdepth:

  Integer. Best-effort maximum recursion depth for the recursive
  [`list.files()`](https://rdrr.io/r/base/list.files.html) search
  (default `8`).

- quiet:

  Logical. If `TRUE`, suppress messages.

## Value

A `data.frame` with one row per detected installation and columns:

- binDir:

  Normalized path to `<root>/bin`.

- baseDir:

  Normalized OTB root directory.

- otbCmd:

  Path to a detected CLI wrapper (ps1/bat/exe).

- envScript:

  Path to `otbenv.ps1` or `otbenv.bat`.

- launcher:

  Path to `otbApplicationLauncherCommandLine.exe`.

- installation_type:

  Classification string (e.g., `"OTB_STANDALONE_PS1"`).

## Details

Backward compatibility: older callers may pass `DL` instead of
`searchLocation`. Internally, `DL` is treated as an alias for
`searchLocation`.

## Examples

``` r
if (FALSE) { # \dontrun{
# bounded search under C:/
searchOTBW("C:/", quiet = FALSE)

# legacy alias
searchOTBW(DL = "C:/", quiet = FALSE)
} # }
```
