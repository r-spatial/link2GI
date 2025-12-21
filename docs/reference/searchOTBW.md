# Search recursively valid 'OTB' installation(s) on a given Windows drive/path

Robust Windows OTB discovery for modern standalone bundles (OTB 9.x): -
detects root by presence of 'otbenv.ps1' (or 'otbenv.bat') - checks for
'bin/otbApplicationLauncherCommandLine.exe' - checks for at least one
'bin/otbcli\_\*.ps1' wrapper

## Usage

``` r
searchOTBW(searchLocation = NULL, DL = "C:/", maxdepth = 8L, quiet = FALSE)
```

## Arguments

- searchLocation:

  Character. Folder to search (default `"C:/"`).

- DL:

  Character. Backward-compatible alias for `searchLocation`. If both are
  provided, `searchLocation` wins.

- maxdepth:

  Integer. Max recursion depth (best effort).

- quiet:

  Logical. Suppress messages.

## Value

data.frame with columns: binDir, baseDir, otbCmd, envScript, launcher,
installation_type

## Details

Backward compatible with legacy callers that use `DL` (as in older
link2GI versions). Internally, `DL` is treated as an alias for
`searchLocation`.
