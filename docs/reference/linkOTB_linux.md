# Locate and describe Orfeo ToolBox (OTB) bindings (Linux/macOS)

Internal OS-specific implementation. Returns a gili list with a valid
\`launcher\` (otbApplicationLauncherCommandLine) if present.

## Usage

``` r
linkOTB_linux(
  bin_OTB = NULL,
  root_OTB = NULL,
  type_OTB = NULL,
  searchLocation = NULL,
  ver_select = FALSE,
  quiet = TRUE,
  returnPaths = TRUE
)
```

## Arguments

- bin_OTB:

  Optional. Path to OTB bin directory.

- root_OTB:

  Optional. Path to OTB root directory.

- type_OTB:

  Optional installation type filter.

- searchLocation:

  Default \`/usr/bin/\` for auto-detect, or a mountpoint.

- ver_select:

  Selection logic (FALSE = newest, TRUE = interactive, numeric = row).

- quiet:

  Logical.

- returnPaths:

  Logical.

## Value

gili list.
