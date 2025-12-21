# Locate and describe Orfeo ToolBox (OTB) API bindings

Public wrapper that dispatches to OS-specific implementations. No PATH
mutation, no environment setup here.

Dispatcher that selects the platform-specific OTB locator. This function
is non-invasive: it does NOT modify PATH or ENV.

## Usage

``` r
linkOTB(
  bin_OTB = NULL,
  root_OTB = NULL,
  type_OTB = NULL,
  searchLocation = NULL,
  ver_select = FALSE,
  quiet = TRUE,
  returnPaths = TRUE
)

linkOTB(
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

  Optional. Path to the OTB \`bin/\` directory.

- root_OTB:

  Optional. Path to the OTB installation root directory.

- type_OTB:

  Optional installation type filter (if available from discovery).

- searchLocation:

  Optional search location for autodetect (e.g. mountpoint).

- ver_select:

  Selection logic: FALSE = newest, TRUE = interactive, numeric = row
  index.

- quiet:

  Logical. If FALSE, print selection tables and messages.

- returnPaths:

  Logical. If TRUE, return the gili descriptor.

## Value

A gili list describing the selected OTB installation.
