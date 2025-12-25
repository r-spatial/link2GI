# Locate and set up SAGA GIS bindings

Detects available SAGA GIS installations and configures the environment
for calling the SAGA command line interface (\`saga_cmd\` /
\`saga_cmd.exe\`). Optionally prepares variables commonly used by
wrappers such as RSAGA.

The function relies on \[findSAGA()\] to discover installations (unless
a predefined installation table is provided via \`default_SAGA\`).

## Usage

``` r
linkSAGA(
  default_SAGA = NULL,
  searchLocation = "default",
  ver_select = FALSE,
  quiet = TRUE,
  returnPaths = TRUE
)
```

## Arguments

- default_SAGA:

  Optional. Either \`NULL\` (default; triggers \[findSAGA()\]) or a
  \`data.frame\`-like object describing installations (at least a first
  column with bin directory paths; a second column may provide module
  paths on Unix).

- searchLocation:

  Character. Search root or hint passed to \[findSAGA()\]. Use
  \`"default"\` to search standard locations.

- ver_select:

  Logical or numeric. If numeric, select the indexed installation. If
  \`TRUE\`, interactive selection. If \`FALSE\`, automatic selection.

- quiet:

  Logical. Suppress messages.

- returnPaths:

  Logical. If \`TRUE\`, return the selection list. If \`FALSE\`, only
  perform environment setup (side effects).

## Value

A list with elements:

- sagaPath:

  Selected SAGA bin directory.

- sagaModPath:

  Selected SAGA module directory (may be empty on Unix in this legacy
  logic).

- sagaCmd:

  Full path to the \`saga_cmd\` executable.

- installed:

  The detected installation table.

- exist:

  Logical; \`TRUE\` if a valid installation was selected.

## Details

The selection strategy is:

- If exactly one installation is found, it is used.

- If multiple are found and \`ver_select\` is numeric, the indexed
  installation is used.

- If multiple are found and \`ver_select = TRUE\`, an interactive
  selection is requested.

- If multiple are found and \`ver_select = FALSE\`, the "latest" entry
  is chosen (currently: last row; improve later by a numeric version
  compare).

On Windows, SAGA uses different module folder conventions depending on
version (e.g., \`tools/\` vs \`modules/\`). This function tries to
derive the correct module directory based on the detected version.
