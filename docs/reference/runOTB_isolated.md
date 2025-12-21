# Execute an OTB application in an isolated OTB environment (mainly Windows)

\- Windows: dot-sources \`otbenv.ps1\` (preferred) or calls
\`otbenv.bat\`, then runs \`otbcli\` within the same shell session. -
Linux/macOS: delegates to \[runOTB()\] (launcher + explicit env already
used).

## Usage

``` r
runOTB_isolated(otbCmdList, gili = NULL, retCommand = FALSE, quiet = TRUE)
```

## Arguments

- otbCmdList:

  Non-empty list. \`otbCmdList\[\[1\]\]\` is the OTB application name.
  Named entries are parameter keys without leading dashes.

- gili:

  Optional list from \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

- retCommand:

  Logical. If \`TRUE\`, returns the exact shell command that would be
  executed instead of running it.

- quiet:

  Logical. If \`TRUE\`, suppresses stdout/stderr (best-effort).

## Value

If \`retCommand=TRUE\`, a character scalar command line. Otherwise an
invisible status code.
