# Search recursively for valid OTB installation(s) on Linux/macOS

Searches for an executable \`otbcli\` and validates presence of
\`otbApplicationLauncherCommandLine\` in the same bin folder (or
base/bin).

## Usage

``` r
searchOTBX(MP = "default", quiet = TRUE)
```

## Arguments

- MP:

  Character. "default" expands to c("~","/opt","/usr/local","/usr").
  Otherwise, one or more mountpoints/roots.

- quiet:

  Logical.

## Value

data.frame with OTB installations (or FALSE).
