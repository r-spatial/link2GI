# Linux/macOS: lists plugin libs `otbapp_*.so`, `otbapp_*.dylib`, `otbapp_*.dll` Windows: lists wrappers `otbcli_<Algo>.ps1`, `otbcli_<Algo>.bat`, `otbcli_<Algo>.exe`\# Retrieve available OTB applications

Linux/macOS: lists \`otbapp\_\*.{so,dylib,dll}\` under
OTB_APPLICATION_PATH derived from OTB root.

## Usage

``` r
parseOTBAlgorithms(gili = NULL)
```

## Arguments

- gili:

  Optional list returned by \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

## Value

Character vector of application names.

## Details

Windows: lists wrappers \`otbcli\_\<Algo\>.{ps1,bat,exe}\` in
\`gili\$pathOTB\` (binDir). \#'
