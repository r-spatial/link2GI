# Parse the "Parameters:" block from OTB -help output

Internal helper to extract parameter definitions from the standard
"Parameters:" section of OTB CLI help output.

## Usage

``` r
.otb_parse_parameters_block(txt)
```

## Arguments

- txt:

  Character vector returned by
  `.otb_run_launcher(gili, c(algo, "-help"), ...)`.

## Value

A `data.frame` with columns:

- key:

  Parameter key without leading `-`.

- missing:

  Logical. TRUE if prefixed with `MISSING`.

- mandatory:

  Logical. TRUE if marked `(mandatory)`.

- type:

  Type string extracted from angle brackets.

- has_pixel:

  Logical. TRUE if parameter has a `[pixel]` qualifier.

- default:

  Parameter default value (if any).

- pixel_default:

  Default pixel type (only for `[pixel]` outputs).

- desc:

  Full human-readable description line.

## Details

The parser handles the following OTB conventions:

- mandatory parameters marked by the prefix `MISSING`

- mandatory parameters marked by `(mandatory)` in the description

- output parameters with pixel type specification `[pixel]`

- separation of parameter defaults from pixel-type defaults
