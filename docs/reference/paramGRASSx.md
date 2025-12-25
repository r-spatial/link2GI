# Usually for internally usage, get 'GRASS GIS' and `rgrass` parameters on 'Linux' OS

Initialize and set up `rgrass` for 'Linux'

## Usage

``` r
paramGRASSx(
  set_default_GRASS = NULL,
  MP = "/usr/bin",
  ver_select = FALSE,
  quiet = TRUE
)
```

## Arguments

- set_default_GRASS:

  default is NULL. will force a search for 'GRASS GIS' You may provide a
  valid combination as c('/usr/lib/grass83','8.3.2','grass')

- MP:

  default is '/usr/bin'. mount point to be searched.

- ver_select:

  if TRUE you must interactively select between alternative
  installations

- quiet:

  boolean, default is TRUE. switch for suppressing console messages

## Details

During the rsession you will have full access to GRASS GIS via the
`rgrass` wrapper.
