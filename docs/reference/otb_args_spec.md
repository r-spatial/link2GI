# Return normalized OTB parameter spec for an application

Return normalized OTB parameter spec for an application

## Usage

``` r
otb_args_spec(algo, gili = NULL)
```

## Arguments

- algo:

  Character. OTB application name.

- gili:

  Optional list returned by \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

## Value

data.frame with columns: key, type, mandatory, has_pixel, pixel_default,
has_default, default, class, desc
