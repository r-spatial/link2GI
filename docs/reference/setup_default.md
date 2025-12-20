# Define working environment default settings

Define working environment default settings

## Usage

``` r
setup_default(
  default = NULL,
  new_folder_list = NULL,
  new_folder_list_name = NULL
)
```

## Arguments

- default:

  name of default list

- new_folder_list:

  containing a list of arbitrary folders to be generated

- new_folder_list_name:

  name of this list

## Value

A list containing the default project settings

## Details

After adding new project settings run \[setup_default()\] to update and
savew the default settings. For compatibility reasons you may also run
\[lutUpdate()\].

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard setup for baseSpatial
setup_default()
} # }
```
