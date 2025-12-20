# Create files or scripts from templates

Create files or scripts from brew templates supplied with the package.

## Usage

``` r
createScript(
  new_file = file.path(tempdir(), "tmp.R"),
  template = c("script_function", "script_control"),
  notes = TRUE,
  template_path = system.file(sprintf("templates/%s.brew", template[1]), package =
    "link2GI")
)
```

## Arguments

- new_file:

  name of the file to be created

- template:

  template to be used for the new file ('script_function',
  'script_control')

- notes:

  logical: include notes from the template in the file

- template_path:

  path to template to be used

## Examples

``` r
if (FALSE) { # \dontrun{
createScript()
} # }
```
