# Compile folder list and create folders

Compile folder list with absolut paths and create folders if necessary.

## Usage

``` r
createFolders(root_folder, folders, create_folders = TRUE)
```

## Arguments

- root_folder:

  root directory of the project.

- folders:

  list of subfolders within the project directory.

- create_folders:

  create folders if not existing already.

## Value

List with folder paths and names.

## Examples

``` r
if (FALSE) { # \dontrun{
 createFolders(root_folder = tempdir(), folders = c('data/', 'data/tmp/'))
} # }
# Create folder list and set variable names pointing to the path values
```
