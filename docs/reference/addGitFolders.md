# Extent folder list by git repository

Extent folder list by git repository and create subdirectories according
to default values.

## Usage

``` r
addGitFolders(folders, git_repository = NULL, git_subfolders = NULL)
```

## Arguments

- folders:

  list of subfolders within the project directory.

## Examples

``` r
if (FALSE) { # \dontrun{
addGitFolders(folders = c('data', 'data/tmp'), git_repository = 'myproject')
} # }
```
