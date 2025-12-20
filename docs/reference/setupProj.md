# Setup project folder structure

Defines folder structures and creates them if necessary, loads
libraries, and sets other project relevant parameters.

## Usage

``` r
setupProj(
  root_folder = tempdir(),
  folders = c("data", "data/tmp"),
  code_subfolder = NULL,
  global = FALSE,
  libs = NULL,
  setup_script = "000_setup.R",
  fcts_folder = NULL,
  source_functions = !is.null(fcts_folder),
  standard_setup = NULL,
  create_folders = TRUE
)
```

## Arguments

- root_folder:

  root directory of the project.

- folders:

  list of sub folders within the project directory.

- code_subfolder:

  sub folders for scripts and functions within the project directory
  that will be created. The folders src, src/functions and src/config
  are recommended.

- global:

  logical: export path strings as global variables?

- libs:

  vector with the names of libraries

- setup_script:

  Name of the installation script that contains all the settings
  required for the project, such as additional libraries, optional
  settings, colour schemes, etc. Important: It should not be used to
  control the runtime parameters of the scripts. This file is not read
  in automatically, even if it is located in the 'fcts_folder' folder.

- fcts_folder:

  path of the folder holding the functions. All files in this folder
  will be sourced at project start.

- source_functions:

  logical: should functions be sourced? Default is TRUE if fcts_folder
  exists.

- standard_setup:

  select one of the predefined settings c('base', 'baseSpatial',
  'advancedSpatial'). In this case, only the name of the base folder is
  required, but individual additional folders can be specified under
  'folders' name of the git repository must be supplied to the function.

- create_folders:

  default is TRUE so create folders if not existing already.

## Value

A list containing the project settings.

## Examples

``` r
if (FALSE) { # \dontrun{
setupProj(
  root_folder = '~/edu', folders = c('data/', 'data/tmp/'),
  libs = c('link2GI')
)
} # }
```
