# Identifies SAGA GIS Installations and returns linking Informations

Finds the existing [SAGA GIS](https://saga-gis.sourceforge.io/)
installation(s), generates and sets the necessary path and system
variables for a seamless use of the command line calls of the 'SAGA GIS'
CLI API, setup valid system variables for calling a default `rsaga.env`
and by this makes available the `RSAGA` wrapper functions.  
All existing installation(s) means that it looks for the `saga_cmd` or
`saga_cmd.exe` executables. If the file is found it is assumed to be a
valid 'SAGA GIS' installation. If it is called without any argument the
most recent (i.e. highest) SAGA GIS version will be linked.

## Usage

``` r
linkSAGA(
  default_SAGA = NULL,
  searchLocation = "default",
  ver_select = FALSE,
  quiet = TRUE,
  returnPaths = TRUE
)
```

## Arguments

- default_SAGA:

  string contains path to `RSAGA` binaries

- searchLocation:

  drive letter to be searched, for Windows systems default is `C:`, for
  Linux systems default is `/usr/bin`.

- ver_select:

  boolean default is FALSE. If there is more than one 'SAGA GIS'
  installation and `ver_select` = TRUE the user can select interactively
  the preferred 'SAGA GIS' version

- quiet:

  boolean switch for supressing console messages default is TRUE

- returnPaths:

  boolean if set to FALSE the paths of the selected version are written
  to the PATH variable only, otherwise all paths and versions of the
  installed SAGA versions ae returned.#'@details If called without any
  parameter `linkSAGA()` it performs a full search over `C:`. If it
  finds one or more 'SAGA GIS' binaries it will take the first hit. You
  have to set `ver_select = TRUE` for an interactive selection of the
  preferred version. Additionally the selected SAGA paths are added to
  the environment and the global variables `sagaPath`, `sagaModPath` and
  `sagaCmd` will be created.

## Value

A list containing the selected `RSAGA` path variables
`$sagaPath`,`$sagaModPath`,`$sagaCmd` and potentially other
installations `$installed`

## Note

The 'SAGA GIS' wrapper [RSAGA](https://CRAN.R-project.org/package=RSAGA)
package was updated several times however it covers currently (May 2014)
only 'SAGA GIS' versions from 2.3.1 LTS - 8.4.1 The fast evolution of
'SAGA GIS' makes it highly impracticable to keep the wrapper adaptions
in line (currently 9.4). `RSAGA` will meet all linking needs perfectly
if you use 'SAGA GIS' versions from 2.0.4 - 7.5.0.  
However you must call `rsaga.env` using the
`rsaga.env(modules = saga$sagaModPath)` assuming that `saga` contains
the returnPaths of `linkSAGA` In addition the very promising
[Rsagacmd](https://github.com/stevenpawley/Rsagacmd) wrapper package is
providing a new list oriented wrapping tool.

## Examples

``` r
if (FALSE) { # \dontrun{

# call if you do not have any idea if and where SAGA GIS is installed
# it will return a list with the selected and available SAGA installations
# it prepares the system for running the selected SAGA version via RSAGA or CLI
linkSAGA()

# overriding the default environment of rsaga.env call 

saga<-linkSAGA()
if (saga$exist) {
require(RSAGA)
RSAGA::rsaga.env(path = saga$installed$binDir[1],modules = saga$installed$moduleDir[1])
}
} # }
```
