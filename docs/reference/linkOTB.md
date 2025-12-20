# Locate and describe 'Orfeo ToolBox' (OTB) API bindings

Locate a valid [Orfeo ToolBox](https://www.orfeo-toolbox.org/)
installation and return the relevant paths and metadata.

## Usage

``` r
linkOTB(
  bin_OTB = NULL,
  root_OTB = NULL,
  type_OTB = NULL,
  searchLocation = NULL,
  ver_select = FALSE,
  quiet = TRUE,
  returnPaths = TRUE
)
```

## Arguments

- bin_OTB:

  String. Path to where the OTB binaries are located.

- root_OTB:

  String. Full path to the OTB root folder containing the installation.
  If provided, it is also used to resolve the environment initialisation
  script on Windows.

- type_OTB:

  String. Optional installation type filter (e.g. OSGeo4W/QGIS).

- searchLocation:

  String. Hard drive letter (Windows) or mounting point (Linux). Default
  for Windows is `C:/`, default for Linux is `/usr/bin/`.

- ver_select:

  Logical or numeric. Default is `FALSE`. If there is more than one OTB
  installation and `ver_select = TRUE`, the user can interactively
  select the preferred OTB version. If `ver_select = FALSE`, the latest
  version is automatically selected. If numeric `> 0`, the corresponding
  row index of the detected installations is selected.

- quiet:

  Logical. Switch for suppressing messages. Default is `TRUE`.

- returnPaths:

  Logical. If set to `FALSE`, nothing is returned. If `TRUE`, the
  selected OTB paths and detected versions are returned.

## Value

A list with at least:

- `exist`: logical, whether a valid installation was found

- `pathOTB`: path to the selected OTB `bin` directory

- `otbCmd`: best-effort base command as detected

- `version`: data.frame of detected installations

- `otbRoot`: inferred OTB root directory (one level above `bin`)

- `envScript`: best-effort path to `otbenv.profile` / `otbenv.bat` if
  found, otherwise `NA_character_`

## Details

The function searches for an OTB command line installation and selects
one version if multiple installations are found. Historically,
`linkOTB()` also prepared the current R session by adding OTB paths to
the environment.

In the current implementation, `linkOTB()` is intended to be
*non-invasive* on all platforms: it returns paths and metadata,
including the inferred OTB root directory and a best-effort path to the
OTB environment initialisation script (`otbenv.profile` on Linux,
`otbenv.bat` on Windows), but does not modify `PATH` or other
environment variables.

It looks for the `otbcli.bat` file on Windows. If the file is found in a
`bin` folder it is assumed to be a valid OTB binary installation.

## Note

You may also set the path manually. Using an 'OSGeo4W64'
<https://trac.osgeo.org/osgeo4w/> installation it is typically
`C:/OSGeo4W64/bin/`.

## Author

Chris Reudenbach

## Examples

``` r
if (FALSE) { # \dontrun{
# call if you do not have any idea if and where OTB is installed
otb <- linkOTB()
if (otb$exist) {
  print(otb)
}
} # }
```
