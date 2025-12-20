# convenient function to establish all link2GI links

brute force search, find and linkl of all link2GI link functions. This
is helpfull if yor system is well setup and the standard linkage
procedure will provide the correct linkages.

## Usage

``` r
linkAll(
  links = NULL,
  simple = TRUE,
  linkItems = c("saga", "grass", "otb", "gdal"),
  sagaArgs = "default",
  grassArgs = "default",
  otbArgs = "default",
  gdalArgs = "default",
  quiet = FALSE
)
```

## Arguments

- links:

  character. links

- simple:

  logical. true make all

- linkItems:

  character. list of c('saga','grass','otb','gdal')

- sagaArgs:

  character. full string of sagaArgs

- grassArgs:

  character. grassArgs full string of grassArgs

- otbArgs:

  character. full string of otbArgs

- gdalArgs:

  character. full string of gdalArgs

- quiet:

  supress all messages default is FALSE

## Note

You may also use the full list of arguments that is made available from
the `link2GI` package, but it is strongly recommended in this case to
use directly the single linkage functions from `link2GI`.

## Examples

``` r
if (FALSE) { # \dontrun{
# required packages
require(link2GI)

# search, find and create the links to all supported  GI software
giLinks<-linkAll()

# makes the GDAL linkage verbose
giLinks<-linkAll(gdalArgs= 'quiet = TRUE') 

} # }
```
