# Write sf object directly to \`GRASS\` vector utilising an existing or creating a new GRASS environment

Write sf object directly to \`GRASS\` vector utilising an existing or
creating a new GRASS environment

## Usage

``` r
sf2gvec(x, epsg, obj_name, gisdbase, location, gisdbase_exist = FALSE)
```

## Arguments

- x:

  `sf` object corresponding to the settings of the corresponding GRASS
  container

- epsg:

  numeric epsg code

- obj_name:

  name of GRASS layer

- gisdbase:

  GRASS gisDbase folder

- location:

  GRASS location name containing `obj_name)`

- gisdbase_exist:

  logical switch if the GRASS gisdbase folder exist default is TRUE

## Note

have a look at the `sf` capabilities to write direct to sqlite

## Author

Chris Reudenbach

## Examples

``` r
run = FALSE
if (run) {
## example 
require(sf)
require(sp)
require(link2GI)
data(meuse)
meuse_sf = st_as_sf(meuse, 
                    coords = c('x', 'y'), 
                    crs = 28992, 
                    agr = 'constant')


# write data to GRASS and create gisdbase
sf2gvec(x = meuse_sf,
        obj_name = 'meuse_R-G',
        gisdbase = '~/temp3/',
        location = 'project1')

# read from existing GRASS          
gvec2sf(x = meuse_sf,
        obj_name = 'meuse_r_g',
        gisdbase = '~/temp3',       
        location = 'project1')

}
```
