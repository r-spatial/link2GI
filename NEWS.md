## link2GI 0.7-0

changes:

* introduction of a strict, introspection-based OTB workflow (`otb_capabilities()`, `otb_args_spec()`, `otb_build_cmd()`, `otb_set_out()`) as the recommended API for robust, version-stable OTB calls
* clarification and consolidation of the OTB execution model:

  * explicit separation between capability discovery (`-help` parsing) and execution (`runOTB`)
  * explicit write-to-disk policy for OTB outputs
* update and extension of vignettes with a fully transparent OTB introspection example (`DimensionalityReduction`, PCA)

bugfixes:

* fix OTB launcher path construction when `linkOTB()` returns `pathOTB` as a `.../bin/` directory
* fix OTB environment variable setup by consistently deriving all paths from the OTB root directory (`otbRoot`)
* restore and stabilize stdout/stderr capture for OTB `-help` calls used in introspection
* tolerate non-zero exit codes returned by some OTB builds for `-help` calls if a valid `Parameters:` block is present
* fix `runOTB()` handling of OTB `[pixel]` outputs where output arguments are vectors (e.g. `c(path, "float")`)
* prevent invalid read-back attempts by explicitly checking output existence before raster/vector import
* improve error reporting in `runOTB()` to avoid downstream length-coercion and “file does not exist” errors

documentation:

* update OTB wrapper vignette to demonstrate:

  * how parameter keys are obtained from `otb_args_spec()` instead of hard-coded fallback lists
  * how to construct version-stable OTB calls without heuristics or hidden defaults


## link2GI 0.6-2

bugfixes:

* fix cran issues git2r



## link2GI 0.6-1

bugfixes:

* fix cran issues
* fixing of the vignettes and examples

changes:

* dropping of linkALL 

## link2GI 0.6-0

bugfixes:

* issue #65

changes:

* variable `rootDir` is deprecated and now `root_folder`

new feature:

* extension of initProj to a lightweight project setup tool


## link2GI 0.5-4

bugfixes:

*  #61 #62 #63


## link2GI 0.5-3

bugfixes:

*  #59 , #58
* fix rgrass7 dependencies

## link2GI 0.5-2

new feature:

* add return of vector data in OTB calls

bugfixes:

*  #58
* minor fixes in runOTB
* prelim fix of  #57
* fix of  #56
* minor fixes

new feature:

* integration of stars and terra raster objects for linking GRASS
* update of documentation

## link2GI 0.5-1

bugfixes:

* prelim fix of  #57
* fix of  #56
* minor fixes

new feature:

* integration of stars and terra raster objects for linking GRASS
* update of documentation

## link2GI 0.5-0

bugfixes:

* prelim fix of  #56

new feature:

* changing to linkGRASS for both GRASS 7.x/8.x - keeping linkGRASS7for backwards compatibility 


## link2GI 0.4-7

bugfixes:

*  #52

## link2GI 0.4-6

bugfixes:

* pre fix  #52

## link2GI 0.4-5

bugfixes:

* fix  #42, #44, #46, #48, #49

new feature:

* add installation vignette
* update some documentation

## link2GI 0.4-4

bugfixes:

* fix  #42

## link2GI 0.4-3

bugfixes:

* fix  #40 

## link2GI 0.4-2

bugfixes:

* fix  #38


## link2GI 0.4-1

new feature

* add linkall
* add direct reading and writing of vector data to GRASS sqlite

bugfixes:

* smaller bugfixes

## link2GI 0.4-0

new feature

* parse and return OTB API help for interactive use in R
* add runOTB function according to #29

bugfixes:

*  #19 #21 #22 #26 #29 fixed
* first parsing of the new GRASS (since 7.8.x ) start file 

## link2GI 0.3-7

new feature: 

* improved implementation of parseOTB

bugfixes:

* several fixes
* examples and typos

## link2GI 0.3-6

new feature: 

* full re-implementation of linkGDAL returns now all gdal installations at a given search path

bugfixes:

* several fixes for Windows search and linking
* examples and typos

## link2GI 0.3-5

new feature: 

* add manual build function for UNC paths

bugfixes:

* remove search argument for PRZP
* several fixes for Windows search and linking
* Comments and typos

## link2GI 0.3-4

new feature: 

* add boolean variable if required API exists

bugfixes:

* fix of the quiet option
* fix the search order
* straighten GRASS problems with r.in.lidar
* several fixes for Windows search and linking
* fix and add a lot of examples
* Comments and typos


## link2GI 0.3-3

new feature: 

* by default the latest version of each API is selected

bugfixes:

* OTB basic wrapper fixed for Windows
* new examples
* Typos

## link2GI 0.3-2

new feature: 

* ver_select can be used directly with the number of selection 
* OTB basic wrapper

bugfixes:

* RSAGA related fixes
* Typos

## link2GI 0.3-1

new feature: 

* add returnPaths argument for suppress all returns

bugfixes:

* windows RSAGA related fixes

## link2GI 0.3-0

The 0.3-0  version is quite restructured to conform to both 
common R conventions (e.g. no hidden globals) and to make the calls 
more intuitive and one-line-like. some of the function follow a slightly 
different naming and argument setting. This was done to keep in line with further 
CLI linking functionality.

new feature:

* Linux support for multi-version selection
* remove all global variables (re-opened  #3)
* homogenization of the link calls
* wrap searchAPIOS functions with OS-agnostic findAPI functions
* add a more robust return of all paths and environmental variables as lists 
* add a quiet option to supress all! console outputs ( #4)
  
bugfixes:

* remove tailing backslashes 
* force compatibility to RSAGA and SAGA ver 3+

## link2GI 0.2-2

new feature:

* findGRASS a function that wraps for searchGRASSW and searchGRASSX
  
## link2GI 0.2-1

bugfixes:

* fix github s #2 #3 #5 
  
## link2GI 0.2-0

bugfixes:

* fix several small bugs

## link2GI 0.1-0

* Initial release
