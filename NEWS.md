## link2GI 0.3-8

bugfixes:
* issue 22 fixed
* first parsing of the new GRASS (since 7.8.x ) start file 

## link2GI 0.3-7
new feature: 
* improved implementation of parseOTB
bugfixes:
* several issues fixes
* examples and typos

## link2GI 0.3-6
new feature: 
* full re-implementation of linkGDAL returns now all gdal installations at a given search path
bugfixes:
* several fixes for Windows search and linking
* examples and typos

## link2GI 0.3-5
new feature: 
* add manual build function for UNC pathes
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
* remove all global variables (re-opened issue #3)
* homogenization of the link calls
* wrap searchAPIOS functions with OS-agnostic findAPI functions
* add a more robust return of all pathes and environmental variables as lists 
* add a quiet option to supress all! console outputs (issue #4)
  
bugfixes:
* remove tailing backslashes 
* force compatibility to RSAGA and SAGA ver 3+

## link2GI 0.2-2
new feature:
* findGRASS a function that wraps for searchGRASSW and searchGRASSX
  
## link2GI 0.2-1

bugfixes:
* fix github issues #2 #3 #5 
  
## link2GI 0.2-0

bugfixes:
* fix several small bugs

## link2GI 0.1-0

* Initial release
