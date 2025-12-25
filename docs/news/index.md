# Changelog

## link2GI 0.7-3

**changes**

- Added deterministic GDAL CLI context using linked system binaries (no
  implicit PATH).
- Introduced GDAL fingerprinting, help-based skeletons, and NDJSON run
  logging for reproducibility.
- Refactored GDAL discovery/linking to be platform-robust and
  non-redundant.

## link2GI 0.7-3

**changes**

- Refactored platform discovery: GRASS, GDAL, and SAGA detection was
  unified and hardened with strict OS guards, consistent return
  structures, and namespace-local calls.
- Stabilized environment setup
- Tests & CRAN hygiene: replaced brittle/mock-heavy tests with
  meaningful coverage, fixed edge cases, and achieved clean R CMD

## link2GI 0.7-2

CRAN release: 2025-12-23

**changes**

- New Self-describing OTB API using CLI `-help` as source of truth  
- Robust command building
  ([`otb_build_cmd()`](https://r-spatial.github.io/link2GI/reference/otb_api.md),
  [`otb_set_out()`](https://r-spatial.github.io/link2GI/reference/otb_api.md)),
  unified execution  
- Bounded Windows + Linux OTB discovery aligned  
- Full CRAN compliance (`R CMD check` clean)  
- Legacy API kept for backward compatibility
- Added testthat coverage for the OTB wrapper (runOTB command
  construction and argument handling).

## link2GI 0.7-1

**bugfixes** *(Fix
[\#67](https://github.com/r-spatial/link2GI/issues/67))*

- fix unintended recursion in project setup logic
- refactor
  [`initProj()`](https://r-spatial.github.io/link2GI/reference/initProj.md)
  to a single, non-recursive execution path
- prevent repeated re-initialisation of folders, git, and `renv`

**documentation**

- update complete documentation to meet new concepts and calls

## link2GI 0.7-0

**changes**

- new Self-describing OTB API
  ([`otb_capabilities()`](https://r-spatial.github.io/link2GI/reference/otb_api.md),
  [`otb_args_spec()`](https://r-spatial.github.io/link2GI/reference/otb_api.md),
  [`otb_build_cmd()`](https://r-spatial.github.io/link2GI/reference/otb_api.md))

**bugfixes** *(Fix
[\#68](https://github.com/r-spatial/link2GI/issues/68))*

- fix OTB path and environment setup (bin/root handling)
- stabilize `-help` introspection (stdout, non-zero exit codes)
- fix
  [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md)
  output handling and error propagation

**documentation**

- update vignette to use introspected parameters instead of hard-coded
  lists

## link2GI 0.6-2

CRAN release: 2024-10-28

bugfixes:

- fix cran issues git2r

## link2GI 0.6-1

CRAN release: 2024-06-01

bugfixes:

- fix cran issues
- fixing of the vignettes and examples

changes:

- dropping of linkALL

## link2GI 0.6-0

bugfixes:

- issue [\#65](https://github.com/r-spatial/link2GI/issues/65)

changes:

- variable `rootDir` is deprecated and now `root_folder`

new feature:

- extension of initProj to a lightweight project setup tool

## link2GI 0.5-4

bugfixes:

- \#61 [\#62](https://github.com/r-spatial/link2GI/issues/62)
  [\#63](https://github.com/r-spatial/link2GI/issues/63)

## link2GI 0.5-3

CRAN release: 2023-10-30

bugfixes:

- \#59 , [\#58](https://github.com/r-spatial/link2GI/issues/58)
- fix rgrass7 dependencies

## link2GI 0.5-2

CRAN release: 2023-01-27

new feature:

- add return of vector data in OTB calls

bugfixes:

- \#58
- minor fixes in runOTB
- prelim fix of [\#57](https://github.com/r-spatial/link2GI/issues/57)
- fix of [\#56](https://github.com/r-spatial/link2GI/issues/56)
- minor fixes

new feature:

- integration of stars and terra raster objects for linking GRASS
- update of documentation

## link2GI 0.5-1

bugfixes:

- prelim fix of [\#57](https://github.com/r-spatial/link2GI/issues/57)
- fix of [\#56](https://github.com/r-spatial/link2GI/issues/56)
- minor fixes

new feature:

- integration of stars and terra raster objects for linking GRASS
- update of documentation

## link2GI 0.5-0

CRAN release: 2022-08-29

bugfixes:

- prelim fix of [\#56](https://github.com/r-spatial/link2GI/issues/56)

new feature:

- changing to linkGRASS for both GRASS 7.x/8.x - keeping linkGRASS7for
  backwards compatibility

## link2GI 0.4-7

CRAN release: 2021-09-03

bugfixes:

- \#52

## link2GI 0.4-6

bugfixes:

- pre fix [\#52](https://github.com/r-spatial/link2GI/issues/52)

## link2GI 0.4-5

CRAN release: 2020-12-07

bugfixes:

- fix [\#42](https://github.com/r-spatial/link2GI/issues/42),
  [\#44](https://github.com/r-spatial/link2GI/issues/44),
  [\#46](https://github.com/r-spatial/link2GI/issues/46),
  [\#48](https://github.com/r-spatial/link2GI/issues/48),
  [\#49](https://github.com/r-spatial/link2GI/issues/49)

new feature:

- add installation vignette
- update some documentation

## link2GI 0.4-4

bugfixes:

- fix [\#42](https://github.com/r-spatial/link2GI/issues/42)

## link2GI 0.4-3

bugfixes:

- fix [\#40](https://github.com/r-spatial/link2GI/issues/40)

## link2GI 0.4-2

bugfixes:

- fix [\#38](https://github.com/r-spatial/link2GI/issues/38)

## link2GI 0.4-1

new feature

- add linkall
- add direct reading and writing of vector data to GRASS sqlite

bugfixes:

- smaller bugfixes

## link2GI 0.4-0

CRAN release: 2019-11-22

new feature

- parse and return OTB API help for interactive use in R
- add runOTB function according to
  [\#29](https://github.com/r-spatial/link2GI/issues/29)

bugfixes:

- \#19 [\#21](https://github.com/r-spatial/link2GI/issues/21)
  [\#22](https://github.com/r-spatial/link2GI/issues/22)
  [\#26](https://github.com/r-spatial/link2GI/issues/26)
  [\#29](https://github.com/r-spatial/link2GI/issues/29) fixed
- first parsing of the new GRASS (since 7.8.x ) start file

## link2GI 0.3-7

CRAN release: 2019-09-05

new feature:

- improved implementation of parseOTB

bugfixes:

- several fixes
- examples and typos

## link2GI 0.3-6

new feature:

- full re-implementation of linkGDAL returns now all gdal installations
  at a given search path

bugfixes:

- several fixes for Windows search and linking
- examples and typos

## link2GI 0.3-5

CRAN release: 2018-10-26

new feature:

- add manual build function for UNC paths

bugfixes:

- remove search argument for PRZP
- several fixes for Windows search and linking
- Comments and typos

## link2GI 0.3-4

CRAN release: 2018-10-03

new feature:

- add boolean variable if required API exists

bugfixes:

- fix of the quiet option
- fix the search order
- straighten GRASS problems with r.in.lidar
- several fixes for Windows search and linking
- fix and add a lot of examples
- Comments and typos

## link2GI 0.3-3

CRAN release: 2018-09-22

new feature:

- by default the latest version of each API is selected

bugfixes:

- OTB basic wrapper fixed for Windows
- new examples
- Typos

## link2GI 0.3-2

CRAN release: 2018-08-23

new feature:

- ver_select can be used directly with the number of selection
- OTB basic wrapper

bugfixes:

- RSAGA related fixes
- Typos

## link2GI 0.3-1

new feature:

- add returnPaths argument for suppress all returns

bugfixes:

- windows RSAGA related fixes

## link2GI 0.3-0

CRAN release: 2018-02-11

The 0.3-0 version is quite restructured to conform to both common R
conventions (e.g. no hidden globals) and to make the calls more
intuitive and one-line-like. some of the function follow a slightly
different naming and argument setting. This was done to keep in line
with further CLI linking functionality.

new feature:

- Linux support for multi-version selection
- remove all global variables (re-opened
  [\#3](https://github.com/r-spatial/link2GI/issues/3))
- homogenization of the link calls
- wrap searchAPIOS functions with OS-agnostic findAPI functions
- add a more robust return of all paths and environmental variables as
  lists
- add a quiet option to supress all! console outputs (
  [\#4](https://github.com/r-spatial/link2GI/issues/4))

bugfixes:

- remove tailing backslashes
- force compatibility to RSAGA and SAGA ver 3+

## link2GI 0.2-2

new feature:

- findGRASS a function that wraps for searchGRASSW and searchGRASSX

## link2GI 0.2-1

CRAN release: 2018-02-06

bugfixes:

- fix github s [\#2](https://github.com/r-spatial/link2GI/issues/2)
  [\#3](https://github.com/r-spatial/link2GI/issues/3)
  [\#5](https://github.com/r-spatial/link2GI/issues/5)

## link2GI 0.2-0

CRAN release: 2018-01-28

bugfixes:

- fix several small bugs

## link2GI 0.1-0

CRAN release: 2017-01-22

- Initial release
