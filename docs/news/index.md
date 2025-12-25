# Changelog

## link2GI 0.7-3

### Changes

- Added a deterministic GDAL CLI context based on linked system binaries
  (no implicit PATH usage).
- Introduced GDAL capability fingerprinting, help-derived minimal CLI
  skeletons, and NDJSON run logging to support reproducibility.
- Refactored GDAL discovery and linking to be platform-robust and
  non-redundant.

## link2GI 0.7-3

### Changes

- Unified and hardened platform discovery for GRASS, GDAL, and SAGA with
  strict OS guards and consistent return structures.
- Stabilized environment setup and binary resolution across platforms.
- Improved test suite and CRAN compliance; removed brittle mocks and
  fixed edge cases.

## link2GI 0.7-2

CRAN release: 2025-12-23

### Changes

- Introduced a new introspection-based OTB API using CLI `-help` output
  as the single source of truth.
- Added robust command construction and unified OTB execution logic.
- Harmonized Windows and Linux OTB discovery.
- Preserved the legacy OTB API for backward compatibility.

### Testing

- Added `testthat` coverage for OTB command construction and argument
  handling.

## link2GI 0.7-1

### Bug fixes

- Fixed unintended recursion in project setup logic.
- Refactored
  [`initProj()`](https://r-spatial.github.io/link2GI/reference/initProj.md)
  to a single, non-recursive execution path.
- Prevented repeated re-initialisation of folders, git, and `renv`.

### Documentation

- Updated documentation to reflect the revised project setup behaviour.

## link2GI 0.7-0

### Changes

- Added a new introspection-based OTB API
  ([`otb_capabilities()`](https://r-spatial.github.io/link2GI/reference/otb_api.md),
  [`otb_args_spec()`](https://r-spatial.github.io/link2GI/reference/otb_api.md),
  [`otb_build_cmd()`](https://r-spatial.github.io/link2GI/reference/otb_api.md)).

### Bug fixes

- Fixed OTB path and environment setup (bin/root handling).
- Stabilized CLI help introspection, including non-zero exit codes.
- Improved
  [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md)
  output handling and error propagation.

### Documentation

- Updated vignette to use introspected parameters instead of hard-coded
  argument lists.

## link2GI 0.6-2

CRAN release: 2024-10-28

### Bug fixes

- Fixed CRAN issues related to `git2r`.

## link2GI 0.6-1

CRAN release: 2024-06-01

### Bug fixes

- Fixed additional CRAN check issues.

## link2GI 0.6-0

### Changes

- Deprecated `rootDir` in favour of `root_folder`.
- Extended
  [`initProj()`](https://r-spatial.github.io/link2GI/reference/initProj.md)
  into a lightweight project setup utility.

### Bug fixes

- Fixed issue [\#65](https://github.com/r-spatial/link2GI/issues/65).

## link2GI 0.5-4

### Bug fixes

- Fixed issues [\#61](https://github.com/r-spatial/link2GI/issues/61),
  [\#62](https://github.com/r-spatial/link2GI/issues/62), and
  [\#63](https://github.com/r-spatial/link2GI/issues/63).

## link2GI 0.5-3

CRAN release: 2023-10-30

### Bug fixes

- Fixed issues [\#58](https://github.com/r-spatial/link2GI/issues/58)
  and [\#59](https://github.com/r-spatial/link2GI/issues/59).
- Stabilized `rgrass7` dependencies.

## link2GI 0.5-2

CRAN release: 2023-01-27

### Changes

- Added support for returning vector data from OTB calls.

### Bug fixes

- Fixed issue [\#58](https://github.com/r-spatial/link2GI/issues/58).
- Minor fixes in
  [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md).

## link2GI 0.5-1

### Changes

- Integrated `stars` and `terra` raster objects for GRASS linking.

### Bug fixes

- Preliminary fixes for issues
  [\#56](https://github.com/r-spatial/link2GI/issues/56) and
  [\#57](https://github.com/r-spatial/link2GI/issues/57).
- Minor internal fixes.

## link2GI 0.5-0

CRAN release: 2022-08-29

### Changes

- Switched to a unified
  [`linkGRASS()`](https://r-spatial.github.io/link2GI/reference/linkGRASS.md)
  for GRASS 7.x and 8.x.
- Retained `linkGRASS7()` for backward compatibility.

### Bug fixes

- Preliminary fix for issue
  [\#56](https://github.com/r-spatial/link2GI/issues/56).

## link2GI 0.4-7

CRAN release: 2021-09-03

### Bug fixes

- Fixed issue [\#52](https://github.com/r-spatial/link2GI/issues/52).

## link2GI 0.4-6

### Bug fixes

- Preliminary fix for issue
  [\#52](https://github.com/r-spatial/link2GI/issues/52).

## link2GI 0.4-5

CRAN release: 2020-12-07

### Bug fixes

- Fixed issues [\#42](https://github.com/r-spatial/link2GI/issues/42),
  [\#44](https://github.com/r-spatial/link2GI/issues/44),
  [\#46](https://github.com/r-spatial/link2GI/issues/46),
  [\#48](https://github.com/r-spatial/link2GI/issues/48), and
  [\#49](https://github.com/r-spatial/link2GI/issues/49).

### Documentation

- Added installation vignette.
- Updated documentation.

## link2GI 0.4-4

### Bug fixes

- Fixed issue [\#42](https://github.com/r-spatial/link2GI/issues/42).

## link2GI 0.4-3

### Bug fixes

- Fixed issue [\#40](https://github.com/r-spatial/link2GI/issues/40).

## link2GI 0.4-2

### Bug fixes

- Fixed issue [\#38](https://github.com/r-spatial/link2GI/issues/38).

## link2GI 0.4-1

### Changes

- Added `linkall()`.
- Added direct reading and writing of vector data to GRASS SQLite.

### Bug fixes

- Several minor fixes.

## link2GI 0.4-0

CRAN release: 2019-11-22

### Changes

- Added parsing and return of OTB CLI help for interactive use in R.
- Introduced
  [`runOTB()`](https://r-spatial.github.io/link2GI/reference/runOTB.md)
  according to issue
  [\#29](https://github.com/r-spatial/link2GI/issues/29).

### Bug fixes

- Fixed issues [\#19](https://github.com/r-spatial/link2GI/issues/19),
  [\#21](https://github.com/r-spatial/link2GI/issues/21),
  [\#22](https://github.com/r-spatial/link2GI/issues/22),
  [\#26](https://github.com/r-spatial/link2GI/issues/26), and
  [\#29](https://github.com/r-spatial/link2GI/issues/29).
- First parsing of the new GRASS (\>= 7.8) start file.

## link2GI 0.3-7

CRAN release: 2019-09-05

### Changes

- Improved implementation of `parseOTB()`.

### Bug fixes

- Several fixes.
- Updated examples and corrected typos.

## link2GI 0.3-6

### Changes

- Full re-implementation of
  [`linkGDAL()`](https://r-spatial.github.io/link2GI/reference/linkGDAL.md),
  now returning all GDAL installations at a given search path.

### Bug fixes

- Multiple fixes for Windows search and linking.
- Updated examples and typos.

## link2GI 0.3-5

CRAN release: 2018-10-26

### Changes

- Added manual build function for UNC paths.

### Bug fixes

- Removed obsolete search argument for PRZP.
- Multiple fixes for Windows search and linking.
- Documentation improvements.

## link2GI 0.3-4

CRAN release: 2018-10-03

### Changes

- Added boolean indicator for required API availability.

### Bug fixes

- Fixed quiet option behaviour.
- Fixed search order.
- Resolved GRASS issues with `r.in.lidar`.
- Multiple Windows-related fixes.
- Added and corrected examples.

## link2GI 0.3-3

CRAN release: 2018-09-22

### Changes

- By default, the latest installed version of each API is selected.

### Bug fixes

- Fixed OTB basic wrapper on Windows.
- Added new examples.
- Corrected typos.

## link2GI 0.3-2

CRAN release: 2018-08-23

### Changes

- `ver_select` can now directly select by index.
- Added OTB basic wrapper.

### Bug fixes

- RSAGA-related fixes.
- Corrected typos.

## link2GI 0.3-1

### Changes

- Added `returnPaths` argument to suppress return values.

### Bug fixes

- Windows RSAGA-related fixes.

## link2GI 0.3-0

CRAN release: 2018-02-11

### Changes

- Major restructuring to align with common R conventions.
- Removed hidden global variables.
- Added Linux support for multi-version selection.
- Homogenized linking functions and OS-agnostic `find*()` wrappers.
- Added robust environment handling and a global quiet option.

### Bug fixes

- Removed trailing backslashes.
- Ensured compatibility with RSAGA and SAGA \>= 3.

## link2GI 0.2-2

### Changes

- Added
  [`findGRASS()`](https://r-spatial.github.io/link2GI/reference/findGRASS.md)
  as a wrapper for
  [`searchGRASSW()`](https://r-spatial.github.io/link2GI/reference/searchGRASSW.md)
  and
  [`searchGRASSX()`](https://r-spatial.github.io/link2GI/reference/searchGRASSX.md).

## link2GI 0.2-1

CRAN release: 2018-02-06

### Bug fixes

- Fixed issues [\#2](https://github.com/r-spatial/link2GI/issues/2),
  [\#3](https://github.com/r-spatial/link2GI/issues/3), and
  [\#5](https://github.com/r-spatial/link2GI/issues/5).

## link2GI 0.2-0

CRAN release: 2018-01-28

### Bug fixes

- Fixed several small bugs.

## link2GI 0.1-0

CRAN release: 2017-01-22

### Changes

- Initial CRAN release.
