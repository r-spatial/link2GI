# Link GI to R

## What is link2GI?

The [link2GI](https://CRAN.R-project.org/package=link2GI) package
provides a lightweight linking framework that simplifies access to
external GIS and remote-sensing software from R. Its primary purpose is
to establish **reliable, reproducible connections to command-line
interfaces** of **GRASS GIS**, **SAGA GIS**, **Orfeo Toolbox (OTB)**,
and **GDAL**, without requiring detailed operating-system knowledge.

The package originated from repeated teaching and research scenarios in
graduate-level GIS and remote-sensing courses. In restrictive university
environments—particularly on Windows systems with limited user
privileges—the configuration of external GIS software often becomes the
dominant obstacle, long before any actual spatial analysis can begin.
*link2GI* addresses this problem by automating discovery, configuration,
and environment setup.

------------------------------------------------------------------------

## Why link2GI?

R provides a mature and powerful ecosystem for spatial data analysis.
Vector data are primarily handled by
[sf](https://CRAN.R-project.org/package=sf) packages, while raster
workflows are covered by
[terra](https://CRAN.R-project.org/package=terra) and
[stars](https://CRAN.R-project.org/package=stars). Specialized formats
such as NetCDF are well supported by packages like
[ncdf4](https://CRAN.R-project.org/package=ncdf4).

Spatial analysis in R is frequently extended via wrapper packages that
expose external libraries or algorithms through R-like interfaces, for
example [geosphere](https://CRAN.R-project.org/package=geosphere),
[Distance](https://CRAN.R-project.org/package=Distance),
[igraph](https://CRAN.R-project.org/package=igraph), or
[spatstat](https://CRAN.R-project.org/package=spatstat). A comprehensive
overview of this ecosystem is provided by [Geocomputation with
R](https://r.geocompx.org/).

Despite this richness, a practical gap remains between R and established
open-source GIS and remote-sensing systems. Software such as **QGIS**,
**GRASS GIS**, **SAGA GIS**, and **OTB** provide large collections of
mature, optimized, and well-validated algorithms that are not fully
replicated within R packages.

Bridging this gap is possible through dedicated wrapper packages. For
**GRASS GIS (7/8)**, this role is served by
[rgrass](https://CRAN.R-project.org/package=rgrass). For **SAGA GIS**,
[RSAGA](https://CRAN.R-project.org/package=RSAGA) and
[Rsagacmd](https://github.com/stevenpawley/Rsagacmd) are available.
However, for the **Orfeo Toolbox**, no comprehensive and actively
maintained R wrapper exists, despite its importance for large-scale
remote-sensing workflows.

**GDAL** occupies a special position: although its functionality is
widely accessible through R packages such as `sf` and `terra`, these
interfaces are bound to the **GDAL** version used at build time and do
not expose the full command-line toolchain. As a result, direct access
to the system-installed **GDAL** binaries—often required for advanced
formats, drivers, or reproducible batch workflows—remains fragmented and
poorly standardized in R.

Consequently, there is no lightweight, version-agnostic R interface that
systematically exposes the **GDAL** command-line utilities while
preserving transparency about the actually used binaries. This gap
becomes particularly relevant in heterogeneous or multi-user
environments, where multiple **GDAL** installations coexist and
reproducibility depends on explicit control over the underlying
toolchain.

In practice, users often encounter technical barriers related to
operating systems, library dependencies, and version mismatches. **GRASS
GIS**, in particular, requires a strictly defined spatial database and a
carefully configured runtime environment. From within R, setting up such
an environment—especially on Windows—can be disproportionately complex
when the goal is simply to run a single algorithm.

------------------------------------------------------------------------

## What does “linking” mean?

In the context of *link2GI*, *linking* refers to the automated
detection, configuration, and validation of all environment variables
and paths required to access external GIS software via their
command-line interfaces.

The package does **not** reimplement GIS algorithms and does **not**
replace existing wrappers. Instead, it prepares a consistent and
explicit execution environment so that existing tools can be called
**directly, reproducibly, and transparently** from R.

*link2GI* analyses available installations, identifies compatible
versions, and establishes either temporary or persistent environments
that comply with the requirements of the respective software. Where
possible, it integrates seamlessly with established wrapper packages.

------------------------------------------------------------------------

### GRASS GIS

GRASS GIS imposes the most stringent requirements. In addition to
numerous environment variables, it requires a correctly defined spatial
database, including projection, extent, and resolution.

The
[`linkGRASS()`](https://r-spatial.github.io/link2GI/reference/linkGRASS.md)
function searches for available GRASS installations, optionally allows
interactive version selection, and initializes a valid GRASS session.
Once initialized, users can access both the **rgrass** interface and the
native GRASS command-line API within the same R session.

------------------------------------------------------------------------

### SAGA GIS

SAGA GIS is comparatively straightforward to configure. The
[`linkSAGA()`](https://r-spatial.github.io/link2GI/reference/linkSAGA.md)
function locates available installations, sets the required paths and
environment variables, and returns a configuration object that can be
passed to wrapper packages such as **RSAGA**. Alternatively, users may
directly call the `saga_cmd` command-line interface via R’s
[`system()`](https://rdrr.io/r/base/system.html) or
[`system2()`](https://rdrr.io/r/base/system2.html) functions.

------------------------------------------------------------------------

### Orfeo Toolbox (OTB)

The Orfeo Toolbox is a powerful framework for remote-sensing tasks such
as classification, filtering, and machine-learning-based image analysis.
While selected OTB algorithms are accessible through R packages, these
interfaces are often limited in scope or not designed for large-scale or
production workflows.

*link2GI* identifies available OTB installations and exposes their
command-line interfaces in a structured and reproducible manner. In the
absence of a comprehensive R wrapper, the package provides a lightweight
introspection mechanism based on OTB’s CLI help system, enabling
scriptable and reproducible execution of OTB modules from R.

------------------------------------------------------------------------

### GDAL

Although GDAL functionality is widely available through R packages,
direct access to GDAL command-line utilities remains important in
several scenarios—particularly following the retirement of `rgdal` and
related packages.

Differences between GDAL, PROJ, and system libraries can make it
difficult to determine which capabilities are actually available on a
given machine. *link2GI* therefore provides explicit discovery and
controlled access to **system-linked GDAL binaries**, allowing users to
run GDAL utilities deterministically from R, independent of how GDAL is
compiled into other packages.

This approach supports reproducibility, transparent diagnostics, and
batch-oriented workflows while deliberately avoiding duplication of
existing GDAL-based R functionality.
