# Link GI to R

## What is link2GI?

The [link2GI](https://CRAN.R-project.org/package=link2GI) package
provides a small linking tool to simplify the use of `GRASS GIS`,
`SAGA GIS`, `Orfeo Toolbox` (`OTB`) and `GDAL` binaries for R users,
with the focus on making this software accessible to non-operating
system specialists or highly experienced GIS geeks. In fact, it is a
direct result of numerous graduate courses with R(-GIS) novices in the
hostile world of university computer pools running on extremely limited
Windows systems.

## Why link2GI now?

R has a lot of classes for storing and manipulating spatial data. For
vector data, the [sp](https://CRAN.R-project.org/package=sp) and
currently the great [sf](https://CRAN.R-project.org/package=sf) packages
are well known, and the raster data world is largely covered by the
[terra](https://CRAN.R-project.org/package=terra) and recently the
[stars](https://CRAN.R-project.org/package=stars) packages. For more
specific links, such as those needed for manipulating atmospheric
models, packages like [ncdf4](https://CRAN.R-project.org/package=ncdf4)
are very helpful.

The spatial analysis itself is often supported by wrapper packages that
integrate external libraries, command line tools, or a mixture of both
in an R-like syntax
[geosphere](https://CRAN.R-project.org/package=geosphere),
[Distance](https://CRAN.R-project.org/package=Distance),
[igraph](https://CRAN.R-project.org/package=igraph), or
[spatstat](https://CRAN.R-project.org/package=spatstat).

A comprehensive introduction to the spatial R-biotope and its
backgrounds is excellently treated in [Geocomputation with
R](https://r.geocompx.org/), which is not only highly recommended as a
reference book, but is also an indispensable basis for working and
analyzing spatial data with R/Python.

Despite all these spatial analysis and data handling capabilities in the
`R` world, it can be said (at least from a non-R point of view) that
there is still a huge gap between R and the mature open source
Geographic Information System (GIS) and even more so the Remote Sensing
(RS) software community. QGIS, GRASS GIS, and SAGA GIS provide an
extensive, growing, and mature collection of sophisticated algorithms.
The algorithms provided are fast, stable and most of them are well
proven. Probably most `R` users who are somehow connected to the GI
community know that there are great wrapper packages to bridge this gap.
For [GRASS GIS 7/8](https://grass.osgeo.org/) it is
[rgrass](https://CRAN.R-project.org/package=rgrass) and for [SAGA
GIS](https://saga-gis.sourceforge.io/) there are
[RSAGA](https://CRAN.R-project.org/package=RSAGA) and
[Rsagacmd](https://github.com/stevenpawley/Rsagacmd). There is no mature
wrapper package for the \[Orfeo Toolbox\]
(<https://gitlab.orfeo-toolbox.org/orfeotoolbox/otb>), which is
indispensable for remote sensing. This package not only creates the API
link, but also provides a lightweight wrapper for easy use of `OTB`
modules from `R`.

Unfortunately you will run into a lot of technical problems depending on
the chosen operating system (OS) or library dependencies or GIS software
versions. In the case of `RSAGA` for example, the main problem was that
the `SAGA` GIS developers not only change the syntax and strategy of the
command line interface (CLI), but also within the same release the calls
differ from OS to OS. So the maintenance of RSAGA is at least tedious
(but thumbs up again). Another example is `GRASS GIS`, which is known
for a sophisticated setup of the environment and the spatial properties
of the database. If you “only” want to use a specific `GRASS` algorithm
from R, you will probably get lost in setting up all the OS dependencies
that are necessary to set up a correct temporary or permanent `GRASS`
environment from “outside”. This is not only due to the strict space and
projection requirements of `GRASS`, but much more due to the demanding
OS environments, especially Windows.

To cut a long story short, it is a bit cumbersome to deal with all this
stuff when you just want to start `GRASS` from the R command line,
e.g. for a powerful random walk cost analysis (`r.walk`) call as
provided by `GRASS`.

## What does linking mean?

Linking simply means providing all the necessary environment variables
to allow full access to the command line APIs of the software tools
mentioned. `link2GI` also tries to be compliant with the existing
wrapper packages and integrate their use. `link2GI` tries to analyse
which software is installed in which version in order to create a
temporary or permanent environment that meets the requirements mentioned
above.

#### GRASS GIS

GRASS GIS has the most demanding requirements. It needs a lot of
environment and path variables as **and** a correct setup of the
geographic data parameters. The `linkGRASS` function tries to find all
installations and lets you (optionally) choose the one you want to use
and generate the necessary variables. As a result you can use both the
`rgrass` package and the command line `API` of `GRASS`.

#### SAGA GIS

SAGA GIS is much easier to set up. Again, the `linkSAGA` function will
try to find all `SAGA` installations, let you (optionally) choose one,
and generate the necessary variables. You can also use `RSAGA`, but you
have to pass the result of `linkSAGA` like
`RSAGA::rsaga.env(path = saga$sagaPath)`. For easy use you can just use
the `R` system() call to interface `R` with the `saga_cmd` API.

#### Orfeo Toolbox (OTB)

The `Orfeo Toolbox` (OTB) is a very powerful remote sensing toolbox. It
is widely used for classification, filtering and machine learning
applications. You will find some of the implemented algorithms in
various R packages, but **always** much slower or only running on small
data chunks. `link2GI` searches and links all `OTB` installations of a
given search path and returns the result as a concise list. Due to a
missing wrapper package, a list-based `OTB` module and function parser
is also available, which can be piped into the `runOTB` function for a
convenient function call.

#### GDAL

Although `GDAL` is well integrated into the R packages, in quite a few
cases it is advantageous to use system calls and to obtain the binaries
directly, especially after the discontinuation of `rgdal` and some other
convenience packages. In particular, the development towards `GDAL 3.x`
and optionally different boxed versions of `GDAL` binaries that work
with different `Python` and `proj4/proj6` libraries make it often
difficult to find the right `GDAL` version. `link2GI` generates a list
of all paths and commands of all `GDAL` installations in the given
search path. With this list, you can easily use all available API calls
of each installation.
