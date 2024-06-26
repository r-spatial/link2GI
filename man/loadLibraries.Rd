% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initProj.R
\name{loadLibraries}
\alias{loadLibraries}
\title{Load libraries and try to install missing ones}
\usage{
loadLibraries(libs)
}
\arguments{
\item{libs}{vector with the  names of libraries}
}
\value{
List indicating which library has been loaded successfully.
}
\description{
Load libaries in the R environment and try to install misssing
ones.
}
\examples{
\dontrun{
# loadLibraries(libs = C('link2GI'))
}
}
\keyword{internal}
