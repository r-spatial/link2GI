% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initProj.R
\name{sourceFunctions}
\alias{sourceFunctions}
\title{Source functions from standard or given directory}
\usage{
sourceFunctions(fcts_folder, setup_script)
}
\arguments{
\item{fcts_folder}{path of the folder holding the functions. All files in
this folder will be sourced at project start.}
}
\value{
Information if sourcing was successfull based on try function.
}
\description{
Source functions into the R environment located in a specified
folder.
}
\examples{
\dontrun{
# sourceFunctions(fcts_folder = '~/project/src/fcts')
}
}
\keyword{internal}
