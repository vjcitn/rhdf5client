% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h5serv.R
\name{getDatasetUUIDs}
\alias{getDatasetUUIDs}
\title{getDatasetUUIDs from hsds server}
\usage{
getDatasetUUIDs(object)
}
\arguments{
\item{object}{instance of H5S_source(updated object with path to file set)}
}
\value{
character of dataset uuid obtained
}
\description{
This function is deprecated and will be defunct in the next release.
}
\examples{
\dontrun{
hsdsCon = H5S_source(URL_hsds()) # hsds server
setPath(hsdsCon, "/home/stvjc/hdf5_mat.h5")-> hsds
getDatasetUUIDs(hsds)
}
}
