% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/File.R
\name{listDatasets}
\alias{listDatasets}
\title{Search inner file hierarchy for datasets}
\usage{
listDatasets(file)
}
\arguments{
\item{file}{an object of type HSDSFile to be searched}
}
\value{
a list of inner-paths
}
\description{
The datasets in an HDF5 file are organized internally by groups.
This routine traverses the internal group hiearchy, locates
all datasets and prints a list of them. Note that if the 
file's group hiearchy is complex, this could be time-consuming.
}
\details{
This function is deprecated and will be defunct in the next release.
}
\examples{
src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
f <- HSDSFile(src, '/home/spollack/testzero.h5')
listDatasets(f)
}
