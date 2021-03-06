% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HSDS_Matrix.R
\name{HSDS_Matrix}
\alias{HSDS_Matrix}
\title{simplify construction of DelayedMatrix from url and path in HSDS}
\usage{
HSDS_Matrix(url, path, title)
}
\arguments{
\item{url}{character(1) URL for HSDS object store with port}

\item{path}{character(1) path from root defining HDF Cloud resource}

\item{title}{character(1) name of dataset to use}
}
\value{
instance of DelayedArray
}
\description{
This class is deprecated and will be defunct in the next release.
}
\examples{
\dontrun{
HSDS_Matrix(URL_hsds(), "/shared/bioconductor/darmgcls.h5")
}
}
